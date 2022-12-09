{
  description = "PureScript Protobuf";

  # for spago2nix
  nixConfig.sandbox = "relaxed";

  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    spago2nix = {
      url = "github:justinwoo/spago2nix";
      flake = false;
    };
    # flake-compat = {
    #   url = "github:edolstra/flake-compat";
    #   flake = false;
    # };
  };

  outputs = { self, ... }@inputs:
    inputs.flake-utils.lib.eachSystem ["x86_64-linux"] (system:
    let

      nixpkgs = inputs.nixpkgs.legacyPackages.${system};
      easy-purescript-nix = import inputs.easy-purescript-nix {pkgs = nixpkgs;};
      protobuf = (import ./nix/protobuf.nix {pkgs = nixpkgs;}).protobuf_v21_10;
      nix-prefetch-git-patched = import ./nix/nix-prefetch-git-patched.nix nixpkgs;
      spago2nix = import inputs.spago2nix {
        pkgs = nixpkgs // {
          nix-prefetch-git = nix-prefetch-git-patched;
        };
      };

      purs = easy-purescript-nix.purs-0_15_4;
      nodejs = nixpkgs.nodejs-18_x;

      spago-packages-nix = {
        spago-dhall ? "spago.dhall", # the main spago.dhall file name, i.e. "spago.dhall"
        srcs-dhall # array of .dhall files, i.e. [./spago.dhall ./packages.dhall]
      }:
        nixpkgs.stdenv.mkDerivation {

        # https://zimbatm.com/notes/nix-packaging-the-heretic-way
        # So that spago2nix can fetch packages from Github.
        __noChroot = true;

        # We need HTTPS to fetch from github.
        SYSTEM_CERTIFICATE_PATH = "${nixpkgs.cacert}/etc/ssl/certs";
        SSL_CERT_FILE = "${nixpkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        NIX_SSL_CERT_FILE = "${nixpkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        GIT_SSL_CAINFO = "${nixpkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

        name = "spago-packages";
        nativeBuildInputs = [
          spago2nix
          easy-purescript-nix.spago
        ];
        srcs = srcs-dhall;
        unpackPhase = ''
          for _src in $srcs; do
            cp "$_src" $(stripHash "$_src")
          done
        '';
        buildPhase = ''
          spago2nix generate 4 -- --config ${spago-dhall} --global-cache skip
          '';
        installPhase = ''
          mkdir $out
          cp spago-packages.nix $out/
          '';
        };

			spago-packages-nix-plugin = spago-packages-nix {
        spago-dhall = "spago-plugin.dhall";
        srcs-dhall = [
          ./spago-plugin.dhall
          ./spago.dhall
          ./packages.dhall
        ];
      };
      # https://nixos.wiki/wiki/Import_From_Derivation
      spago-packages-plugin = import "${spago-packages-nix-plugin}/spago-packages.nix" {pkgs=nixpkgs;};

      protoc-gen-purescript = nixpkgs.stdenv.mkDerivation {
        name = "protoc-gen-purescript";
        buildInputs = [
          spago-packages-plugin.installSpagoStyle
          spago-packages-plugin.buildSpagoStyle
        ];
        nativeBuildInputs = [
          nodejs
          purs
        ];
        src = nixpkgs.nix-gitignore.gitignoreSource [ ".git" ] ./.;
        unpackPhase = ''
          cp -r $src/src .
          cp -r $src/plugin .
          '';
        buildPhase = ''
          install-spago-style
          build-spago-style "./src/**/*.purs" "./plugin/**/*.purs"
          '';
        installPhase = ''
           mkdir -p $out/bin
           mv output $out/
           echo "#!/usr/bin/env bash" >> $out/bin/protoc-gen-purescript
           echo "${nodejs}/bin/node --input-type=module -e \"import {main} from '$out/output/ProtocPlugin.Main/index.js'; main();\"" >> $out/bin/protoc-gen-purescript
           chmod +x $out/bin/protoc-gen-purescript
           '';
      };

			spago-packages-nix-conformance = spago-packages-nix {
        spago-dhall = "spago-conformance.dhall";
        srcs-dhall = [
          ./spago-conformance.dhall
          ./spago.dhall
          ./packages.dhall
        ];
      };
      # https://nixos.wiki/wiki/Import_From_Derivation
      spago-packages-conformance = import "${spago-packages-nix-conformance}/spago-packages.nix" {pkgs=nixpkgs;};
      conformance-purescript = nixpkgs.stdenv.mkDerivation {
        name = "conformance-purescript";
        buildInputs = [
          spago-packages-conformance.installSpagoStyle
          spago-packages-conformance.buildSpagoStyle
        ];
        nativeBuildInputs = [
          nodejs
          purs
          protoc-gen-purescript
          protobuf
        ];
        src = nixpkgs.nix-gitignore.gitignoreSource [ ".git" ] ./.;
        unpackPhase = ''
          cp -r $src/src .
          cp -r $src/conformance .
          '';
        buildPhase = ''
          install-spago-style
          mkdir generated
          protoc --purescript_out=./generated --proto_path=${protobuf}/src --proto_path=${protobuf}/conformance ${protobuf}/conformance/conformance.proto
          protoc --purescript_out=./generated --proto_path=${protobuf}/src --proto_path=${protobuf}/conformance ${protobuf}/src/google/protobuf/test_messages_proto3.proto
          build-spago-style "./src/**/*.purs" "./conformance/**/*.purs" "./generated/**/*.purs"
          '';
        installPhase = ''
           mkdir -p $out/bin
           mv output $out/
           echo "#!/usr/bin/env bash" >> $out/bin/conformance-purescript
           echo "${nodejs}/bin/node --input-type=module --abort-on-uncaught-exception --trace-sigint --trace-uncaught --eval=\"import {main} from '$out/output/Conformance.Main/index.js'; main();\"" >> $out/bin/conformance-purescript
           chmod +x $out/bin/conformance-purescript
           '';
      };

    in {
      devShells.default = nixpkgs.mkShell {
        nativeBuildInputs = [
          purs
          nodejs
          easy-purescript-nix.spago
          easy-purescript-nix.pulp
          protobuf
          nixpkgs.nodePackages.bower
          easy-purescript-nix.psc-package
          nixpkgs.dhall
          nixpkgs.dhall-json
          protoc-gen-purescript
        ];
        shellHook = ''
        # shopt -s globstar # Need for globbing packagePath in vscode PureScript IDE
        source <(spago --bash-completion-script `which spago`)
        source <(node --completion-bash)
        echo "PureScript Protobuf development environment"
        protoc --version
        echo -n "purs "
        purs --version
        echo -n "node "
        node --version
        echo ""
        echo "To build the protoc compiler plugin, run:"
        echo ""
        echo "    spago -x spago-plugin.dhall build"
        echo ""
        echo "To compile PureScript .purs files from .proto files, run for example:"
        echo ""
        echo "    protoc --purescript_out=. google/protobuf/timestamp.proto"
        echo ""
        '';
        LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
      };
      packages = {
        inherit protoc-gen-purescript;
        inherit protobuf;
        inherit conformance-purescript;
      };
      apps = {
        conformance =
          let
            conformance-run = nixpkgs.writeScriptBin "conformance" ''
              set -e
              set -x
              ${protobuf}/bin/conformance-test-runner --enforce_recommended ${conformance-purescript}/bin/conformance-purescript
              '';
          in
          {
            type = "app";
            program = "${conformance-run}/bin/conformance";
          };
      };
    }
    );
}
