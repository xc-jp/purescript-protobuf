{
  description = "PureScript Protobuf";

  # for spago2nix
  nixConfig.sandbox = "relaxed";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      # inputs.nixpkgs.follow = "nixpkgs";
    };
    spago2nix = {
      url = "github:justinwoo/spago2nix";
    };
  };

  outputs = { self, ... }@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
    let

      nixpkgs = inputs.nixpkgs.legacyPackages.${system};
      easy-purescript-nix = import inputs.easy-purescript-nix {pkgs = nixpkgs;};
      protobufs = (import ./nix/protobuf.nix {pkgs = nixpkgs;});
      protobuf = protobufs.protobuf_v23_2;

      purs = easy-purescript-nix.purs-0_15_8;
      nodejs = nixpkgs.nodejs-18_x;

      protoc-gen-purescript = nixpkgs.stdenv.mkDerivation {
        name = "protoc-gen-purescript";
        nativeBuildInputs = [
          nodejs
          purs
        ] ++ (
          inputs.spago2nix.packages.${system}.spago2nix_nativeBuildInputs {
            spago-dhall = "spago-plugin.dhall";
            srcs-dhall = [
              ./spago-plugin.dhall
              ./spago.dhall
              ./packages.dhall
            ];
        });
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

      conformance-purescript = nixpkgs.stdenv.mkDerivation {
        name = "conformance-purescript";
        nativeBuildInputs = [
          nodejs
          purs
          protoc-gen-purescript
          protobuf
        ] ++ (
          inputs.spago2nix.packages.${system}.spago2nix_nativeBuildInputs {
            spago-dhall = "spago-conformance.dhall";
            srcs-dhall = [
              ./spago-conformance.dhall
              ./spago.dhall
              ./packages.dhall
            ];
        });
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
        source <(spago --bash-completion-script `which spago`)
        source <(node --completion-bash)
        export PURS_IDE_SOURCES=$(${easy-purescript-nix.spago}/bin/spago -x spago-dev.dhall sources)
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
        # https://github.com/nwolverson/vscode-ide-purescript/issues/104
        # https://github.com/nwolverson/purescript-language-server/pull/75
        # https://github.com/nwolverson/purescript-language-server/issues/175
        # PURS_IDE_SOURCES = "'src/**/*.purs plugin/**/*.purs test/**/*.purs conformance/**/*.purs .spago/**/*.purs'";
      };
      packages = {
        inherit protoc-gen-purescript;
        inherit protobuf;
        inherit conformance-purescript;
        inherit (protobufs) protobuf_v21_10;
        inherit (protobufs) protobuf_v23_2;
      };
      apps = {
        protoc = {
          type = "app";
          program = "${protobuf}/bin/protoc";
        };
        protoc-gen-purescript = {
          type = "app";
          program = "${protoc-gen-purescript}/bin/protoc-gen-purescript";
        };
        conformance_test_runner = {
          type = "app";
          program = "${protobuf}/bin/conformance_test_runner";
        };
        conformance =
          let
            conformance-run = nixpkgs.writeScriptBin "conformance" ''
              set -e
              set -x
              ${protobuf}/bin/conformance_test_runner --enforce_recommended ${conformance-purescript}/bin/conformance-purescript
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
