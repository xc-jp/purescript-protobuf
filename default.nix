# Derivation which provides `protoc-gen-purescript` on the PATH.
#
# To try it out, `nix-shell ./nix/demo.nix`, then
#
#     protoc --purescript_out=path_to_output file.proto
#
{ pkgs ? import ./nix/pkgs.nix {} }:

let
  node2NixFiles = srcPath:
    pkgs.stdenv.mkDerivation {
      name = "node2NixFiles";
      nativeBuildInputs = with pkgs; [ nodejs node2nix.package ];
      src = srcPath;
      unpackPhase = ''
      cp $src/package.json $src/package-lock.json .
      '';
      buildPhase = ''
        node2nix -l package-lock.json
      '';
      installPhase = ''
        mkdir $out
        cp node-env.nix node-packages.nix default.nix $out/
        cp package.json package-lock.json $out/
      '';
    };

  protobufNixNode = import (node2NixFiles ./.) { inherit pkgs; };
  spagoPkgs = import ./nix/spago-packages.nix { inherit pkgs; };
in

pkgs.stdenv.mkDerivation {
  name = "purescript-protobuf-generator";
  buildInputs = [
    spagoPkgs.installSpagoStyle
    spagoPkgs.buildSpagoStyle
    ];
  nativeBuildInputs = with pkgs; [
    purs
    easy-ps.spago
    ];
  src = ./.;
  unpackPhase = ''
    cp $src/spago.dhall .
    cp $src/packages.dhall .
    cp -r $src/src .
    cp -r $src/plugin .
    cp -r ${protobufNixNode.nodeDependencies}/lib/node_modules .
    install-spago-style
    '';
  buildPhase = ''
    build-spago-style "./src/**/*.purs" "./plugin/**/*.purs"
    '';
  installPhase = ''
    mkdir -p $out/bin
    mv output $out/
    cp -r node_modules $out/
    echo "node -e \"require('$out/output/ProtocPlugin.Main/index.js').main()\"" >> $out/bin/protoc-gen-purescript
    chmod +x $out/bin/protoc-gen-purescript
    '';
  LC_ALL = "C.UTF-8";
}
