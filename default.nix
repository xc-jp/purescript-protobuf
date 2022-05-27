# Derivation which provides `protoc-gen-purescript` on the PATH.
#
# To try it out, `nix-shell ./nix/demo.nix`, then
#
#     protoc --purescript_out=path_to_output file.proto
#
{ pkgs ? import ./nix/pkgs.nix {} }:

let
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
    install-spago-style
    '';
  buildPhase = ''
    build-spago-style "./src/**/*.purs" "./plugin/**/*.purs"
    '';
  installPhase = ''
    mkdir -p $out/bin
    mv output $out/
    echo "node --input-type=module -e \"import {main} from '$out/output/ProtocPlugin.Main/index.js'; main();\"" >> $out/bin/protoc-gen-purescript
    chmod +x $out/bin/protoc-gen-purescript
    '';
  LC_ALL = "C.UTF-8";
}
