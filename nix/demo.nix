# Demonstration of how to import a derivation of the purescript-protobuf
# compiler plugin protoc-gen-purescript.
{ pkgs ? import ./pkgs.nix {} }:
let
  protoc-gen-purescript = import ../default.nix { inherit pkgs; };
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    nodejs
    protobuf
    protoc-gen-purescript
  ];
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
}
