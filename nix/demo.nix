# Demonstration of how to import a derivation of the purescript-protobuf
# compiler plugin protoc-gen-purescript.
{ pkgs ? import ./pkgs.nix {} }:
let
  purescript-protobuf = builtins.fetchGit {
    url = "git@github.com:xc-jp/purescript-protobuf.git";
    ref = "master";
    rev = "23d2537c239450aa670c53edb491d46cd18c4422";
  };
  protoc-gen-purescript = import "${purescript-protobuf}/default.nix" {};
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    nodejs-13_x
    protobuf3_9
    protoc-gen-purescript
  ];
}
