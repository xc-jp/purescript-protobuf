{ pkgs ? import ./pkgs.nix { } }:
let
  protobuf = import ./protobuf.nix { inherit pkgs; };
in
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.easy-ps.purs-0_13_8
    pkgs.easy-ps.spago
    pkgs.nodejs-13_x
    protobuf
  ];
  shellHook = ''
    protoc --version
    echo -n "purs "
    purs --version
    export PATH="./bin:$PATH"   # PATH to protoc-gen-purescript
    set -e
    spago build
    protoc --purescript_out=./conformance/generated --proto_path=${protobuf}/src --proto_path=${protobuf}/conformance ${protobuf}/conformance/conformance.proto
    protoc --purescript_out=./conformance/generated --proto_path=${protobuf}/src --proto_path=${protobuf}/conformance ${protobuf}/src/google/protobuf/test_messages_proto3.proto
    spago -x conformance.dhall build
    conformance-test-runner --enforce_recommended bin/conformance-purescript
  '';
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
}
