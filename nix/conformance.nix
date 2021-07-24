{ pkgs ? import ./pkgs.nix { } }:

pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.purs
    pkgs.easy-ps.spago
    pkgs.nodejs
    pkgs.protobuf
    # pkgs.writeScriptBin "run" ''
    #   set -e
    #   spago build
    #   protoc --purescript_out=./conformance/generated --proto_path=${pkgs.protobuf}/src --proto_path=${pkgs.protobuf}/conformance ${pkgs.protobuf}/conformance/conformance.proto
    #   protoc --purescript_out=./conformance/generated --proto_path=${pkgs.protobuf}/src --proto_path=${pkgs.protobuf}/conformance ${pkgs.protobuf}/src/google/protobuf/test_messages_proto3.proto
    #   spago -x conformance.dhall build
    #   conformance-test-runner --enforce_recommended bin/conformance-purescript
    # ''
  ];
  shellHook = ''
    protoc --version
    echo -n "purs "
    purs --version
    export PATH="./bin:$PATH"   # PATH to protoc-gen-purescript
    # echo "Run conformance test:"
    # echo ""
    # echo "    run"
    # echo ""
    # set -e
    spago -x spago-protoc.dhall build
    protoc --purescript_out=./conformance/generated --proto_path=${pkgs.protobuf}/src --proto_path=${pkgs.protobuf}/conformance ${pkgs.protobuf}/conformance/conformance.proto
    protoc --purescript_out=./conformance/generated --proto_path=${pkgs.protobuf}/src --proto_path=${pkgs.protobuf}/conformance ${pkgs.protobuf}/src/google/protobuf/test_messages_proto3.proto
    spago -x spago-conformance.dhall build
    conformance-test-runner --enforce_recommended bin/conformance-purescript
  '';
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
}
