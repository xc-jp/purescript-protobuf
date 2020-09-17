{ pkgs ? import ./nix/pkgs.nix { } }:
let
  protobuf = import ./nix/protobuf.nix { inherit pkgs; };
  # protoc = protobuf.protoc;
  # conformance = protobuf.conformance;
in
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.easy-ps.purs-0_13_8
    pkgs.easy-ps.spago
    pkgs.nodejs-13_x
    # pkgs.easy-ps.pulp
    # pkgs.protobuf3_9
    # pkgs.nodePackages.bower
    # pkgs.easy-ps.psc-package
    # pkgs.dhall
    # pkgs.dhall-json
    # conformance.conformance
    protobuf
  ];
  shellHook = ''
    export PATH="./bin:$PATH"   # PATH to protoc-gen-purescript
    npm install
    spago build
    protoc --purescript_out=./conformance/generated --proto_path=${protobuf}/src --proto_path=${protobuf}/conformance ${protobuf}/src/google/protobuf/test_messages_proto3.proto"





  export PATH="./bin:$PATH"   # PATH to protoc-gen-purescript
  echo "Purescript Protobuf development environment."
  echo "To build purescript-protobuf, run:"
  echo ""
  echo "    npm install"
  echo "    spago build"
  echo ""
  echo "To test purescript-protobuf, run:"
  echo ""
  echo "    protoc --purescript_out=./test/generated test/*.proto"
  echo "    spago -x test.dhall build"
  echo "    spago -x test.dhall test"
  echo ""
  echo "To generate Purescript .purs files from .proto files, run:"
  echo ""
  echo "    protoc --purescript_out=path_to_output *.proto"
  echo ""
  echo "To run the Google conformance test on purescript-protobuf, run:"
  echo ""
  echo "    protoc --purescript_out=./generate-conformance --proto_path=conformance/result/src/  --proto_path=conformance/result/conformance/ ./conformance/result/src/google/protobuf/test_messages_proto3.proto"
  echo "    spago -x conformance.dhall build"
  echo "    spago -x conformance.dhall build"
  '';
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
}
