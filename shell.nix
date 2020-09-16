{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "1ec689df0adf8e8ada7fcfcb513876307ea34226";
      sha256 = "12hk2zbjkrq2i5fs6xb3x254lnhm9fzkcxph0a7ngxyzfykvf4hi";
    }) {
    inherit pkgs;
  };
  conformance = import ./conformance/protobuf.nix { inherit pkgs; };
in
pkgs.mkShell {
  nativeBuildInputs = [
    easy-ps.purs-0_13_8
    easy-ps.spago
    pkgs.nodejs-14_x
    easy-ps.pulp
    pkgs.protobuf3_9
    pkgs.nodePackages.bower
    easy-ps.psc-package
    pkgs.dhall
    pkgs.dhall-json
    conformance.conformance
  ];
  shellHook = ''
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
