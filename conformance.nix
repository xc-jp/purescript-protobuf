{ pkgs ? import ./nix/pkgs.nix { } }:
let
  protobuf = import ./nix/protobuf.nix { inherit pkgs; };
  # protoc = protobuf.protoc;
  # conformance = protobuf.conformance;
  # conformance-purescript = pkgs.writeScriptBin "conformance-purescript" ''
  #   exec node -e "require('./Conformance.Main/index.js').main()"
  # '';

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
    # conformance-test-runner --enforce_recommended ${conformance-purescript}
  # buildInputs = [ conformance-purescript ];
  shellHook = ''
    export PATH="./bin:$PATH"   # PATH to protoc-gen-purescript
    # export NODE_PATH="$PWD/output:$PWD/node_modules:$NODE_PATH"   # PATH to protoc-gen-purescript
    set -e
    npm install
    spago build
    protoc --purescript_out=./conformance/generated --proto_path=${protobuf}/src --proto_path=${protobuf}/conformance ${protobuf}/conformance/conformance.proto
    protoc --purescript_out=./conformance/generated --proto_path=${protobuf}/src --proto_path=${protobuf}/conformance ${protobuf}/src/google/protobuf/test_messages_proto3.proto
    spago -x conformance.dhall build
    # conformance-test-runner --enforce_recommended $(node -e "require('./output/Conformance.Main/index.js').main()")
    # conformance-test-runner --enforce_recommended node -e "require('./output/Conformance.Main/index.js').main()"
    conformance-test-runner --enforce_recommended bin/conformance-purescript
  '';
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
}
