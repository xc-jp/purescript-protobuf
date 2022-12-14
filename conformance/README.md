# Protocol Buffer Conformance Testing

## Running the conformance test

From the top directory of this repo,

```
nix run .#conformance
```

## About the conformance test runner

[Conformance README](https://github.com/protocolbuffers/protobuf/tree/master/conformance)

The derivations in `nix/protobuf.nix` will build `protoc` and the
`conformance-test-runner`.

## Dev

To generate the conformance `.purs` in the dev environment:

```
protoc --purescript_out=./conformance/generated --proto_path=$(nix path-info .#protobuf)/conformance $(nix path-info .#protobuf)/conformance/conformance.proto
protoc --purescript_out=./conformance/generated --proto_path=$(nix path-info .#protobuf)/src $(nix path-info .#protobuf)/src/google/protobuf/test_messages_proto3.proto
```