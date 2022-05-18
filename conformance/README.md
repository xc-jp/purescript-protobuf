# Protocol Buffer Conformance Testing

## Running the conformance test

From the top directory of this repo,

```
nix-shell nix/conformance.nix
run
```

## About the conformance test runner

[Conformance README](https://github.com/protocolbuffers/protobuf/tree/master/conformance)

The derivations in `nix/protobuf.nix` will build `protobuf` and the
`conformance-test-runner`.
