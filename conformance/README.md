# Protocol Buffer Conformance Testing

## Running the conformance test

From the top directory of this repo, run

```
nix-shell --command 'npm install'
nix-shell nix/conformance.nix
```

## About the conformance test runner

[Conformance README](https://github.com/protocolbuffers/protobuf/tree/master/conformance)

The `nix/protobuf.nix` derivation will build protobuf along with its
conformance test runners. There are way too many fnords in the conformance
test build system. Whoever wrote the conformance test code must have
immanentized the eschaton.
