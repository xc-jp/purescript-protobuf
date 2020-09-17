# Protocol Buffer Conformance Testing

[Conformance README](https://github.com/protocolbuffers/protobuf/tree/master/conformance)

The `nix/protobuf.nix` derivation will build protobuf along with its
conformance test runners.

## Running the conformance test

From the top directory of this repo, run

```
nix-shell --command 'npm install'
nix-shell nix/conformance.nix
```
