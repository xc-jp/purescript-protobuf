# WIP purescript-protobuf

Purescript support for Google Protocol Buffers.

Only
[Protocol Buffers Version 3](https://developers.google.com/protocol-buffers/docs/reference/proto3-spec)
supported.

Works with both Node and browser?

## Compile-time Code Generation

The `shell.nix` environment provides
* The Purescript toolchain
* The `protoc` compiler
* The `bin/protoc-gen-purescript` executable on the `PATH`
  [so that `protoc` can find it](https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.compiler.plugin).

Enter the Nix shell and run `protoc` with the directory `OUT_DIR` name
for the generated `.purs` files, and the `$PROTO_FILES` list of input
`.proto` files.

```
nix-shell
protoc --purescript_out=$OUT_DIR $PROTO_FILES
```
