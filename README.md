# WIP purescript-protobuf

Purescript code generation and runtime library for Google Protocol Buffers.

Only
[Protocol Buffers Version 3](https://developers.google.com/protocol-buffers/docs/reference/proto3-spec)
supported.

## Runtime library

This library operates on
[`ArrayBuffer`](https://pursuit.purescript.org/packages/purescript-arraybuffer-types/docs/Data.ArrayBuffer.Types#t:ArrayBuffer), so works in both 
[Node](https://pursuit.purescript.org/packages/purescript-node-buffer/docs/Node.Buffer.Class)
and browser runtime environments.

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
