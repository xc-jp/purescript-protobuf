# WIP purescript-protobuf

Purescript code generation and runtime library for Google Protocol Buffers.

Only
[Protocol Buffers Version 3](https://developers.google.com/protocol-buffers/docs/reference/proto3-spec)
supported.

## Runtime library

This library operates on
[`ArrayBuffer`](https://pursuit.purescript.org/packages/purescript-arraybuffer-types/docs/Data.ArrayBuffer.Types#t:ArrayBuffer), so 
[works in both Node](https://pursuit.purescript.org/packages/purescript-node-buffer/docs/Node.Buffer.Class)
and browser runtime environments.

## Compile-time Code Generation

The `shell.nix` environment provides

* The Purescript toolchain
* The `protoc` compiler
* The `protoc` compiler plugin for Purescript. The `bin/protoc-gen-purescript`
  executable is on the `PATH`
  [so that `protoc` can find it](https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.compiler.plugin).

```
$ nix-shell

Purescript Protobuf development environment.
To build purescript-protobuf, run:

    npm install
    spago build

To test purescript-protobuf, run:

    spago test

To generate Purescript .purs files from .proto files, run:

    protoc --purescript_out=./test test/test.proto

[nix-shell]$
```
