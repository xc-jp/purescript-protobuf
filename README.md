# WIP purescript-protobuf

Purescript library and code generator for Google Protocol Buffers.

Only
[Protocol Buffers Version 3](https://developers.google.com/protocol-buffers/docs/reference/proto3-spec)
is supported.

This library operates on
[`ArrayBuffer`](https://pursuit.purescript.org/packages/purescript-arraybuffer-types/docs/Data.ArrayBuffer.Types#t:ArrayBuffer), so it will run both
[in Node](https://pursuit.purescript.org/packages/purescript-node-buffer/docs/Node.Buffer.Class)
and in browser environments.

## Imports

None of the modules in this package should be imported directly in your program.
Rather, you'll import the message modules in the generated `.purs` files,
as well as modules for reading and writing `ArrayBuffer`s.

```purescript
import GeneratedMessages (MyMessage, parseMyMessage, putMyMessage)
import Text.Parsing.Parser (runParserT)
import Data.ArrayBuffer.Builder (execPut)
```

The generated message modules will import the `Protobuf.Runtime` module.

## Dependencies

You'll need these packages for reading and writing `ArrayBuffer`s.

* [__Data.ArrayBuffer.Builder__](http://pursuit.purescript.org/packages/purescript-arraybuffer-builder/)
* [__Text.Parsing.DataView__](http://pursuit.purescript.org/packages/purescript-parsing-dataview/)

## Code Generation

The `shell.nix` environment provides

* The Purescript toolchain
* The [`protoc`](https://github.com/protocolbuffers/protobuf/blob/master/src/README.md) compiler
* The `protoc-gen-purescript` executable plugin for `protoc` on the `PATH` so that
  [`protoc` can find it](https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.compiler.plugin).

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
