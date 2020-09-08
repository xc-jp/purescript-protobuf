# WIP purescript-protobuf

Purescript library and code generator for Google Protocol Buffers.

Only
[Protocol Buffers Version 3](https://developers.google.com/protocol-buffers/docs/reference/proto3-spec)
supported.

## Imports

This library operates on
[`ArrayBuffer`](https://pursuit.purescript.org/packages/purescript-arraybuffer-types/docs/Data.ArrayBuffer.Types#t:ArrayBuffer), so it will
[run in both Node](https://pursuit.purescript.org/packages/purescript-node-buffer/docs/Node.Buffer.Class)
and browser environments.

None of the modules in this package should be imported directly in your program.
Rather, you'll import the message modules in the generated `.purs` files,
as well as some standard dependencies for reading and writing `ArrayBuffer`s.
[__ArrayBuffer.Builder__](http://pursuit.purescript.org/packages/purescript-arraybuffer-builder/)
[__Parsing.DataView__](http://pursuit.purescript.org/packages/purescript-parsing-dataview/)

The generated message modules will import the `Protobuf.Runtime` module.

```purescript
import Text.Parsing.Parser (runParserT)
import Data.ArrayBuffer.Builder (execPut)
import GeneratedMessages (MyMessage, parseMyMessage, putMyMessage)
```

## Code Generation

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
