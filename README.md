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

* [__arraybuffer-builder__](http://pursuit.purescript.org/packages/purescript-arraybuffer-builder/)
* [__parsing-dataview__](http://pursuit.purescript.org/packages/purescript-parsing-dataview/)

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

## Interpreting decoding errors

When
[`Text.Parsing.Parser.ParserT`](https://pursuit.purescript.org/packages/purescript-parsing/docs/Text.Parsing.Parser#t:ParserT)
fails at runtime it will return a `ParseError String (Position {line::Int,column::Int})`.
The byte offset at which the decoding error occured is given by the
formula `column - 1`.

## Features

We only support __proto3__ so that means we don't support
[extensions](https://developers.google.com/protocol-buffers/docs/proto?hl=en#extensions).

The generated record fields will use `Nothing` instead of the 
[default values](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#default).

We support
[enumerations](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#enum).

We do not preserve
[unknown fields](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#unknowns).

We do not support the
[Any message type](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#any).

We do not support
[`oneof`](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#oneof).
The fields in a `oneof` will all be added to the message.

We do not support
[maps](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#maps).

We support
[packages](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#packages).

We do not support
[services](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#services).

We do not support any
[options](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#options).

## Contributing

Pull requests are welcome.
