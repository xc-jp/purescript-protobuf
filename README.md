# purescript-protobuf 💝

[![Test](https://github.com/xc-jp/purescript-protobuf/workflows/Test/badge.svg?branch=master)](https://github.com/xc-jp/purescript-protobuf/actions)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-protobuf/badge)](http://pursuit.purescript.org/packages/purescript-protobuf/)

Purescript library and code generator for
[Google Protocol Buffers version 3](https://developers.google.com/protocol-buffers/docs/proto3).

This library operates on
[`ArrayBuffer`](https://pursuit.purescript.org/packages/purescript-arraybuffer-types/docs/Data.ArrayBuffer.Types#t:ArrayBuffer), so it will run both
[in *Node.js*](https://pursuit.purescript.org/packages/purescript-node-buffer/docs/Node.Buffer.Class)
and in browser environments.

## Features

We aim to support binary-encoded (not JSON-encoded)
[__proto3__](https://developers.google.com/protocol-buffers/docs/proto3).
Many __proto2__-syntax descriptor files will
also work, as long as they don't use __proto2__ features, like
[groups](https://developers.google.com/protocol-buffers/docs/proto#groups).

The generated optional record fields will use `Nothing` instead of the
[default values](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#default).

We do not support
[extensions](https://developers.google.com/protocol-buffers/docs/proto?hl=en#extensions).

We do not support
[services](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#services).

### Conformance and Testing

At the time of this writing, we pass all 194 of the
[Google conformance tests](https://github.com/protocolbuffers/protobuf/tree/master/conformance)
for binary-wire-format proto3.

See the `conformance/README.md` in this repository for details.

We also have our own unit tests, see `test/README.md` in this repository.

## Code Generation

The `shell.nix` environment provides

* The Purescript toolchain: *purs*, *spago*, and *nodejs*.
* The [`protoc`](https://developers.google.com/protocol-buffers/docs/proto3?hl=en#generating) compiler
* The `protoc-gen-purescript` executable plugin for `protoc` on the `PATH` so that
  [`protoc` can find it](https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.compiler.plugin).

```
$ nix-shell

Purescript Protobuf development environment.
To build purescript-protobuf, run:

    npm install
    spago build

To generate Purescript .purs files from .proto files, run:

    protoc --purescript_out=path_to_output file.proto

[nix-shell]$
```

If you don't want to use Nix, then install the Purescript toolchain and *protoc*.

## Writing programs with the generated code

A message in a `shapes.proto` file declared as

```
package interproc;

message Rectangle {
  double width = 1;
  double height = 2;
}
```

will export these four names in a generated `shapes.Interproc.purs` file.

1. A message data type

   ```purescript
   newtype Rectangle = Rectangle { width :: Maybe Number, height :: Maybe Number }
   ```

2. A message maker which constructs a message from a `Record`
   with some message fields

   ```purescript
   mkRectangle :: forall r. Record r -> Rectangle
   ```

   All message fields are optional, and can be omitted from the `Record`. If we want the compiler to check that we've explicitly supplied all the fields,
   then we can use the ordinary message data type constructor.

3. A message encoder which works with
   [__purescript-arraybuffer-builder__](http://pursuit.purescript.org/packages/purescript-arraybuffer-builder/)

   ```purescript
   putRectangle :: forall m. MonadEffect m => Rectangle -> PutM m Unit
   ```

4. A message decoder which works with
   [__purescript-parsing-dataview__](http://pursuit.purescript.org/packages/purescript-parsing-dataview/)

   ```purescript
   parseMyMessage :: forall m. MonadEffect m => Int -> ParserT DataView m Rectangle
   ```

   The message decoder needs an argument which tells it the
   length of the message which it’s about to decode, because
   [“the Protocol Buffer wire format is not self-delimiting.”](https://developers.google.com/protocol-buffers/docs/techniques#streaming)

In our program, our imports will look something like this.
The only module from this package which we will import into our program
will be the `Protobuf.Library` module.
We'll import the message modules from the generated `.purs` files.
We'll also import modules for reading and writing `ArrayBuffer`s.


```purescript
import Protobuf.Library (Bytes(..), parseMaybe)
import Interproc.Shapes (Rectangle, mkRectangle, putRectangle, parseRectangle)
import Text.Parsing.Parser (runParserT)
import Data.ArrayBuffer.Builder (execPutM)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.Newtype (unwrap)
```

Serialize a `Rectangle` to an `ArrayBuffer`.

```purescript
do
    arraybuffer <- execPutM $ putRectangle $ mkRectangle
        { width: Just 3.0
        , height: Just 4.0
        }
```

Now we'll deserialize `Rectangle` from the `ArrayBuffer`.

```purescript
    result <- runParserT (whole arraybuffer) $ do
        rectangle <- parseRectangle (byteLength arraybuffer)
```

Now at this point, we've consumed all of the parser input, but
we're not finished parsing.

In [proto3, all fields are optional](https://github.com/protocolbuffers/protobuf/issues/2497).
We want to “validate” the `Rectangle` message to make sure it has all of the
fields that we require. Fortunately, we are already in the `ParserT` monad,
so we can do better than “validation.”
[Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/).

We will construct and return a tuple
with the width and height of the `Rectangle`. For this step,
[pattern matching](https://github.com/purescript/documentation/blob/master/language/Pattern-Matching.md)
on the `Rectangle` message type works well, or we might want to use some of the
convenience parsing functions supplied by `Protobuf.Library`, like `parseMaybe`.

```purescript
        width <- parseMaybe "Missing required width" (unwrap rectangle).width
        height <- parseMaybe "Missing required height" (unwrap rectangle).height
        pure $ Tuple width height
```

The `result` will now be `:: Either ParseError (Tuple Number Number)`.

### Dependencies

The generated code modules will import modules from this
package.

The generated code depends on packages

```
  , "protobuf"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arraybuffer-builder"
  , "parsing"
  , "parsing-dataview"
  , "uint"
  , "longs"
  , "text-encoding"
```

which are all in
[__package-sets__](https://github.com/purescript/package-sets).

It also depends on the Javascript package
[__long__](https://www.npmjs.com/package/long).

### Generated message instances

All of the generated message types have instances of
[`Eq`](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Eq#t:Eq),
[`Show`](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Show#t:Show),
[`Generic`](https://pursuit.purescript.org/packages/purescript-generics-rep/docs/Data.Generic.Rep#t:Generic),
[`NewType`](https://pursuit.purescript.org/packages/purescript-newtype/docs/Data.Newtype#t:Newtype).

### Examples

The __purescript-protobuf__ repository contains three executable *Node.js*
programs which use code generated by __purescript-protobuf__. Refer to these
for further examples of how to use the generated code.

1. The `protoc`
   [compiler plugin](https://github.com/xc-jp/purescript-protobuf/blob/master/src/ProtocPlugin/Main.purs).
   The code generator imports generated code. Trippy, right? This program
   literally writes itself.
2. The
   [unit test suite](https://github.com/xc-jp/purescript-protobuf/blob/master/test/Main.purs)
3. The Google
   [conformance test program](https://github.com/xc-jp/purescript-protobuf/blob/master/conformance/Main.purs)

### Interpreting invalid encoding parse failures

When the decode parser encounters an invalid encoding in the protobuf input
stream then it will fail to parse.

When
[`Text.Parsing.Parser.ParserT`](https://pursuit.purescript.org/packages/purescript-parsing/docs/Text.Parsing.Parser#t:ParserT)
fails it will return a `ParseError String (Position {line::Int,column::Int})`.

The byte offset at which the parse failure occured is given by the
formula `column - 1`.

The path to the protobuf definition which failed to parse will be included
in the `ParseError String` and delimited by `'/'`, something
like `"Message1 / string_field_1 / Invalid UTF8 encoding."`.

### Imports

The code generator will use the `package` statement in the `.proto` file
and the base file name as the Purescript module name for that file.

The Protobuf
[`import`](https://developers.google.com/protocol-buffers/docs/proto3#importing_definitions)
statement allows Protobuf messages to have fields
consisting of Protobuf messages imported from another file, and qualified
by the package name in that file. In order to generate
the correct Purescript module name qualifier on the types of imported message
fields, the code generator must be able to lookup the package name
statement in the imported file.

For that reason, we can only use top-level
(not [nested](https://developers.google.com/protocol-buffers/docs/proto3#nested))
`message` and `enum` types from an `import`.

The generated Purescript code will usually have module imports which cause
the `purs` compiler to emit warnings. Sorry.

## Nix derivation

If we want to run the `.proto` → `.purs` generation step as part of a pure Nix
derivation, then `import` the top-level `default.nix` from this repository
as a `nativeBuildInput`.

Then `protoc --purescript_out=path_to_output file.proto` will be runnable
in our derivation phases.

See the `nix/demo.nix` file for an example.

## Performance

The implementation is simple and straightforward. We haven't done
any special optimizations. For example, when encoding a protobuf varint, we
allocate a list of new one-byte `ArrayBuffers`s and then copy them all into
position in the final `ArrayBuffer`. For another example, when decoding a
packed field of numbers, we build a list of the numbers, and then copy them
all into the final `Array`. Also, this whole library is very stack-unsafe.
This may all be improved in later versions.

## Contributing

Pull requests welcome.
