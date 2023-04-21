# purescript-protobuf 💝

[![Pursuit](http://pursuit.purescript.org/packages/purescript-protobuf/badge)](http://pursuit.purescript.org/packages/purescript-protobuf/)

PureScript library and code generator for
[Google Protocol Buffers version 3](https://protobuf.dev/programming-guides/proto3/).

This library operates on
[`ArrayBuffer`](https://pursuit.purescript.org/packages/purescript-arraybuffer-types/docs/Data.ArrayBuffer.Types#t:ArrayBuffer), so it will run both
[in *Node.js*](https://pursuit.purescript.org/packages/purescript-node-buffer/docs/Node.Buffer.Class)
and in browser environments.

## Features

We aim to support binary-encoded protobuf for
`syntax = "proto3";` descriptor files.

Many `syntax = "proto2";` descriptor files will
also work, as long as they don't use `"proto2"` features, especially
[groups](https://protobuf.dev/programming-guides/proto/#groups),
which we do not support. We also do not support `"proto2"`
[extensions](https://protobuf.dev/programming-guides/proto/#extensions).

We do not support
[services](https://protobuf.dev/programming-guides/proto3/#services).

We do not support
[JSON Mapping](https://protobuf.dev/programming-guides/proto3/#json).

### Conformance and Testing

In this version, we pass all 651 of the
[Google conformance tests](https://github.com/protocolbuffers/protobuf/tree/main/conformance)
of binary-wire-format *proto3* for [Protocol Buffers v21.10](https://github.com/protocolbuffers/protobuf/releases/tag/v21.10).
See the `conformance/README.md` in this repository for details.

We also have our own unit tests, see `test/README.md` in this repository.

## Code Generation

The `nix develop` environment provides

* The PureScript toolchain: `purs`, `spago`, and `node`.
* The [`protoc`](https://protobuf.dev/programming-guides/proto3/#generating) compiler.
* The `protoc-gen-purescript` executable plugin for `protoc` on the `PATH` so that
  [`protoc` can find it](https://protobuf.dev/reference/cpp/api-docs/google.protobuf.compiler.plugin/).

```
$ nix develop

PureScript Protobuf development environment.
libprotoc 3.21.10
purs 0.15.6
node v17.1.0

To build the protoc compiler plugin, run:

    spago -x spago-plugin.dhall build

To compile PureScript .purs files from .proto files, run for example:

    protoc --purescript_out=. google/protobuf/timestamp.proto
```

We can test out code generation immediately by
generating `.purs` files for any of Google’s built-in “well-known types” in the
[`google.protobuf`](https://protobuf.dev/reference/protobuf/google.protobuf/) package namespace. Try the command `protoc --purescript_out=. google/protobuf/any.proto`
or `protoc --purescript_out=. google/protobuf/timestamp.proto`.

To see
[all of the `.proto` definitions](https://github.com/protocolbuffers/protobuf/tree/main/src/google/protobuf)
included with the Nix PureScript Protobuf installation including
the “well-known types,”
`ls $(nix path-info .#protobuf)/src/google/protobuf/*.proto`

If you don't want to use Nix, then install the PureScript toolchain and `protoc`,
and add the executable script
[`bin/protoc-gen-purescript`](bin/protoc-gen-purescript)
to your `PATH`.

## Writing programs with the generated code

The code generator will use the `package` import statement in the `.proto` file
and the base `.proto` file name as the PureScript module name for that file.

A message in a `shapes.proto` descriptor file declared as

```
syntax = "proto3";
package interproc;

message Rectangle {
  double width = 1;
  double height = 2;
}
```

will export these four names from module `Interproc.Shapes` in a
generated `shapes.Interproc.purs` file.

1. A message data type.

   ```purescript
   newtype Rectangle = Rectangle { width :: Maybe Number, height :: Maybe Number }
   ```

   The message data type will also include an `__unknown_fields` array field for
   holding received fields which were not in the descriptor `.proto` file. We can
   ignore `__unknown_fields` if we want to.

2. A message maker which constructs a message from a `Record`
   with some message fields.

   ```purescript
   mkRectangle :: forall r. Record r -> Rectangle
   ```

   All message fields are optional, and can be omitted when making a message.
   There are some extra type constraints, not shown here, which will cause a
   compiler error if we try to add a field which is not in the message data type.

   If we want the compiler to check that we’ve explicitly supplied all the fields,
   then we can use the ordinary message data type constructor `Rectangle`.

3. A message serializer which works with
   [__arraybuffer-builder__](http://pursuit.purescript.org/packages/purescript-arraybuffer-builder/).

   ```purescript
   putRectangle :: forall m. MonadEffect m => Rectangle -> PutM m Unit
   ```

4. A message deserializer which works with
   [__parsing-dataview__](http://pursuit.purescript.org/packages/purescript-parsing-dataview/).

   ```purescript
   parseRectangle :: forall m. MonadEffect m => ByteLength -> ParserT DataView m Rectangle
   ```

   The message parser needs an argument which tells it the
   length of the message which it’s about to parse, because
   [“the Protocol Buffer wire format is not self-delimiting.”](https://protobuf.dev/programming-guides/techniques/#streaming)

In our program, our imports will look something like this.
The only module from this package which we will import into our program
will be the `Protobuf.Library` module.
We'll import the message modules from the generated `.purs` files.
We'll also import modules for reading and writing `ArrayBuffer`s.


```purescript
import Protobuf.Library (Bytes(..))
import Interproc.Shapes (Rectangle, mkRectangle, putRectangle, parseRectangle)
import Parsing (runParserT, ParseError, liftMaybe)
import Data.ArrayBuffer.Builder (execPutM)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.Tuple (Tuple)
import Data.Newtype (unwrap)
```

This is how we serialize a `Rectangle` to an `ArrayBuffer`.
We must be in a `MonadEffect`.

```purescript
do
  arraybuffer <- execPutM $ putRectangle $ mkRectangle
    { width: Just 3.0
    , height: Just 4.0
    }
```

Next we’ll deserialize `Rectangle` from the `ArrayBuffer` that we just made.

```purescript
  result :: Either ParseError {width :: Number, height :: Number}
    <- runParserT (whole arraybuffer) do
      rectangle :: Rectangle <- parseRectangle (byteLength arraybuffer)
```

At this point we’ve consumed all of the parser input and constructed our
`Rectangle` message, but we’re not finished parsing.
We want to “validate” the `Rectangle` message to make sure it has all of the
fields that we require, because in
[*proto3*, all fields are optional](https://github.com/protocolbuffers/protobuf/issues/2497).

Fortunately we are already in the `ParserT` monad,
so we can do better than to “validate”:
[Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/).

We will construct a record `{width::Number, height::Number}`
with the width and height of the `Rectangle`. If the width or height
are missing from the `Rectangle` message, then we will fail in the `ParserT`
monad.

For this validation step,
[pattern matching](https://github.com/purescript/documentation/blob/master/language/Pattern-Matching.md)
on the `Rectangle` message type works well, so we could validate this way:

```purescript
      case rectangle of
        Rectangle { width: Just width, height: Just height } ->
          pure {width, height}
        _ -> fail "Missing required width or height"
```

Or we might want to use `liftMaybe` for more fine-grained validation:

```purescript
      width <- liftMaybe "Missing required width" (unwrap rectangle).width
      height <- liftMaybe "Missing required height" (unwrap rectangle).height
      pure {width, height}
```

And now the `result` is either a parsing error or a fully validated rectangle.

### Dependencies

The generated code modules will import modules from this
package.

The generated code depends on packages which are all in
[__package-sets__](https://github.com/purescript/package-sets).

If the runtime environment is *Node.js*, then it must be at least *v11*,
because that is the version in which
[`TextDecoder`](https://nodejs.org/docs/latest-v11.x/api/globals.html#globals_textdecoder)
 and
[`TextEncoder`](https://nodejs.org/docs/latest-v11.x/api/globals.html#globals_textencoder)
were added to `Globals`.


### Generated message instances

All of the generated message types have instances of
[`Eq`](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Eq#t:Eq),
[`Show`](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Show#t:Show),
[`Generic`](https://pursuit.purescript.org/packages/purescript-generics-rep/docs/Data.Generic.Rep#t:Generic),
[`NewType`](https://pursuit.purescript.org/packages/purescript-newtype/docs/Data.Newtype#t:Newtype).

### Usage Examples

The __protobuf__ repository contains three executable *Node.js*
programs which use code generated by __protobuf__. Refer to these
for further examples of how to use the generated code.

1. The `protoc`
   [compiler plugin](https://github.com/xc-jp/purescript-protobuf/blob/master/plugin/ProtocPlugin/Main.purs).
   The code generator imports generated code. Trippy, right? This program
   literally writes itself.
2. The
   [unit test suite](https://github.com/xc-jp/purescript-protobuf/blob/master/test/Main.purs)
3. The Google
   [conformance test program](https://github.com/xc-jp/purescript-protobuf/blob/master/conformance/Main.purs)

The [Protobuf Decoder Explainer](http://jamesdbrock.github.io/protobuf-decoder-explainer/) shows an
example of how to use this library to parse binary Protobuf when we don’t
have access to the `.proto` descriptor schema file and can’t generate
message-reading code.

### Presence Discipline

This is how [*field presence*](https://github.com/protocolbuffers/protobuf/blob/main/docs/field_presence.md) works
in our implementation.

#### When deserializing

A message field will always be `Just` when the field is present on the wire.

A message field will always be `Nothing` when the field is not present on the wire, even if
it’s a *no presence* field.
If we want to interpret a missing *no presence* field as a
[default value](https://protobuf.dev/programming-guides/proto3/#default) then
we have the
[`Protobuf.Library.toDefault`](https://pursuit.purescript.org/packages/purescript-protobuf/docs/Protobuf.Library#v:toDefault)
 function for that.

#### When serializing

A *no presence* field will not be serialized on the wire when it is `Nothing`, or `Just` the
default value.

An *explicit presence* (`optional`) field will not be serialized on the wire when it is `Nothing`.
It will be serialized when it is `Just` the default value.

### Interpreting invalid encoding parse failures

When the parser encounters an invalid encoding in the Protobuf input
stream then it will fail to parse.

When
[`ParserT`](https://pursuit.purescript.org/packages/purescript-parsing/docs/Parsing#t:ParserT)
fails it will return a `ParseError String (Position {index::Int,line::Int,column::Int})`.

The byte offset at which the parse failure occurred is given by the
`index`.

The path to the Protobuf definition which failed to parse will be included
in the `ParseError String` and delimited by `'/'`, something
like `"Message1 / string_field_1 / Invalid UTF8 encoding."`.

### Protobuf Imports

The Protobuf
[`import`](https://protobuf.dev/programming-guides/proto3/#importing-definitions)
statement allows Protobuf messages to have fields
consisting of Protobuf messages imported from another file, and qualified
by the package name in that file. In order to generate
the correct PureScript module name qualifier on the types of imported message
fields, the code generator must be able to lookup the package name
statement in the imported file.

For that reason, we can only use top-level
(not [nested](https://protobuf.dev/programming-guides/proto3/#nested))
`message` and `enum` types from a Protobuf `import`.

### PureScript Imports

The generated PureScript code will usually have module imports which cause
the `purs` compiler to emit redundant import warnings. Sorry. If this causes
trouble then the imports can be fixed automatically in a precompiling pass
with the command-line tool
[__suggest__](https://github.com/nwolverson/purescript-suggest).

## Nix derivation

The `flake.nix` provides a package `protoc-gen-purescript` so that we
can run the `.proto` → `.purs` generation step as part of a Nix
derivation. Include `protoc-gen-purescript` and `protobuf` as `nativeBuildInputs`.
Then `protoc --purescript_out=path_to_output file.proto` will be runnable
in our derivation phases.

(`protoc-gen-purescript` requires an impure build for the `spago2nix` step,
so you’ll have to grant
trust at the prompt or configure a relaxed Nix sandbox.)

The `flake.nix` provides the Google Protocol Buffers conformance tests
as an `app`. To run the conformance tests right now
[without installing or cloning](https://determinate.systems/posts/nix-run)
anything,

```shell
nix run github:xc-jp/purescript-protobuf#conformance
```

## Contributing

Pull requests welcome.

## Other References

* [__justifill__](https://pursuit.purescript.org/packages/purescript-justifill) package may be useful for message construction.
* [__morello__](https://pursuit.purescript.org/packages/purescript-morello) package may be useful for message validation.
* [Third-Party Add-ons for Protocol Buffers](https://github.com/protocolbuffers/protobuf/blob/main/docs/third_party.md) Google’s list of Protocol Buffers language implementations.
* [A vision for data interchange in Elm](https://gist.github.com/evancz/1c5f2cf34939336ecb79b97bb89d9da6) Comparison of JSON, ProtoBuf, GraphQL by Evan Czaplicki.
