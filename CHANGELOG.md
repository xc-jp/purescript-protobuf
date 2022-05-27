# v3.0.0 Unreleased

- PureScript v0.15.0
- Protobuf v3.21.0
- Replace __text-encoding__ dependency with __web-encoding__. Consequently we require *Node.js ≥ v11*.
- Replace __longs__ dependency with __int64__.
- Remove the only NPM dependency: __longs.js__.
- Rework the async I/O in the protoc plugin.

# v2.1.2 2021-09-10

Now the codegen
[Nix derivation](https://github.com/xc-jp/purescript-protobuf#nix-derivation)
is working properly.

# v2.1.0 2021-07-25

Reorganized the `spago` files so that programs which import generated code
can use `spago.dhall` for the minimum necessary dependencies instead
of `spago-library.dhall`.

Reorganized the PureScript modules so that all modules except `Library`
are now explicity `Internal` modules.

# v2.0.0 2021-07-22

Upgraded to PureScript 0.14.3.

For byte fields we’re now using the `DataBuff` type
from __arraybuffer-builder__ v2.1.0 which allows us to avoid array copies,
and so may speed up encoding and decoding in some cases.

Created a `Protobuf.Prelude` module to eliminate many import warnings
in the generated code.

## Breaking changes

The `Bytes` field type was formerly a wrapper for `ArrayBuffer`, and now
it is a wrapper for `DataBuff`, which may be either `ArrayBuffer` or `DataView`.
This changes the API and will require some changes in dependent code.

## Bugfixes

Fixed a bug which sometimes incorrectly errored with “index out of bounds” when
decoding packed repeated fields of double, float, fixed32, or sfixed32.

Fixed a bug which would cause a compilation failure in generated code for
enums with negative values.

# v1.8.0

Large (×1000) speedup for decoding packed repeated fields of

* double
* float
* fixed32
* fixed64
* sfixed32
* sfixed64

# v1.7.0

Upgraded from Protobuf v3.14.0 to v3.15.8.

Now supporting [proto3 field presence](https://github.com/protocolbuffers/protobuf/blob/master/docs/field_presence.md).

# v1.6.0

Upgraded from Protobuf v3.9.2 to v3.14.0.

To conform with v3.14.0, we’ve added a new type class `Default` for
awareness of Protobuf
[default values](https://developers.google.com/protocol-buffers/docs/proto3#default).

# v1.5.0

In the code generator, when `protoc` hands us nonsense descriptors, then
report the error instead of generating uncompilable code.

Export `Library.manyLength`, it’s generally useful.

# v1.4.0

Improve `Protobuf.Runtime.manyLength` for stack-safety in the case of
packed repeated fields.

# v1.3.0

Newtype `Protobuf.Library.Bytes` wrapper for `ArrayBuffer`, so that all
messages can have `Eq` and `Show` instances.

Nix `default.nix` derivation for including the code generation step as
part another derivation.

# v1.2.0

Preserve unknown fields, so now all Google binary-wire-format proto3
conformance tests pass.

# v1.1.0

Added parsing labels so that parsing failure messages include a path
to the protobuf definition in which the failure occurred.

# v1.0.0

First release
