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
