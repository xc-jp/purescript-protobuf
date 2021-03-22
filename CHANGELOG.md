Elide default values when encoding optional fields.


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
