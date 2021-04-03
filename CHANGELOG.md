# v2.0.0

For __purescript-protobuf__ version 2, we’re trying to strictly conform
to Protocol Buffers version 3.11.4?
To that end, we're no longer using `Maybe` for field values, and instead we’re
using the Protobuf
[default values](https://developers.google.com/protocol-buffers/docs/proto3#default).

There is a new type class `Default`. In version 1, all message fields were
`Maybe`s. In version 2, no message fields are `Maybe`s, and all message fields
have a `Default` instance.

So this is a pretty big breaking change, but it brings our implementation into
closer agreement with what Google has designed Protobuf to be (optimized for C++
structs), rather than what we wish it were (optimized for a language with sum types.)

In version 1, we had to be mindful that sometimes when we received a field
value of `Nothing` it actually meant *0*. In version 2, we have to be mindful
that sometimes when we receive a field value of *0* it actually means `Nothing`.
We have introduced a `fromDefault` function to help us remember.

We’re still ignoring the `REQUIRED`/`OPTIONAL` distinction, even though it
appears that Jeff Dean has caved and
[allowed it back in](https://chromium.googlesource.com/external/github.com/protocolbuffers/protobuf/+/refs/heads/master/docs/implementing_proto3_presence.md) in the form of
“synthetic oneofs.”

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
