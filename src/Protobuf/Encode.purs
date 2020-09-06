-- | Primitive builders for encoding Google Protocol Buffers.
module Protobuf.Encode
( double
, float
, int32
, int64
, uint32
, uint64
, sint32
, sint64
, fixed32
, fixed64
, sfixed32
, sfixed64
, bool
, string
, bytes
)
where

import Prelude
import Effect (Effect)
import Data.ArrayBuffer.Builder as Builder
import Data.UInt (UInt)
import Data.Long (toUnsigned, lowBits, highBits)
import Data.Long.Unsigned as LU
import Data.Long.Unsigned (Long, Unsigned)
import Data.TextEncoding (encodeUtf8)
import Data.ArrayBuffer.Typed as AT
import Data.ArrayBuffer as AB
import Protobuf.Common (FieldNumber)

-- We have a special varint32 for encoding varints which we're just going
-- to assume fit in the 32-bit range. There is no "32-bit varint" in protobuf.
-- This is a performance optimization, and probably misguided, but we're
-- horrified at the thought of using so many Longs in our program.
import Protobuf.Encode32
( zigzag32
, tag32
, varint32
)
import Protobuf.Encode64
( zigzag64
, varint64
)

-- | __double__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
double :: forall m. MonadEffect m => FieldNumber -> Number -> Builder.PutM m Unit
-- https://developers.google.com/protocol-buffers/docs/encoding#non-varint_numbers
double fieldNumber n = do
  tag32 fieldNumber 1
  Builder.putFloat64le n

-- | __float__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
float :: forall m MonadEffect m => FieldNumber -> Float32 -> Builder.PutM m Unit
float fieldNumber n = do
  tag32 fieldNumber 5
  Builder.putFloat32le n

-- | __int32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
int32 :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
-- “If you use int32 or int64 as the type for a negative number, the resulting
-- varint is always ten bytes long”
-- https://developers.google.com/protocol-buffers/docs/encoding#signed_integers
int32 fieldNumber n = do
  tag32 fieldNumber 0
  varint64 $ LU.fromInt n

-- | __int64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
int64 :: forall m. MonadEffect m => FieldNumber -> Long Signed -> Builder.PutM m Unit
-- “If you use int32 or int64 as the type for a negative number, the resulting
-- varint is always ten bytes long”
-- https://developers.google.com/protocol-buffers/docs/encoding#signed_integers
int64 fieldNumber n = do
  tag32 fieldNumber 0
  varint64 $ toUnsigned n

-- | __uint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint32 :: forall m. MonadEffect m => FieldNumber -> UInt -> Builder.PutM m Unit
uint32 fieldNumber n = do
  tag32 fieldNumber 0
  varint32 n

-- | __uint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint64 :: forall m. MonadEffect m => FieldNumber -> Long Unsigned -> Builder.PutM m Unit
uint64 fieldNumber n = do
  tag32 fieldNumber 0
  varint64 n

-- | __sint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sint32 :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
sint32 fieldNumber n = do
  tag32 fieldNumber 0
  varint32 $ zigZag32 n

-- | __sint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sint64 :: forall m. MonadEffect m => FieldNumber -> Long Signed -> Builder.PutM m Unit
sint64 fieldNumber n = do
  tag32 fieldNumber 0
  varint64 $ zigZag64 n

-- | __fixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
fixed32 :: forall m. MonadEffect m => FieldNumber -> UInt -> Builder.PutM m Unit
-- https://developers.google.com/protocol-buffers/docs/encoding#non-varint_numbers
fixed32 fieldNumber n = do
  tag32 fieldNumber 5
  Builder.putUint32le n

-- | __fixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
fixed64 :: forall m. MonadEffect m => FieldNumber -> Long Unsigned -> Builder.PutM m Unit
fixed64 fieldNumber n = do
  tag32 fieldNumber 1
  Builder.putInt32le $ LU.lowBits n
  Builder.putInt32le $ LU.highBits n

-- | __sfixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sfixed32 :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
sfixed32 fieldNumber n = do
  tag fieldNumber 5
  Builder.putInt32le n

-- | __sfixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sfixed64 :: forall m. MonadEffect m => FieldNumber -> Long Signed -> Builder.PutM m Unit
sfixed64 fieldNumber n = do
  tag32 fieldNumber 1
  Builder.putInt32le $ lowBits n
  Builder.putInt32le $ highBits n

-- | __bool__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
bool :: forall m. MonadEffect m => FieldNumber -> Boolean -> Builder.PutM m Unit
bool fieldNumber n = do
  tag32 fieldNumber 0
  if n then Builder.putUint8 1 else Builder.putUint8 0

-- | __string__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
string :: forall m. MonadEffect m => FieldNumber -> String -> Builder.PutM m Unit
-- https://developers.google.com/protocol-buffers/docs/encoding#strings
string fieldNumber s = do
  tag32 fieldNumber 2
  let stringbuf = AT.buffer $ encodeUtf8 s
  int32 $ AB.byteLength stringbuf
  Builder.putArrayBuffer stringbuf

-- | __bytes__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
bytes :: forall m. MonadEffect m => FieldNumber -> ArrayBuffer -> Builder.PutM m Unit
-- I guess if we wanted to do this right, this could be a DataView.
-- But for that, the ArrayBuffer Builder would have to accept DataView.
bytes fieldNumber s = do
  tag32 fieldNumber 2
  int32 $ AB.byteLength s
  Builder.putArrayBuffer s

