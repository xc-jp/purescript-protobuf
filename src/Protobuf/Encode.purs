-- | Primitive builders for encoding Google Protocol Buffers.
module Protobuf.Encode
( double
, float
, int32
, int64as32
, uint32
, uint64as32
, sint32
, sint64as32
, fixed32
, fixed64as32
, sfixed32
, sfixed64as32
, bool
, string
, bytes
)
where

import Prelude
import Effect (Effect)
import Data.ArrayBuffer.Builder as Builder
import Data.UInt (UInt, fromInt, (.&.), (.|.), (.^.))
import Data.TextEncoding (encodeUtf8)
import Data.ArrayBuffer.Typed as AT
import Data.ArrayBuffer as AB
import Protobuf.Common (FieldNumber)


-- | __double__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
double :: forall m. MonadEffect m => FieldNumber -> Number -> Builder.PutM m Unit
-- https://developers.google.com/protocol-buffers/docs/encoding#non-varint_numbers
double fieldNumber n = do
  tag fieldNumber 1
  Builder.putFloat64le n

-- | __float__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
float :: forall m MonadEffect m => FieldNumber -> Float32 -> Builder.PutM m Unit
float fieldNumber n = do
  tag fieldNumber 5
  Builder.putFloat32le n

-- | __int32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
int32 :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
-- “If you use int32 or int64 as the type for a negative number, the resulting
-- varint is always ten bytes long”
-- https://developers.google.com/protocol-buffers/docs/encoding#signed_integers
int32 fieldNumber n = do
  tag fieldNumber 0
  varint32 $ fromInt n -- TODO Wrong when negative

-- | We don't have `Int64` in Purescript, but this will encode an `Int`
-- | to an __int64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
int64as32 :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
-- “If you use int32 or int64 as the type for a negative number, the resulting
-- varint is always ten bytes long”
-- https://developers.google.com/protocol-buffers/docs/encoding#signed_integers
int64as32 fieldNumber n = do
  tag fieldNumber 0
  varint32 $ fromInt n -- TODO Wrong when negative

-- | __uint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint32 :: forall m. MonadEffect m => FieldNumber -> UInt -> Builder.PutM m Unit
uint32 fieldNumber n = do
  tag fieldNumber 0
  varint32 n

-- | We don't have `UInt64` in Purescript, but this will encode a `UInt`
-- | to a __uint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint64as32 :: forall m. MonadEffect m => FieldNumber -> UInt -> Builder.PutM m Unit
uint64as32 fieldNumber n = do
  tag fieldNumber 0
  varint32 n

-- | __sint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sint32 :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
sint32 fieldNumber n = do
  tag fieldNumber 0
  varint32 $ zigZag32 n

-- | We don't have `Int64` in Purescript, but this will encode an `Int`
-- | to a __sint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sint64as32 :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
sint64as32 fieldNumber n = do
  tag fieldNumber 0
  varint32 $ zigZag32 n

-- | __fixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
fixed32 :: forall m. MonadEffect m => FieldNumber -> UInt -> Builder.PutM m Unit
-- https://developers.google.com/protocol-buffers/docs/encoding#non-varint_numbers
fixed32 fieldNumber n = do
  tag fieldNumber 5
  Builder.putUint32le n

-- | We don't have `UInt64` in Purescript, but this will encode a `UInt`
-- | to a __fixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
fixed64as32 :: forall m. MonadEffect m => FieldNumber -> UInt -> Builder.PutM m Unit
fixed64as32 fieldNumber n = do
  tag fieldNumber 1
  Builder.putUint32le n
  Builder.putUint32le (fromInt 0)

-- | __sfixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sfixed32 :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
sfixed32 fieldNumber n = do
  tag fieldNumber 5
  Builder.putInt32le n

-- | We don't have `Int64` in Purescript, but this will encode an `Int`
-- | to an __sfixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sfixed64as32 :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
sfixed64as32 fieldNumber n = do
  tag fieldNumber 1
  Builder.putInt32le n
  if n < 0
    then Builder.putInt32le 0xFFFF
    else Builder.putInt32le 0

-- | __bool__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
bool :: forall m. MonadEffect m => FieldNumber -> Boolean -> Builder.PutM m Unit
bool fieldNumber n = do
  tag fieldNumber 0
  if n then Builder.putUint8 1 else Builder.putUint8 0

-- | __string__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
string :: forall m. MonadEffect m => FieldNumber -> String -> Builder.PutM m Unit
-- https://developers.google.com/protocol-buffers/docs/encoding#strings
string fieldNumber s = do
  tag fieldNumber 2
  let stringbuf = AT.buffer $ encodeUtf8 s
  int32 $ AB.byteLength stringbuf
  Builder.putArrayBuffer stringbuf

-- | __bytes__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
bytes :: forall m. MonadEffect m => FieldNumber -> ArrayBuffer -> Builder.PutM m Unit
-- I guess if we wanted to do this right, this could be a DataView.
-- But for that, the ArrayBuffer Builder would have to accept DataView.
bytes fieldNumber s = do
  tag fieldNumber 2
  int32 $ AB.byteLength s
  Builder.putArrayBuffer s

-- | https://developers.google.com/protocol-buffers/docs/encoding#signed_integers
zigZag32 :: Int -> UInt
zigZag32 n = let n' = fromInt n in (n' `shl` 1) .^. (n' `zshr` 31)

-- | https://developers.google.com/protocol-buffers/docs/encoding#structure
tag :: forall m. MonadEffect m => UInt -> WireType -> Builder.PutM m Unit
tag fieldNumber wireType =
  varint32 $ (fieldNumber `shl` 3) .|. (fromInt wireType)

-- | https://developers.google.com/protocol-buffers/docs/encoding#varints
varint32 :: forall m. MonadEffect m => UInt -> Builder.PutM m Unit
varint32 n_0 = do
  let group_0 = n_0 .&. u0x7F
      n_1     = n_0 `zshr` u7
  in
  if n_1 == u0
    then Builder.putUint8 group_0
    else do
      Builder.putUint8 $  u0x80 .|. group_0
      let group_1 = n_1 .&. u0x7F
          n_2     = n_1 `zshr` u7
      in
      if n_2 == u0
        then Builder.putUint8 group_1
        else do
          Builder.putUint8 $ u0x80 .|. group_1
          let group_2 = n_2 .&. u0x7F
              n_3     = n_2 `zshr` u7
          in
          if n_3 == u0
            then Builder.putUint8 group_2
            else do
              Builder.putUint8 $ u0x80 .| group_2
              let group_3 = n_3 .&. u0x7F
                  n_4     = n_3 `zshr` u7
              in
              if n_4 == u0
                then Builder.putUint8 group_3
                else do
                  Builder.putUint8 $ u0x80 .|. group_3
                  Builder.putUint8 n_4
 where
  u0    = fromInt 0
  u7    = fromInt 7
  u0x7F = fromInt 0x7F
  u0x80 = fromInt 0x80

