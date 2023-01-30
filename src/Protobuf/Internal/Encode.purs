-- | Primitive builders for encoding Google Protocol Buffers.
module Protobuf.Internal.Encode
  ( encodeDoubleField
  , encodeDouble
  , encodeFloatField
  , encodeFloat
  , encodeInt32Field
  , encodeInt32
  , encodeInt64Field
  , encodeInt64
  , encodeUint32Field
  , encodeUint32
  , encodeUint64Field
  , encodeUint64
  , encodeSint32Field
  , encodeSint32
  , encodeSint64Field
  , encodeSint64
  , encodeFixed32Field
  , encodeFixed32
  , encodeFixed64Field
  , encodeFixed64
  , encodeSfixed32Field
  , encodeSfixed32
  , encodeSfixed64Field
  , encodeSfixed64
  , encodeBoolField
  , encodeBool
  , encodeStringField
  , encodeBytesField
  , encodeBuilder
  , encodeVarint32
  , encodeZigzag32
  , encodeTag32
  , encodeVarint64
  , encodeZigzag64
  ) where

import Prelude

import Control.Monad.Writer.Trans (tell)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Builder (toView)
import Data.ArrayBuffer.Builder as Builder
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as AT
import Data.Enum (fromEnum)
import Data.Float32 (Float32)
import Data.Int64 (Int64)
import Data.Int64 as Int64
import Data.UInt64 (UInt64)
import Data.UInt64 as UInt64
import Web.Encoding.TextEncoder as TextEncoder
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Class (class MonadEffect)
import Effect.Unsafe (unsafePerformEffect)
import Protobuf.Internal.Common (FieldNumber, WireType(..), Bytes(..))

encodeDoubleField :: forall m. MonadEffect m => FieldNumber -> Number -> Builder.PutM m Unit
-- https://protobuf.dev/programming-guides/encoding#non-varint_numbers
encodeDoubleField fieldNumber n = do
  encodeTag32 fieldNumber Bits64
  encodeDouble n

-- | __double__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeDouble :: forall m. MonadEffect m => Number -> Builder.PutM m Unit
-- https://protobuf.dev/programming-guides/encoding#non-varint_numbers
encodeDouble n = do
  Builder.putFloat64le n

encodeFloatField :: forall m. MonadEffect m => FieldNumber -> Float32 -> Builder.PutM m Unit
encodeFloatField fieldNumber n = do
  encodeTag32 fieldNumber Bits32
  encodeFloat n

-- | __float__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeFloat :: forall m. MonadEffect m => Float32 -> Builder.PutM m Unit
encodeFloat n = do
  Builder.putFloat32le n

encodeInt32Field :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
-- “If you use int32 or int64 as the type for a negative number, the resulting
-- varint is always ten bytes long”
-- https://protobuf.dev/programming-guides/encoding#signed_integers
encodeInt32Field fieldNumber n = do
  encodeTag32 fieldNumber VarInt
  encodeInt32 n

-- | __int32__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeInt32 :: forall m. MonadEffect m => Int -> Builder.PutM m Unit
-- “If you use int32 or int64 as the type for a negative number, the resulting
-- varint is always ten bytes long”
-- https://protobuf.dev/programming-guides/encoding#signed_integers
encodeInt32 n = do
  encodeVarint64 $ Int64.toUnsigned $ Int64.fromInt n

encodeInt64Field :: forall m. MonadEffect m => FieldNumber -> Int64 -> Builder.PutM m Unit
-- “If you use int32 or int64 as the type for a negative number, the resulting
-- varint is always ten bytes long”
-- https://protobuf.dev/programming-guides/encoding#signed_integers
encodeInt64Field fieldNumber n = do
  encodeTag32 fieldNumber VarInt
  encodeInt64 n

-- | __int64__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeInt64 :: forall m. MonadEffect m => Int64 -> Builder.PutM m Unit
-- “If you use int32 or int64 as the type for a negative number, the resulting
-- varint is always ten bytes long”
-- https://protobuf.dev/programming-guides/encoding#signed_integers
encodeInt64 n = do
  encodeVarint64 $ Int64.toUnsigned n

encodeUint32Field :: forall m. MonadEffect m => FieldNumber -> UInt -> Builder.PutM m Unit
encodeUint32Field fieldNumber n = do
  encodeTag32 fieldNumber VarInt
  encodeUint32 n

-- | __uint32__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeUint32 :: forall m. MonadEffect m => UInt -> Builder.PutM m Unit
encodeUint32 n = do
  encodeVarint32 n

encodeUint64Field :: forall m. MonadEffect m => FieldNumber -> UInt64 -> Builder.PutM m Unit
encodeUint64Field fieldNumber n = do
  encodeTag32 fieldNumber VarInt
  encodeUint64 n

-- | __uint64__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeUint64 :: forall m. MonadEffect m => UInt64 -> Builder.PutM m Unit
encodeUint64 n = do
  encodeVarint64 n

encodeSint32Field :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
encodeSint32Field fieldNumber n = do
  encodeTag32 fieldNumber VarInt
  encodeSint32 n

-- | __sint32__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeSint32 :: forall m. MonadEffect m => Int -> Builder.PutM m Unit
encodeSint32 n = do
  encodeVarint32 $ encodeZigzag32 n

encodeSint64Field :: forall m. MonadEffect m => FieldNumber -> Int64 -> Builder.PutM m Unit
encodeSint64Field fieldNumber n = do
  encodeTag32 fieldNumber VarInt
  encodeSint64 n

-- | __sint64__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeSint64 :: forall m. MonadEffect m => Int64 -> Builder.PutM m Unit
encodeSint64 n = do
  encodeVarint64 $ encodeZigzag64 n

encodeFixed32Field :: forall m. MonadEffect m => FieldNumber -> UInt -> Builder.PutM m Unit
-- https://protobuf.dev/programming-guides/encoding#non-varint_numbers
encodeFixed32Field fieldNumber n = do
  encodeTag32 fieldNumber Bits32
  encodeFixed32 n

-- | __fixed32__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeFixed32 :: forall m. MonadEffect m => UInt -> Builder.PutM m Unit
-- https://protobuf.dev/programming-guides/encoding#non-varint_numbers
encodeFixed32 n = do
  Builder.putUint32le n

encodeFixed64Field :: forall m. MonadEffect m => FieldNumber -> UInt64 -> Builder.PutM m Unit
encodeFixed64Field fieldNumber n = do
  encodeTag32 fieldNumber Bits64
  encodeFixed64 n

-- | __fixed64__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeFixed64 :: forall m. MonadEffect m => UInt64 -> Builder.PutM m Unit
encodeFixed64 n = do
  Builder.putInt32le $ UInt64.lowBits n
  Builder.putInt32le $ UInt64.highBits n

encodeSfixed32Field :: forall m. MonadEffect m => FieldNumber -> Int -> Builder.PutM m Unit
encodeSfixed32Field fieldNumber n = do
  encodeTag32 fieldNumber Bits32
  encodeSfixed32 n

-- | __sfixed32__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeSfixed32 :: forall m. MonadEffect m => Int -> Builder.PutM m Unit
encodeSfixed32 n = do
  Builder.putInt32le n

encodeSfixed64Field :: forall m. MonadEffect m => FieldNumber -> Int64 -> Builder.PutM m Unit
encodeSfixed64Field fieldNumber n = do
  encodeTag32 fieldNumber Bits64
  encodeSfixed64 n

-- | __sfixed64__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeSfixed64 :: forall m. MonadEffect m => Int64 -> Builder.PutM m Unit
encodeSfixed64 n = do
  Builder.putInt32le $ Int64.lowBits n
  Builder.putInt32le $ Int64.highBits n

encodeBoolField :: forall m. MonadEffect m => FieldNumber -> Boolean -> Builder.PutM m Unit
encodeBoolField fieldNumber n = do
  encodeTag32 fieldNumber VarInt
  encodeBool n

-- | __bool__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeBool :: forall m. MonadEffect m => Boolean -> Builder.PutM m Unit
encodeBool n = do
  if n then Builder.putInt8 1 else Builder.putInt8 0

-- | __string__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeStringField :: forall m. MonadEffect m => FieldNumber -> String -> Builder.PutM m Unit
-- https://protobuf.dev/programming-guides/encoding#strings
encodeStringField fieldNumber s = do
  encodeTag32 fieldNumber LenDel
  let
    stringbuf = AT.buffer $ TextEncoder.encode s textEncoder
  encodeVarint32 $ UInt.fromInt $ AB.byteLength stringbuf
  Builder.putArrayBuffer stringbuf

textEncoder :: TextEncoder.TextEncoder
textEncoder = unsafePerformEffect TextEncoder.new

-- | __bytes__
-- | [Scalar Value Type](https://protobuf.dev/programming-guides/proto3#scalar)
encodeBytesField :: forall m. MonadEffect m => FieldNumber -> Bytes -> Builder.PutM m Unit
encodeBytesField fieldNumber (Bytes buf) = do
  encodeTag32 fieldNumber LenDel
  encodeVarint32 $ UInt.fromInt $ DV.byteLength $ toView buf
  Builder.putDataBuff buf

-- | `tell` with a tag and a length delimit.
encodeBuilder :: forall m. MonadEffect m => FieldNumber -> Builder.Builder -> Builder.PutM m Unit
encodeBuilder fieldNumber s = do
  encodeTag32 fieldNumber LenDel
  encodeVarint32 $ UInt.fromInt $ Builder.length s
  tell s

-- | https://protobuf.dev/programming-guides/encoding#signed_integers
encodeZigzag32 :: Int -> UInt
encodeZigzag32 n = let n' = UInt.fromInt n in (n' `UInt.shl` (UInt.fromInt 1)) `UInt.xor` (n' `UInt.shr` (UInt.fromInt 31))

-- | https://protobuf.dev/programming-guides/encoding#structure
encodeTag32 :: forall m. MonadEffect m => FieldNumber -> WireType -> Builder.PutM m Unit
encodeTag32 fieldNumber wireType = encodeVarint32 $ (fieldNumber `UInt.shl` (UInt.fromInt 3)) `UInt.or` (UInt.fromInt $ fromEnum wireType)

-- | There is no `varint32` in the Protbuf spec, this is
-- | just a performance-improving assumption we make
-- | in cases where we would be surprised to see a number
-- | larger than 32 bits, such as in field numbers.
-- | We think this is worth the risk because `UInt` is
-- | represented as a native Javascript Number whereas
-- | `UInt64` is a composite library type, so we expect the
-- | performance difference to be significant.
-- |
-- | https://protobuf.dev/programming-guides/encoding#varints
encodeVarint32 :: forall m. MonadEffect m => UInt -> Builder.PutM m Unit
encodeVarint32 n_0 = do
  let
    group_0 = n_0 `UInt.and` u0x7F

    n_1 = n_0 `UInt.zshr` u7
  if n_1 == u0 then
    Builder.putUint8 group_0
  else do
    Builder.putUint8 $ u0x80 `UInt.or` group_0
    let
      group_1 = n_1 `UInt.and` u0x7F

      n_2 = n_1 `UInt.zshr` u7
    if n_2 == u0 then
      Builder.putUint8 group_1
    else do
      Builder.putUint8 $ u0x80 `UInt.or` group_1
      let
        group_2 = n_2 `UInt.and` u0x7F

        n_3 = n_2 `UInt.zshr` u7
      if n_3 == u0 then
        Builder.putUint8 group_2
      else do
        Builder.putUint8 $ u0x80 `UInt.or` group_2
        let
          group_3 = n_3 `UInt.and` u0x7F

          n_4 = n_3 `UInt.zshr` u7
        if n_4 == u0 then
          Builder.putUint8 group_3
        else do
          Builder.putUint8 $ u0x80 `UInt.or` group_3
          Builder.putUint8 n_4
  where
  u0 = UInt.fromInt 0

  u7 = UInt.fromInt 7

  u0x7F = UInt.fromInt 0x7F

  u0x80 = UInt.fromInt 0x80

-- | https://protobuf.dev/programming-guides/encoding#signed_integers
encodeZigzag64 :: Int64 -> UInt64
encodeZigzag64 n = Int64.toUnsigned $ (n `Int64.shl` (Int64.fromInt 1)) `Int64.xor` (n `Int64.shr` (Int64.fromInt 63))

encodeVarint64 :: forall m. MonadEffect m => UInt64 -> Builder.PutM m Unit
encodeVarint64 n_0 = do
  let
    group_0 = takeGroup n_0

    n_1 = n_0 `UInt64.zshr` u7
  if n_1 == u0 then
    Builder.putUint8 group_0
  else do
    Builder.putUint8 $ contGroup group_0
    let
      group_1 = takeGroup n_1

      n_2 = n_1 `UInt64.zshr` u7
    if n_2 == u0 then
      Builder.putUint8 group_1
    else do
      Builder.putUint8 $ contGroup group_1
      let
        group_2 = takeGroup n_2

        n_3 = n_2 `UInt64.zshr` u7
      if n_3 == u0 then
        Builder.putUint8 group_2
      else do
        Builder.putUint8 $ contGroup group_2
        let
          group_3 = takeGroup n_3

          n_4 = n_3 `UInt64.zshr` u7
        if n_4 == u0 then
          Builder.putUint8 group_3
        else do
          Builder.putUint8 $ contGroup group_3
          let
            group_4 = takeGroup n_4

            n_5 = n_4 `UInt64.zshr` u7
          if n_5 == u0 then
            Builder.putUint8 group_4
          else do
            Builder.putUint8 $ contGroup group_4
            let
              group_5 = takeGroup n_5

              n_6 = n_5 `UInt64.zshr` u7
            if n_6 == u0 then
              Builder.putUint8 group_5
            else do
              Builder.putUint8 $ contGroup group_5
              let
                group_6 = takeGroup n_6

                n_7 = n_6 `UInt64.zshr` u7
              if n_7 == u0 then
                Builder.putUint8 group_6
              else do
                Builder.putUint8 $ contGroup group_6
                let
                  group_7 = takeGroup n_7

                  n_8 = n_7 `UInt64.zshr` u7
                if n_8 == u0 then
                  Builder.putUint8 group_7
                else do
                  Builder.putUint8 $ contGroup group_7
                  let
                    group_8 = takeGroup n_8

                    n_9 = n_8 `UInt64.zshr` u7
                  if n_9 == u0 then
                    Builder.putUint8 group_8
                  else do
                    Builder.putUint8 $ contGroup group_8
                    Builder.putUint8 $ takeGroup n_9
  where
  -- copy the low seven bits group from a UInt64
  takeGroup :: UInt64 -> UInt
  takeGroup n = (UInt.fromInt $ UInt64.lowBits n) `UInt.and` u0x7F

  -- Set the high eigth continuation bit of a group
  contGroup :: UInt -> UInt
  contGroup n = u0x80 `UInt.or` n

  u0 = UInt64.unsafeFromInt 0 :: UInt64

  u7 = UInt64.unsafeFromInt 7 :: UInt64

  u0x7F = UInt.fromInt 0x7F

  u0x80 = UInt.fromInt 0x80
