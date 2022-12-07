-- | Primitive parsers for decoding Google Protocol Buffers.
module Protobuf.Internal.Decode
  ( decodeDouble
  , decodeDoubleArray
  , decodeFloat
  , decodeFloatArray
  , decodeInt32
  , decodeInt64
  , decodeUint32
  , decodeUint64
  , decodeSint32
  , decodeSint64
  , decodeFixed32
  , decodeFixed32Array
  , decodeFixed64
  , decodeFixed64Array
  , decodeSfixed32
  , decodeSfixed32Array
  , decodeSfixed64
  , decodeSfixed64Array
  , decodeBool
  , decodeString
  , decodeBytes
  , decodeZigzag32
  , decodeVarint32
  , decodeTag32
  , decodeZigzag64
  , decodeVarint64
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Builder (DataBuff(..))
import Data.ArrayBuffer.Cast (toUint8Array)
import Data.ArrayBuffer.Types (ByteLength, DataView)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Float32 (Float32)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Int64 (Int64)
import Data.Int64 as Int64
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.UInt64 (UInt64)
import Data.UInt64 as UInt64
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (catchException, message)
import Effect.Unsafe (unsafePerformEffect)
import Parsing (ParserT, fail)
import Parsing.DataView as Parse
import Protobuf.Internal.Common (Bytes(..), FieldNumber, WireType, label)
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel as UtfLabel

-- | __double__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeDouble :: forall m. MonadEffect m => ParserT DataView m Number
decodeDouble = Parse.anyFloat64le

-- | repeated packed __double__
-- |
-- | Equivalent to `manyLength decodeDouble`, but specialized so it’s faster.
decodeDoubleArray :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array Number)
decodeDoubleArray len = label "decodeDoubleArray / " do
  when ((len `mod` 8) /= 0) $ fail "Cannot consume a byteLength indivisible by 8."
  dv <- Parse.takeN len
  pure $ unsafeCopyFloat64le dv (len `div` 8)

-- | Copy *N* little-endian `Number`s from the `DataView` into a new `Array`.
foreign import unsafeCopyFloat64le :: DataView -> Int -> Array Number

-- | __float__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeFloat :: forall m. MonadEffect m => ParserT DataView m Float32
decodeFloat = Parse.anyFloat32le

-- | repeated packed __float__
-- |
-- | Equivalent to `manyLength decodeFloat`, but specialized so it’s faster.
decodeFloatArray :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array Float32)
decodeFloatArray len = label "decodeFloatArray / " do
  when ((len `mod` 4) /= 0) $ fail "Cannot consume a byteLength indivisible by 4."
  dv <- Parse.takeN len
  pure $ unsafeCopyFloat32le dv (len `div` 4)

-- | Copy *N* little-endian `Float32`s from the `DataView` into a new `Array`.
foreign import unsafeCopyFloat32le :: DataView -> Int -> Array Float32

-- | __int32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeInt32 :: forall m. MonadEffect m => ParserT DataView m Int
decodeInt32 = do
  n <- decodeVarint64
  -- But this is a problem with the Protobuf spec?
  -- “If you use int32 or int64 as the type for a negative number, the resulting
  -- varint is always ten bytes long”
  -- https://developers.google.com/protocol-buffers/docs/encoding#signed_integers
  -- So what are we supposed to do if the field type is int32 but the
  -- decoded varint is too big? There is no guarantee that either negative
  -- or positive numbers encoded as varints will be in range.
  -- I suppose we could check that the high bits are all 0
  -- for positive or all 1 for negative.
  -- The conformance tests seem to like it if we just throw away
  -- the hight bits. So that's what we'll do.
  --
  -- conformance checker hates this:
  --   case Int64.toInt n of
  --     Nothing -> fail "int32 value out of range."
  --     Just x -> pure x
  pure $ UInt64.lowBits n

-- | __int64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeInt64 :: forall m. MonadEffect m => ParserT DataView m Int64
decodeInt64 = Int64.toSigned <$> decodeVarint64

-- | __uint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeUint32 :: forall m. MonadEffect m => ParserT DataView m UInt
decodeUint32 = do
  n <- decodeVarint64
  pure $ UInt.fromInt $ UInt64.lowBits n

-- | __uint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeUint64 :: forall m. MonadEffect m => ParserT DataView m UInt64
decodeUint64 = decodeVarint64

-- | __sint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeSint32 :: forall m. MonadEffect m => ParserT DataView m Int
decodeSint32 = do
  n <- decodeZigzag32 <$> decodeVarint32
  pure n

-- | __sint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeSint64 :: forall m. MonadEffect m => ParserT DataView m Int64
decodeSint64 = decodeZigzag64 <$> decodeVarint64

-- | __fixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeFixed32 :: forall m. MonadEffect m => ParserT DataView m UInt
decodeFixed32 = Parse.anyUint32le

-- | repeated packed __fixed32__
-- |
-- | Equivalent to `manyLength decodeFixed32`, but specialized so it’s faster.
decodeFixed32Array :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array UInt)
-- | repeated packed __float__
decodeFixed32Array len = label "decodeFixed32Array / " do
  when ((len `mod` 4) /= 0) $ fail "Cannot consume a byteLength indivisible by 4."
  dv <- Parse.takeN len
  pure $ unsafeCopyUInt32le dv (len `div` 4)

-- | Copy *N* little-endian `UInt`s from the `DataView` into a new `Array`.
foreign import unsafeCopyUInt32le :: DataView -> Int -> Array UInt

-- | __fixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeFixed64 :: forall m. MonadEffect m => ParserT DataView m UInt64
decodeFixed64 = UInt64.fromLowHighBits <$> Parse.anyInt32le <*> Parse.anyInt32le

-- | repeated packed __fixed64__
-- |
-- | Equivalent to `manyLength decodeFixed64`, but specialized so it’s faster.
decodeFixed64Array :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array UInt64)
-- | repeated packed __float__
decodeFixed64Array len = label "decodeFixed64Array / " do
  when ((len `mod` 8) /= 0) $ fail "Cannot consume a byteLength indivisible by 8."
  dv <- Parse.takeN len
  pure $ unsafeCopyInt64le (mkFn2 UInt64.fromLowHighBits) dv (len `div` 8)

-- | Copy *N* little-endian `Int64`s or `UInt64`s from the `DataView` into a new `Array`.
-- | Takes a `fromLowHighBits` constructor.
foreign import unsafeCopyInt64le :: forall a. (Fn2 Int Int a) -> DataView -> Int -> Array a

-- | __sfixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeSfixed32 :: forall m. MonadEffect m => ParserT DataView m Int
decodeSfixed32 = Parse.anyInt32le

-- | repeated packed __sfixed32__
-- |
-- | Equivalent to `manyLength decodeSfixed32`, but specialized so it’s faster.
decodeSfixed32Array :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array Int)
-- | repeated packed __float__
decodeSfixed32Array len = label "decodeSfixed32Array / " do
  when ((len `mod` 4) /= 0) $ fail "Cannot consume a byteLength indivisible by 4."
  dv <- Parse.takeN len
  pure $ unsafeCopyInt32le dv (len `div` 4)

-- | Copy *N* little-endian `Int`s from the `DataView` into a new `Array`.
foreign import unsafeCopyInt32le :: DataView -> Int -> Array Int

-- | __sfixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeSfixed64 :: forall m. MonadEffect m => ParserT DataView m Int64
decodeSfixed64 = Int64.fromLowHighBits <$> Parse.anyInt32le <*> Parse.anyInt32le

-- | repeated packed __sfixed64__
-- |
-- | Equivalent to `manyLength decodeSfixed64`, but specialized so it’s faster.
decodeSfixed64Array :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array Int64)
-- | repeated packed __float__
decodeSfixed64Array len = label "decodeSfixed64Array / " do
  when ((len `mod` 8) /= 0) $ fail "Cannot consume a byteLength indivisible by 8."
  dv <- Parse.takeN len
  pure $ unsafeCopyInt64le (mkFn2 Int64.fromLowHighBits) dv (len `div` 8)

-- | __bool__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeBool :: forall m. MonadEffect m => ParserT DataView m Boolean
decodeBool = do
  x <- decodeVarint64
  if UInt64.lowBits x == 0 && UInt64.highBits x == 0 then
    pure false
  else
    pure true

-- | __string__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeString :: forall m. MonadEffect m => ParserT DataView m String
decodeString = do
  stringlen <- decodeVarint32
  stringview <-
    (Parse.takeN $ UInt.toInt stringlen)
    <|>
    (defer \_ -> fail $ "decodeString expected string of length " <> show stringlen)
  stringarray <- lift $ liftEffect $ toUint8Array stringview
  result <- lift $ liftEffect $ catchException
    (\error -> pure $ Left $ message error)
    (Right <$> TextDecoder.decode stringarray textDecoder)
  case result of
    Left msg -> defer \_ -> fail $ "decodeString decode failed " <> msg
    Right x -> pure x

textDecoder :: TextDecoder.TextDecoder
textDecoder = unsafePerformEffect $ TextDecoder.new UtfLabel.utf8

-- | __bytes__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeBytes :: forall m. MonadEffect m => ParserT DataView m Bytes
decodeBytes = do
  len <- UInt.toInt <$> decodeVarint32
  dv <- Parse.takeN len
  pure $ Bytes $ View dv

-- | https://stackoverflow.com/questions/2210923/zig-zag-decoding
decodeZigzag32 :: UInt -> Int
decodeZigzag32 n = UInt.toInt $ (n `UInt.zshr` (UInt.fromInt 1)) `UInt.xor` (unegate (n `UInt.and` (UInt.fromInt 1)))
  where
  unegate :: UInt -> UInt
  unegate = UInt.fromInt <<< negate <<< UInt.toInt

-- | Parse the field number and wire type of the next field.
-- | https://developers.google.com/protocol-buffers/docs/encoding#structure
decodeTag32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber WireType)
decodeTag32 = do
  n <- decodeVarint32
  let
    wireTypeInt = UInt.toInt $ n `UInt.and` (UInt.fromInt 7)
  case toEnum wireTypeInt of
    Just wireType -> pure $ Tuple (n `UInt.shr` (UInt.fromInt 3)) wireType
    Nothing -> fail $ "Unknown WireType " <> show wireTypeInt

-- | There is no `varint32` in the Protbuf spec, this is
-- | just a performance-improving assumption we make
-- | in cases where we would be surprised to see a number
-- | larger than 32 bits, such as in field numbers.
-- | We think this is worth the risk because `UInt` is
-- | represented as a native Javascript Number whereas
-- | `UInt64` is a composite library type, so we expect the
-- | performance difference to be significant.
-- |
-- | https://developers.google.com/protocol-buffers/docs/encoding#varints
decodeVarint32 :: forall m. MonadEffect m => ParserT DataView m UInt
decodeVarint32 = do
  n_0 <- Parse.anyUint8
  if n_0 < u0x80 then
    pure n_0
  else do
    let
      acc_0 = n_0 `UInt.and` u0x7F
    n_1 <- Parse.anyUint8
    if n_1 < u0x80 then
      pure $ acc_0 `UInt.or` (n_1 `UInt.shl` u7)
    else do
      let
        acc_1 = ((n_1 `UInt.and` u0x7F) `UInt.shl` u7) `UInt.or` acc_0
      n_2 <- Parse.anyUint8
      if n_2 < u0x80 then
        pure $ acc_1 `UInt.or` (n_2 `UInt.shl` u14)
      else do
        let
          acc_2 = ((n_2 `UInt.and` u0x7F) `UInt.shl` u14) `UInt.or` acc_1
        n_3 <- Parse.anyUint8
        if n_3 < u0x80 then
          pure $ acc_2 `UInt.or` (n_3 `UInt.shl` u21)
        else do
          let
            acc_3 = ((n_3 `UInt.and` u0x7F) `UInt.shl` u21) `UInt.or` acc_2
          n_4 <- Parse.anyUint8
          -- if n_4 < u0x10 -- This is the correct logic, but doesn't pass conformance?
          if n_4 < u0x80 then
            pure $ acc_3 `UInt.or` (n_4 `UInt.shl` u28)
          else
            fail "varint32 overflow. This varint was expected to fit in 32 bits."
  where
  u7 = UInt.fromInt 7

  u14 = UInt.fromInt 14

  u21 = UInt.fromInt 21

  u28 = UInt.fromInt 28

  u0x7F = UInt.fromInt 0x7F

  u0x80 = UInt.fromInt 0x80

-- | https://stackoverflow.com/questions/2210923/zig-zag-decoding
decodeZigzag64 :: UInt64 -> Int64
decodeZigzag64 n = let n' = Int64.toSigned n in (n' `Int64.zshr` one) `Int64.xor` (lnegate (n' `Int64.and` one))
  where
  lnegate x = Int64.complement x + one

-- | https://developers.google.com/protocol-buffers/docs/encoding#varints
decodeVarint64 :: forall m. MonadEffect m => ParserT DataView m UInt64
decodeVarint64 = do
  n_0 <- fromInt <$> Parse.anyUint8
  if n_0 < u0x80 then
    pure n_0
  else do
    let
      acc_0 = n_0 `UInt64.and` u0x7F
    n_1 <- fromInt <$> Parse.anyUint8
    if n_1 < u0x80 then
      pure $ acc_0 `UInt64.or` (n_1 `UInt64.shl` u7)
    else do
      let
        acc_1 = ((n_1 `UInt64.and` u0x7F) `UInt64.shl` u7) `UInt64.or` acc_0
      n_2 <- fromInt <$> Parse.anyUint8
      if n_2 < u0x80 then
        pure $ acc_1 `UInt64.or` (n_2 `UInt64.shl` u14)
      else do
        let
          acc_2 = ((n_2 `UInt64.and` u0x7F) `UInt64.shl` u14) `UInt64.or` acc_1
        n_3 <- fromInt <$> Parse.anyUint8
        if n_3 < u0x80 then
          pure $ acc_2 `UInt64.or` (n_3 `UInt64.shl` u21)
        else do
          let
            acc_3 = ((n_3 `UInt64.and` u0x7F) `UInt64.shl` u21) `UInt64.or` acc_2
          n_4 <- fromInt <$> Parse.anyUint8
          if n_4 < u0x80 then
            pure $ acc_3 `UInt64.or` (n_4 `UInt64.shl` u28)
          else do
            let
              acc_4 = ((n_4 `UInt64.and` u0x7F) `UInt64.shl` u28) `UInt64.or` acc_3
            n_5 <- fromInt <$> Parse.anyUint8
            if n_5 < u0x80 then
              pure $ acc_4 `UInt64.or` (n_5 `UInt64.shl` u35)
            else do
              let
                acc_5 = ((n_5 `UInt64.and` u0x7F) `UInt64.shl` u35) `UInt64.or` acc_4
              n_6 <- fromInt <$> Parse.anyUint8
              if n_6 < u0x80 then
                pure $ acc_5 `UInt64.or` (n_6 `UInt64.shl` u42)
              else do
                let
                  acc_6 = ((n_6 `UInt64.and` u0x7F) `UInt64.shl` u42) `UInt64.or` acc_5
                n_7 <- fromInt <$> Parse.anyUint8
                if n_7 < u0x80 then
                  pure $ acc_6 `UInt64.or` (n_7 `UInt64.shl` u49)
                else do
                  let
                    acc_7 = ((n_7 `UInt64.and` u0x7F) `UInt64.shl` u49) `UInt64.or` acc_6
                  n_8 <- fromInt <$> Parse.anyUint8
                  if n_8 < u0x80 then
                    pure $ acc_7 `UInt64.or` (n_8 `UInt64.shl` u56)
                  else do
                    let
                      acc_8 = ((n_8 `UInt64.and` u0x7F) `UInt64.shl` u56) `UInt64.or` acc_7
                    n_9 <- fromInt <$> Parse.anyUint8
                    if n_9 < u0x02 then
                      pure $ acc_8 `UInt64.or` (n_9 `UInt64.shl` u63)
                    else
                      fail "varint64 overflow. Possibly there is an encoding error in the input stream."
  where
  fromInt :: UInt -> UInt64
  fromInt = Int64.toUnsigned <<< Int64.fromInt <<< UInt.toInt

  u7 = UInt64.unsafeFromInt 7 :: UInt64

  u14 = UInt64.unsafeFromInt 14 :: UInt64

  u21 = UInt64.unsafeFromInt 21 :: UInt64

  u28 = UInt64.unsafeFromInt 28 :: UInt64

  u35 = UInt64.unsafeFromInt 35 :: UInt64

  u42 = UInt64.unsafeFromInt 42 :: UInt64

  u49 = UInt64.unsafeFromInt 49 :: UInt64

  u56 = UInt64.unsafeFromInt 56 :: UInt64

  u63 = UInt64.unsafeFromInt 63 :: UInt64

  u0x02 = UInt64.unsafeFromInt 2 :: UInt64

  u0x7F = UInt64.unsafeFromInt 0x7F :: UInt64

  u0x80 = UInt64.unsafeFromInt 0x80 :: UInt64
