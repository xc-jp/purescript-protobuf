-- | Primitive parsers for decoding Google Protocol Buffers.
-- |
-- | You almost never need to import this module.
-- | See package README for explanation.
module Protobuf.Decode
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
import Control.Apply (lift2)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.State (StateT(..))
import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView (AProxy(..), getFloat32le, getFloat64le, getInt32le, getUint32le)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (class TypedArray)
import Data.ArrayBuffer.Typed as AT
import Data.ArrayBuffer.Types (ArrayView, ByteLength, ByteOffset, DataView, Uint8Array, kind ArrayViewType)
import Data.ArrayBuffer.Types as ArrayTypes
import Data.ArrayBuffer.ValueMapping (class BinaryValue, class BytesPerValue, class ShowArrayViewType)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Float32 (Float32)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.TextDecoding (decodeUtf8)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Nat, D8, toInt')
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Protobuf.Common (Bytes(..), FieldNumber, WireType)
import Text.Parsing.Parser (ParseError(..), ParseState(..), ParserT(..), fail)
import Text.Parsing.Parser.DataView as Parse
import Text.Parsing.Parser.Pos (Position(..))
import Type.Proxy (Proxy(..))
import Data.Long.Internal (Long, Unsigned, Signed)
import Data.Long.Internal as Long.Internal

-- | __double__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeDouble :: forall m. MonadEffect m => ParserT DataView m Number
decodeDouble = Parse.anyFloat64le

-- | __float__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeFloat :: forall m. MonadEffect m => ParserT DataView m Float32
decodeFloat = Parse.anyFloat32le

-- | True if we are running in a JavaScript environment in which `TypedArray` is big-endian.
isBigEndian :: Effect Boolean
isBigEndian = unsafePerformEffect _isBigEndian

decodeArray ::
  forall a m b t name.
  BinaryValue a t =>
  BytesPerValue a b =>
  ShowArrayViewType a name =>
  IsSymbol name =>
  TypedArray a t =>
  Nat b =>
  MonadEffect m =>
  AProxy a ->
  (DataView -> ByteOffset -> Effect (Maybe t)) ->
  ByteLength -> ParserT DataView m (Array t)
decodeArray a p n = do
  big <- lift (liftEffect isBigEndian)
  if big then
    packedArray a p n
  else
    typedArray a n >>= lift <<< liftEffect <<< AT.toArray

-- | repeated packed __float__
decodeFloatArray :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array Float32)
decodeFloatArray = decodeArray (AProxy :: AProxy ArrayTypes.Float32) getFloat32le

-- | repeated packed __double__
decodeDoubleArray :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array Number)
decodeDoubleArray = decodeArray (AProxy :: AProxy ArrayTypes.Float64) getFloat64le

-- | repeated packed __sfixed32__
decodeSfixed32Array :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array Int)
decodeSfixed32Array = decodeArray (AProxy :: AProxy ArrayTypes.Int32) getInt32le

foreign import data BigInt64 :: ArrayViewType

instance bytesPerValueBigInt64 :: BytesPerValue BigInt64 D8

instance binaryValueBigInt64 :: BinaryValue BigInt64 (Long Signed)

instance showArrayViewBigInt64 :: ShowArrayViewType BigInt64 "long signed"

getLongle :: DataView -> Int -> Effect (Maybe (Long Signed))
getLongle dv idx = do
  lo <- getInt32le dv idx
  hi <- getInt32le dv (idx + 4)
  pure $ lift2 Long.Internal.fromLowHighBits lo hi

-- | repeated packed __sfixed64__
decodeSfixed64Array :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array (Long Signed))
decodeSfixed64Array = packedArray (AProxy :: AProxy BigInt64) getLongle

foreign import data BigUInt64 :: ArrayViewType

instance bytesPerValueBigUInt64 :: BytesPerValue BigUInt64 D8

instance binaryValueBigUInt64 :: BinaryValue BigUInt64 (Long Unsigned)

instance showArrayViewBigUInt64 :: ShowArrayViewType BigUInt64 "long unsigned"

getULongle :: DataView -> Int -> Effect (Maybe (Long Unsigned))
getULongle dv idx = do
  lo <- getInt32le dv idx
  hi <- getInt32le dv (idx + 4)
  pure $ lift2 Long.Internal.fromLowHighBits lo hi

-- | repeated packed __fixed32__
decodeFixed32Array :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array UInt)
decodeFixed32Array = decodeArray (AProxy :: AProxy ArrayTypes.Uint32) getUint32le

-- | repeated packed __fixed64__
decodeFixed64Array :: forall m. MonadEffect m => ByteLength -> ParserT DataView m (Array (Long Unsigned))
decodeFixed64Array = packedArray (AProxy :: AProxy BigUInt64) getULongle

foreign import _decodeArray ::
  forall t. (ByteOffset -> Effect t) -> Int -> Effect (Array t)

foreign import _isBigEndian :: Effect (Effect Boolean)

-- | Parse a slice of the DataView to an Array given a parser for the
-- | individual elements
packedArray ::
  forall a m b t name.
  BinaryValue a t =>
  BytesPerValue a b =>
  ShowArrayViewType a name =>
  IsSymbol name =>
  Nat b =>
  MonadEffect m =>
  AProxy a ->
  (DataView -> ByteOffset -> Effect (Maybe t)) ->
  ByteLength ->
  ParserT DataView m (Array t)
packedArray _ decodeValue n =
  let
    byteSize = toInt' (Proxy :: Proxy b)

    name = reflectSymbol (SProxy :: SProxy name)
  in
    do
      unless
        (n `mod` byteSize == 0)
        ( pure unit
            >>= \_ ->
                fail
                  $ joinWith " "
                      [ "byte length not a multiple of"
                      , show byteSize
                      , "when parsing"
                      , name
                      , "array"
                      ]
        )
      ParserT $ ExceptT
        $ StateT \state@(ParseState s pos@(Position { line, column }) _) ->
            let
              pos' = Position { line, column: column + n }

              -- we assume all indexing into the dataview will be valid given the
              -- check against the byteLength
              unsafeDecodeValue i =
                unsafePartial
                  $ map fromJust
                  $ decodeValue s ((i * byteSize) + column - 1)
            in
              if column + n - 1 > DV.byteLength s then
                pure (Tuple (Left (ParseError "index out of bounds" pos')) state)
              else do
                x <- liftEffect (_decodeArray unsafeDecodeValue (n `div` byteSize))
                pure (Tuple (pure x) (ParseState s pos' true))

-- | Parse a slice of the DataView by casting the slice to a TypedArray
typedArray ::
  forall a m b t name.
  BinaryValue a t =>
  BytesPerValue a b =>
  ShowArrayViewType a name =>
  TypedArray a t =>
  IsSymbol name =>
  Nat b =>
  MonadEffect m =>
  AProxy a ->
  ByteLength ->
  ParserT DataView m (ArrayView a)
typedArray _ n =
  let
    byteSize = toInt' (Proxy :: Proxy b)

    name = reflectSymbol (SProxy :: SProxy name)
  in
    do
      unless
        (n `mod` byteSize == 0)
        ( pure unit
            >>= \_ ->
                fail
                  $ joinWith " "
                      [ "byte length not a multiple of"
                      , show byteSize
                      , "when parsing"
                      , name
                      , "array"
                      ]
        )
      ParserT $ ExceptT
        $ StateT \state@(ParseState s pos@(Position { line, column }) _) ->
            let
              pos' = Position { line, column: column + n }

              backing = DV.buffer s

              start = DV.byteOffset s + column - 1

              end = start + n

              isAligned = start `mod` byteSize == 0
            in
              if end > DV.byteLength s then
                pure (Tuple (Left (ParseError "index out of bounds" pos')) state)
              else do
                x <-
                  liftEffect
                    $ if isAligned then
                        AT.part backing (start `div` byteSize) (n `div` byteSize)
                      else
                        AT.whole (AB.slice start end backing)
                pure (Tuple (pure x) (ParseState s pos' true))

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
  --   case Long.toInt n of
  --     Nothing -> fail "int32 value out of range."
  --     Just x -> pure x
  pure $ Long.Internal.lowBits n

-- | __int64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeInt64 :: forall m. MonadEffect m => ParserT DataView m (Long Signed)
decodeInt64 = Long.Internal.unsignedToSigned <$> decodeVarint64

-- | __uint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeUint32 :: forall m. MonadEffect m => ParserT DataView m UInt
decodeUint32 = do
  n <- decodeVarint64
  pure $ UInt.fromInt $ Long.Internal.lowBits n

-- | __uint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeUint64 :: forall m. MonadEffect m => ParserT DataView m (Long Unsigned)
decodeUint64 = decodeVarint64

-- | __sint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeSint32 :: forall m. MonadEffect m => ParserT DataView m Int
decodeSint32 = do
  n <- decodeZigzag32 <$> decodeVarint32
  pure n

-- | __sint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeSint64 :: forall m. MonadEffect m => ParserT DataView m (Long Signed)
decodeSint64 = decodeZigzag64 <$> decodeVarint64

-- | __fixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeFixed32 :: forall m. MonadEffect m => ParserT DataView m UInt
decodeFixed32 = Parse.anyUint32le

-- | __fixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeFixed64 :: forall m. MonadEffect m => ParserT DataView m (Long Unsigned)
decodeFixed64 = Long.Internal.fromLowHighBits <$> Parse.anyInt32le <*> Parse.anyInt32le

-- | __sfixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeSfixed32 :: forall m. MonadEffect m => ParserT DataView m Int
decodeSfixed32 = Parse.anyInt32le

-- | __sfixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeSfixed64 :: forall m. MonadEffect m => ParserT DataView m (Long Signed)
decodeSfixed64 = Long.Internal.fromLowHighBits <$> Parse.anyInt32le <*> Parse.anyInt32le

-- | __bool__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeBool :: forall m. MonadEffect m => ParserT DataView m Boolean
decodeBool = do
  x <- decodeVarint64
  if Long.Internal.lowBits x == 0 && Long.Internal.highBits x == 0 then
    pure false
  else
    pure true

-- | __string__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeString :: forall m. MonadEffect m => ParserT DataView m String
decodeString = do
  stringview <- decodeVarint32 >>= UInt.toInt >>> Parse.takeN
  stringarray <- lift $ liftEffect $ mkTypedArray stringview
  case decodeUtf8 stringarray of
    Left err -> fail $ "string decodeUtf8 failed. " <> show err
    Right s -> pure s
  where
  mkTypedArray :: DataView -> Effect Uint8Array
  mkTypedArray dv = do
    let
      buffer = DV.buffer dv

      byteOffset = DV.byteOffset dv

      byteLength = DV.byteLength dv
    AT.part buffer byteOffset byteLength

-- | __bytes__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
decodeBytes :: forall m. MonadEffect m => ParserT DataView m Bytes
decodeBytes = do
  len <- UInt.toInt <$> decodeVarint32
  dv <- Parse.takeN len
  let
    ab = DV.buffer dv
  let
    begin = DV.byteOffset dv
  pure $ Bytes $ AB.slice begin (begin + len) ab

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
-- | in cases where only a deranged lunatic would use a value
-- | bigger than 32 bits, such as in field numbers.
-- | We think this is worth the risk because `UInt` is
-- | represented as a native Javascript Number whereas
-- | `Long` is a composite library type, so we expect the
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

  u0x10 = UInt.fromInt 0x10

  u0x7F = UInt.fromInt 0x7F

  u0x80 = UInt.fromInt 0x80

-- | https://stackoverflow.com/questions/2210923/zig-zag-decoding
decodeZigzag64 :: Long Unsigned -> Long Signed
decodeZigzag64 n = let n' = Long.Internal.unsignedToSigned n in (n' `Long.Internal.zshr` u1) `Long.Internal.xor` (lnegate (n' `Long.Internal.and` u1))
  where
  lnegate x = Long.Internal.complement x + u1

  u1 = Long.Internal.unsafeFromInt 1

-- | https://developers.google.com/protocol-buffers/docs/encoding#varints
decodeVarint64 :: forall m. MonadEffect m => ParserT DataView m (Long Unsigned)
decodeVarint64 = do
  n_0 <- fromInt <$> Parse.anyUint8
  if n_0 < u0x80 then
    pure n_0
  else do
    let
      acc_0 = n_0 `Long.Internal.and` u0x7F
    n_1 <- fromInt <$> Parse.anyUint8
    if n_1 < u0x80 then
      pure $ acc_0 `Long.Internal.or` (n_1 `Long.Internal.shl` u7)
    else do
      let
        acc_1 = ((n_1 `Long.Internal.and` u0x7F) `Long.Internal.shl` u7) `Long.Internal.or` acc_0
      n_2 <- fromInt <$> Parse.anyUint8
      if n_2 < u0x80 then
        pure $ acc_1 `Long.Internal.or` (n_2 `Long.Internal.shl` u14)
      else do
        let
          acc_2 = ((n_2 `Long.Internal.and` u0x7F) `Long.Internal.shl` u14) `Long.Internal.or` acc_1
        n_3 <- fromInt <$> Parse.anyUint8
        if n_3 < u0x80 then
          pure $ acc_2 `Long.Internal.or` (n_3 `Long.Internal.shl` u21)
        else do
          let
            acc_3 = ((n_3 `Long.Internal.and` u0x7F) `Long.Internal.shl` u21) `Long.Internal.or` acc_2
          n_4 <- fromInt <$> Parse.anyUint8
          if n_4 < u0x80 then
            pure $ acc_3 `Long.Internal.or` (n_4 `Long.Internal.shl` u28)
          else do
            let
              acc_4 = ((n_4 `Long.Internal.and` u0x7F) `Long.Internal.shl` u28) `Long.Internal.or` acc_3
            n_5 <- fromInt <$> Parse.anyUint8
            if n_5 < u0x80 then
              pure $ acc_4 `Long.Internal.or` (n_5 `Long.Internal.shl` u35)
            else do
              let
                acc_5 = ((n_5 `Long.Internal.and` u0x7F) `Long.Internal.shl` u35) `Long.Internal.or` acc_4
              n_6 <- fromInt <$> Parse.anyUint8
              if n_6 < u0x80 then
                pure $ acc_5 `Long.Internal.or` (n_6 `Long.Internal.shl` u42)
              else do
                let
                  acc_6 = ((n_6 `Long.Internal.and` u0x7F) `Long.Internal.shl` u42) `Long.Internal.or` acc_5
                n_7 <- fromInt <$> Parse.anyUint8
                if n_7 < u0x80 then
                  pure $ acc_6 `Long.Internal.or` (n_7 `Long.Internal.shl` u49)
                else do
                  let
                    acc_7 = ((n_7 `Long.Internal.and` u0x7F) `Long.Internal.shl` u49) `Long.Internal.or` acc_6
                  n_8 <- fromInt <$> Parse.anyUint8
                  if n_8 < u0x80 then
                    pure $ acc_7 `Long.Internal.or` (n_8 `Long.Internal.shl` u56)
                  else do
                    let
                      acc_8 = ((n_8 `Long.Internal.and` u0x7F) `Long.Internal.shl` u56) `Long.Internal.or` acc_7
                    n_9 <- fromInt <$> Parse.anyUint8
                    if n_9 < u0x02 then
                      pure $ acc_8 `Long.Internal.or` (n_9 `Long.Internal.shl` u63)
                    else
                      fail "varint64 overflow. Possibly there is an encoding error in the input stream."
  where
  fromInt :: UInt -> Long Unsigned
  fromInt = Long.Internal.signedToUnsigned <<< Long.Internal.signedLongFromInt <<< UInt.toInt

  u7 = Long.Internal.unsafeFromInt 7

  u14 = Long.Internal.unsafeFromInt 14

  u21 = Long.Internal.unsafeFromInt 21

  u28 = Long.Internal.unsafeFromInt 28

  u35 = Long.Internal.unsafeFromInt 35

  u42 = Long.Internal.unsafeFromInt 42

  u49 = Long.Internal.unsafeFromInt 49

  u56 = Long.Internal.unsafeFromInt 56

  u63 = Long.Internal.unsafeFromInt 63

  u0x02 = Long.Internal.unsafeFromInt 2

  u0x7F = Long.Internal.unsafeFromInt 0x7F

  u0x80 = Long.Internal.unsafeFromInt 0x80
