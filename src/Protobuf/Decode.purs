-- | Primitive parsers for decoding Google Protocol Buffers.
-- |
-- | You almost never need to import this module.
-- | See package README for explanation.
module Protobuf.Decode
  ( double
  , doubleArray
  , float
  , floatArray
  , int32
  , int64
  , uint32
  , uint64
  , sint32
  , sint64
  , fixed32
  , fixed32Array
  , fixed64
  , fixed64Array
  , sfixed32
  , sfixed32Array
  , sfixed64
  , sfixed64Array
  , bool
  , string
  , bytes
  , module Protobuf.Decode32
  , module Protobuf.Decode64
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
import Data.Float32 (Float32)
import Data.Long.Internal (Long, Signed, Unsigned, fromLowHighBits, highBits, lowBits, unsignedToSigned)
import Data.Maybe (Maybe, fromJust)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.TextDecoding (decodeUtf8)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Nat, D8, toInt')
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Partial.Unsafe (unsafePartial)
import Protobuf.Common (Bytes(..))
import Protobuf.Decode32 (varint32, zigzag32, tag32)
import Protobuf.Decode64 (varint64, zigzag64)
import Text.Parsing.Parser (ParseError(..), ParseState(..), ParserT(..), fail)
import Text.Parsing.Parser.DataView as Parse
import Text.Parsing.Parser.Pos (Position(..))
import Type.Proxy (Proxy(..))

-- | __double__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
double :: forall m. MonadEffect m => ParserT DataView m Number
double = Parse.anyFloat64le

-- | __float__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
float :: forall m. MonadEffect m => ParserT DataView m Float32
float = Parse.anyFloat32le

-- | __repeated packed float__
floatArray :: forall m. MonadEffect m => Int -> ParserT DataView m (Array Float32)
floatArray = endianTypedArray (AProxy :: AProxy ArrayTypes.Float32) getFloat32le

endianTypedArray ::
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
  Int -> ParserT DataView m (Array t)
endianTypedArray a p n = do
  big <- lift (liftEffect _isBigEndian)
  if big then
    typedArray a p n
  else
    typedArray' a n >>= lift <<< liftEffect <<< AT.toArray

-- | __repeated packed double__
doubleArray :: forall m. MonadEffect m => Int -> ParserT DataView m (Array Number)
doubleArray = endianTypedArray (AProxy :: AProxy ArrayTypes.Float64) getFloat64le

-- | __repeated packed sfixed32__
sfixed32Array :: forall m. MonadEffect m => Int -> ParserT DataView m (Array Int)
sfixed32Array = endianTypedArray (AProxy :: AProxy ArrayTypes.Int32) getInt32le

foreign import data BigInt64 :: ArrayViewType

instance bytesPerValueBigInt64 :: BytesPerValue BigInt64 D8

instance binaryValueBigInt64 :: BinaryValue BigInt64 (Long Signed)

instance showArrayViewBigInt64 :: ShowArrayViewType BigInt64 "long signed"

getLongle :: DataView -> Int -> Effect (Maybe (Long Signed))
getLongle dv idx = do
  lo <- getInt32le dv idx
  hi <- getInt32le dv (idx + 4)
  pure $ lift2 fromLowHighBits lo hi

-- | __repeated packed sfixed64__
sfixed64Array :: forall m. MonadEffect m => Int -> ParserT DataView m (Array (Long Signed))
sfixed64Array = typedArray (AProxy :: AProxy BigInt64) getLongle

foreign import data BigUInt64 :: ArrayViewType

instance bytesPerValueBigUInt64 :: BytesPerValue BigUInt64 D8

instance binaryValueBigUInt64 :: BinaryValue BigUInt64 (Long Unsigned)

instance showArrayViewBigUInt64 :: ShowArrayViewType BigUInt64 "long unsigned"

getULongle :: DataView -> Int -> Effect (Maybe (Long Unsigned))
getULongle dv idx = do
  lo <- getInt32le dv idx
  hi <- getInt32le dv (idx + 4)
  pure $ lift2 fromLowHighBits lo hi

-- | __repeated packed fixed32__
fixed32Array :: forall m. MonadEffect m => Int -> ParserT DataView m (Array UInt)
fixed32Array = endianTypedArray (AProxy :: AProxy ArrayTypes.Uint32) getUint32le

-- | __repeated packed fixed64__
fixed64Array :: forall m. MonadEffect m => Int -> ParserT DataView m (Array (Long Unsigned))
fixed64Array = typedArray (AProxy :: AProxy BigUInt64) getULongle

foreign import _decodeArray ::
  forall t. (ByteOffset -> Effect t) -> Int -> Effect (Array t)

foreign import _isBigEndian :: Effect Boolean

-- | Parse a slice of the DataView to an Array given a parser for the
-- | individual elements
typedArray ::
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
typedArray _ decodeValue n =
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

-- | Parse a slice of the DataView to an Array given a parser for the
-- | individual elements
typedArray' ::
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
typedArray' _ n =
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

              start = column - 1

              isAligned = start `mod` byteSize == 0
            in
              if column + n - 1 > DV.byteLength s then
                pure (Tuple (Left (ParseError "index out of bounds" pos')) state)
              else do
                x <-
                  liftEffect
                    $ if isAligned then
                        AT.part backing (start `div` byteSize) (n `div` byteSize) :: Effect (ArrayView a)
                      else
                        AT.whole (AB.slice start (start + n) backing)
                pure (Tuple (pure x) (ParseState s pos' true))

-- | __int32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
int32 :: forall m. MonadEffect m => ParserT DataView m Int
int32 = do
  n <- varint64
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
  pure $ lowBits n

-- | __int64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
int64 :: forall m. MonadEffect m => ParserT DataView m (Long Signed)
int64 = unsignedToSigned <$> varint64

-- | __uint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint32 :: forall m. MonadEffect m => ParserT DataView m UInt
uint32 = do
  n <- varint64
  pure $ UInt.fromInt $ lowBits n

-- | __uint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint64 :: forall m. MonadEffect m => ParserT DataView m (Long Unsigned)
uint64 = varint64

-- | __sint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sint32 :: forall m. MonadEffect m => ParserT DataView m Int
sint32 = do
  n <- zigzag32 <$> varint32
  pure n

-- | __sint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sint64 :: forall m. MonadEffect m => ParserT DataView m (Long Signed)
sint64 = zigzag64 <$> varint64

-- | __fixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
fixed32 :: forall m. MonadEffect m => ParserT DataView m UInt
fixed32 = Parse.anyUint32le

-- | __fixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
fixed64 :: forall m. MonadEffect m => ParserT DataView m (Long Unsigned)
fixed64 = fromLowHighBits <$> Parse.anyInt32le <*> Parse.anyInt32le

-- | __sfixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sfixed32 :: forall m. MonadEffect m => ParserT DataView m Int
sfixed32 = Parse.anyInt32le

-- | __sfixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sfixed64 :: forall m. MonadEffect m => ParserT DataView m (Long Signed)
sfixed64 = fromLowHighBits <$> Parse.anyInt32le <*> Parse.anyInt32le

-- | __bool__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
bool :: forall m. MonadEffect m => ParserT DataView m Boolean
bool = do
  x <- varint64
  if lowBits x == 0 && highBits x == 0 then
    pure false
  else
    pure true

-- | __string__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
string :: forall m. MonadEffect m => ParserT DataView m String
string = do
  stringview <- varint32 >>= UInt.toInt >>> Parse.takeN
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
bytes :: forall m. MonadEffect m => ParserT DataView m Bytes
bytes = do
  len <- UInt.toInt <$> varint32
  dv <- Parse.takeN len
  let
    ab = DV.buffer dv
  let
    begin = DV.byteOffset dv
  pure $ Bytes $ AB.slice begin (begin + len) ab
