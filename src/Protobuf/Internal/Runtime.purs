-- | This module is for import by the generated .purs message modules.
module Protobuf.Internal.Runtime
  ( parseMessage
  , UnknownField(..)
  , parseFieldUnknown
  , putFieldUnknown
  , parseLenDel
  , FieldNumberInt
  , manyLength
  , putLenDel
  , putOptional
  , putRepeated
  , putPacked
  , putEnumField
  , putEnum
  , parseEnum
  , mergeWith
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.Trans.Class (lift)
import Data.Array (snoc)
import Data.ArrayBuffer.Builder (DataBuff(..), PutM, subBuilder)
import Data.ArrayBuffer.Types (DataView, ByteLength)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (foldl, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Int64 as Int64
import Data.UInt64 (UInt64)
import Data.UInt64 as UInt64
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.UInt as UInt
import Effect.Class (class MonadEffect)
import Protobuf.Internal.Common (Bytes(..), FieldNumber, WireType(..), label)
import Protobuf.Internal.Decode as Decode
import Protobuf.Internal.Encode as Encode
import Record.Builder (build, modify)
import Record.Builder as RecordB
import Parsing (ParserT, Position(..), fail, position)
import Parsing.DataView (takeN)
import Type.Proxy(Proxy(..))

-- | The parseField argument is a parser which returns a Record builder which,
-- | when applied to a Record, will modify the Record to add the parsed field.
parseMessage ::
  forall m a r.
  MonadEffect m =>
  MonadRec m =>
  (Record r -> a) ->
  (Record r) ->
  (FieldNumberInt -> WireType -> ParserT DataView m (RecordB.Builder (Record r) (Record r))) ->
  ByteLength ->
  ParserT DataView m a
parseMessage construct default parseField length = do
  builders <- manyLength applyParser length
  pure $ construct $ build (foldl (>>>) identity builders) default
  where
  applyParser = do
    Tuple fieldNumber wireType <- Decode.decodeTag32
    if fieldNumber == UInt.fromInt 0 then
      fail "Field number 0 not allowed." -- Conformance tests require this
    else
      parseField (UInt.toInt fieldNumber) wireType

-- | We want an Int FieldNumber to pass to parseField so that we can pattern
-- | match on Int literals. UInt doesn't export any constructors, so we can’t
-- | pattern match on it.
type FieldNumberInt
  = Int

-- | Call a parser repeatedly until exactly *N* bytes have been consumed.
-- | Will fail if too many bytes are consumed.
manyLength ::
  forall m a.
  MonadEffect m =>
  MonadRec m =>
  ParserT DataView m a ->
  ByteLength ->
  ParserT DataView m (Array a)
manyLength p len = do
  Position { index: posBegin' } <- position
  begin posBegin'
  pure mutablearray
  where
  mutablearray = [] :: Array a

  begin :: Int -> ParserT DataView m Unit
  begin posBegin = do
    tailRecM go unit
    where
    go :: Unit -> ParserT DataView m (Step Unit Unit)
    go _ = do
      Position { index: pos } <- position
      case compare (pos - posBegin) len of
        GT -> fail "manyLength consumed too many bytes."
        EQ -> lift $ pure (Done unit)
        LT -> do
          x <- p
          _ <- pure $ unsafeArrayPush mutablearray [ x ]
          lift $ pure (Loop unit)

-- | We are just going to exploit the high-performance Array push behavior
-- | of V8 here.
-- |
-- | Forego all of the guarantees of the type system and mutate
-- | The first array by concatenating the second array with Javascript
-- | [`push`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push).
-- | Returns the length of the mutated array.
-- |
-- | With Purescript's strict semantics, we can probably get away
-- | with this?
foreign import unsafeArrayPush :: forall a. Array a -> Array a -> Int

-- | A message field value from an unknown `.proto` definition.
-- |
-- | See [Message Structure](https://developers.google.com/protocol-buffers/docs/encoding#structure)
-- | for an explanation.
-- |
-- | - __`UnknownVarInt`__ Use `Protobuf.Internal.Decode.decodeZigzag64` to
-- |   to interpret this as a signed integer.
-- | - __`UnknownLenDel`__ holds a variable-length `Bytes`.
-- | - __`UnknownBits64`__ must hold `Bytes` of length 8.
-- | - __`UnknownBits32`__ must hold `Bytes` of length 4.
-- |
-- | See the modules __Protobuf.Internal.Encode__
-- | and __Protobuf.Internal.Decode__ for ways to operate on the `Bytes`.
data UnknownField
  = UnknownVarInt FieldNumber UInt64
  | UnknownBits64 FieldNumber Bytes
  | UnknownLenDel FieldNumber Bytes
  | UnknownBits32 FieldNumber Bytes

derive instance eqUnknownField :: Eq UnknownField

derive instance genericUnknownField :: Generic UnknownField _

instance showUnknownField :: Show UnknownField where
  show = genericShow

-- | Parse and preserve an unknown field.
parseFieldUnknown ::
  forall m r.
  MonadEffect m =>
  Int ->
  WireType ->
  ParserT DataView m (RecordB.Builder (Record ( "__unknown_fields" :: Array UnknownField | r )) (Record ( "__unknown_fields" :: Array UnknownField | r )))
parseFieldUnknown fieldNumberInt wireType =
  label ("Unknown " <> show wireType <> " " <> show fieldNumber <> " / ")
    $ case wireType of
        VarInt -> do
          x <- Decode.decodeUint64
          pure $ modify (Proxy :: Proxy "__unknown_fields")
            $ flip snoc
            $ UnknownVarInt fieldNumber x
        Bits64 -> do
          x <- takeN 8
          pure $ modify (Proxy :: Proxy "__unknown_fields")
            $ flip snoc
            $ UnknownBits64 fieldNumber $ Bytes $ View x
        LenDel -> do
          len <- UInt64.toInt <$> Decode.decodeVarint64
          case len of
            Nothing -> fail $ "Length-delimited value of unknown field " <> show fieldNumber <> " was too long."
            Just l -> do
              dv <- takeN l
              pure $ modify (Proxy :: Proxy "__unknown_fields")
                $ flip snoc $ UnknownLenDel fieldNumber $ Bytes $ View dv
        Bits32 -> do
          x <- takeN 4
          pure $ modify (Proxy :: Proxy "__unknown_fields")
            $ flip snoc
            $ UnknownBits32 fieldNumber $ Bytes $ View x
  where
  fieldNumber = UInt.fromInt fieldNumberInt

putFieldUnknown ::
  forall m.
  MonadEffect m =>
  UnknownField ->
  PutM m Unit
putFieldUnknown (UnknownBits64 fieldNumber x) = Encode.encodeBytesField fieldNumber x

putFieldUnknown (UnknownVarInt fieldNumber x) = Encode.encodeUint64Field fieldNumber x

putFieldUnknown (UnknownLenDel fieldNumber x) = Encode.encodeBytesField fieldNumber x

putFieldUnknown (UnknownBits32 fieldNumber x) = Encode.encodeBytesField fieldNumber x

-- | Parse a length, then call a parser which takes one length as its argument.
parseLenDel ::
  forall m a.
  MonadEffect m =>
  (Int -> ParserT DataView m a) ->
  ParserT DataView m a
parseLenDel p = p <<< UInt.toInt =<< Decode.decodeVarint32

putLenDel ::
  forall m a.
  MonadEffect m =>
  (a -> PutM m Unit) ->
  FieldNumber -> a -> PutM m Unit
putLenDel p fieldNumber x = do
  b <- subBuilder $ p x
  Encode.encodeBuilder fieldNumber b

putOptional ::
  forall m a.
  MonadEffect m =>
  FieldNumberInt ->
  Maybe a ->
  (a -> Boolean) -> -- isDefault predicate. Put nothing if this is true.
  (FieldNumber -> a -> PutM m Unit) ->
  PutM m Unit
putOptional _ Nothing _ _ = pure unit

putOptional fieldNumber (Just x) isDefault encoder = do
  when (not $ isDefault x) $ encoder (UInt.fromInt fieldNumber) x

putRepeated ::
  forall m a.
  MonadEffect m =>
  FieldNumberInt ->
  Array a ->
  (FieldNumber -> a -> PutM m Unit) ->
  PutM m Unit
putRepeated fieldNumber xs encoder = flip traverse_ xs $ encoder $ UInt.fromInt fieldNumber

putPacked ::
  forall m a.
  MonadEffect m =>
  FieldNumberInt ->
  Array a ->
  (a -> PutM m Unit) ->
  PutM m Unit
putPacked _ [] _ = pure unit

putPacked fieldNumber xs encoder = do
  b <- subBuilder $ traverse_ encoder xs
  Encode.encodeBuilder (UInt.fromInt fieldNumber) b

putEnumField ::
  forall m a.
  MonadEffect m =>
  BoundedEnum a =>
  FieldNumber -> a -> PutM m Unit
putEnumField fieldNumber x = do
  Encode.encodeTag32 fieldNumber VarInt
  putEnum x

putEnum :: forall m a. MonadEffect m => BoundedEnum a => a -> PutM m Unit
putEnum x = Encode.encodeVarint64 (UInt64.fromLowHighBits x_low x_high :: UInt64)
  where
  x_int = fromEnum x

  x_slong = Int64.fromInt x_int

  x_high = Int64.highBits x_slong

  x_low = Int64.lowBits x_slong

parseEnum :: forall m a. MonadEffect m => BoundedEnum a => ParserT DataView m a
parseEnum = do
  -- “Enumerator constants must be in the range of a 32-bit integer.”
  -- Protobuf Enums can be negative.
  -- https://developers.google.com/protocol-buffers/docs/proto3#enum
  x <- Decode.decodeVarint64
  case toEnum (UInt64.lowBits x) of
    Nothing -> fail $ "Enum " <> show x <> " out of bounds."
    Just e -> pure e

-- | Merge the new left with the old right.
mergeWith :: forall a. (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mergeWith f (Just l) (Just r) = Just (f l r)

mergeWith _ l Nothing = l

mergeWith _ Nothing r = r
