-- | This module is for import by the generated .purs message modules.
-- |
-- | Do not import this module.
-- | See package README for explanation.
module Protobuf.Runtime
( parseMessage
, UnknownField(..)
, parseFieldUnknown
, putFieldUnknown
, parseLenDel
, Pos
, FieldNumberInt
, positionZero
, manyLength
, putLenDel
, putOptional
, putRepeated
, putPacked
, putEnum
, putEnum'
, parseEnum
, label
, mergeWith
, class Mergeable
, mergeScalar
)
where

import Prelude

import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.Trans.Class (lift)
import Data.Array (snoc)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Builder (PutM, subBuilder)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (DataView, ByteLength)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (foldl, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Long.Internal (Long, Signed, Unsigned, fromLowHighBits, highBits, lowBits, signedLongFromInt)
import Data.Long.Unsigned as Long.Unsigned
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Class (class MonadEffect)
import Protobuf.Common (FieldNumber, WireType(..), Bytes(..))
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode
import Record.Builder (build, modify)
import Record.Builder as RecordB
import Text.Parsing.Parser (ParserT, fail, position, ParseError(..))
import Text.Parsing.Parser.DataView (takeN)
import Text.Parsing.Parser.Pos (Position(..))

-- | The parseField argument is a parser which returns a Record builder which,
-- | when applied to a Record, will modify the Record to add the parsed field.
parseMessage
  :: forall m a r
   . MonadEffect m
  => MonadRec m
  => (Record r -> a)
  -> (Record r)
  -> (FieldNumberInt -> WireType -> ParserT DataView m (RecordB.Builder (Record r) (Record r)))
  -> Int
  -> ParserT DataView m a
parseMessage construct default parseField length = do
  builders <- manyLength applyParser length
  pure $ construct $ build (foldl (>>>) identity builders) default
 where
  applyParser = do
    Tuple fieldNumber wireType <- Decode.tag32
    if fieldNumber == UInt.fromInt 0
      then fail "Field number 0 not allowed." -- Conformance tests require this
      else parseField (UInt.toInt fieldNumber) wireType

-- | Parse position, zero-based, unlike Text.Parsing.Parser.Position which is one-based.
type Pos = Int

-- | We want Int FieldNumber to pass to parseField so that we can pattern
-- | match on Int literals. UInt doesn't export any constructors, so can't
-- | pattern match on it.
type FieldNumberInt = Int

-- | Zero-based position in the parser.
positionZero :: forall s m. Monad m => ParserT s m Pos
positionZero = do
  Position {column,line} <- position
  pure $ column - 1

-- | Call a parser repeatedly until exactly *N* bytes have been consumed.
-- | Will fail if too many bytes are consumed.
manyLength
  :: forall m a
   . MonadEffect m
  => MonadRec m
  => ParserT DataView m a
  -> ByteLength
  -> ParserT DataView m (Array a)
manyLength p len = do
  posBegin' <- positionZero
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
      pos <- positionZero
      case compare (pos - posBegin) len of
        GT -> fail "manyLength consumed too many bytes."
        EQ -> lift $ pure (Done unit)
        LT -> do
          x <- p
          _ <- pure $ unsafeArrayPush mutablearray [x]
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


data UnknownField
  = UnknownVarInt FieldNumber (Long Unsigned)
  | UnknownBits64 FieldNumber (Long Unsigned)
  | UnknownLenDel FieldNumber Bytes
  | UnknownBits32 FieldNumber UInt

derive instance eqUnknownField :: Eq UnknownField
derive instance genericUnknownField :: Generic UnknownField _
instance showUnknownField :: Show UnknownField where show = genericShow

-- | Parse and preserve an unknown field.
parseFieldUnknown
  :: forall m r
   . MonadEffect m
  => Int
  -> WireType
  -> ParserT DataView m (RecordB.Builder (Record ("__unknown_fields" :: Array UnknownField | r)) (Record ("__unknown_fields" :: Array UnknownField | r)))
parseFieldUnknown fieldNumberInt wireType = label ("Unknown " <> show wireType <> " " <> show fieldNumber <> " / ") $
  case wireType of
    VarInt -> do
      x <- Decode.uint64
      pure $ modify (SProxy :: SProxy "__unknown_fields") $
        flip snoc $ UnknownVarInt fieldNumber x
    Bits64 -> do
      x <- Decode.fixed64
      pure $ modify (SProxy :: SProxy "__unknown_fields") $
        flip snoc $ UnknownBits64 fieldNumber x
    LenDel -> do
      len <- Long.Unsigned.toInt <$> Decode.varint64
      case len of
        Nothing -> fail $ "Length-delimited value of unknown field " <> show fieldNumber <> " was too long."
        Just l -> do
          dv <- takeN l
          pure $ modify (SProxy :: SProxy "__unknown_fields") $
            flip snoc $ UnknownLenDel fieldNumber $ Bytes $
              AB.slice (DV.byteOffset dv) (DV.byteLength dv) (DV.buffer dv)
    Bits32 -> do
      x <- Decode.fixed32
      pure $ modify (SProxy :: SProxy "__unknown_fields") $
        flip snoc $ UnknownBits32 fieldNumber x
 where
  fieldNumber = UInt.fromInt fieldNumberInt

putFieldUnknown
  :: forall m
   . MonadEffect m
  => UnknownField
  -> PutM m Unit
putFieldUnknown (UnknownBits64 fieldNumber x) = Encode.fixed64 fieldNumber x
putFieldUnknown (UnknownVarInt fieldNumber x) = Encode.uint64 fieldNumber x
putFieldUnknown (UnknownLenDel fieldNumber x) = Encode.bytes fieldNumber x
putFieldUnknown (UnknownBits32 fieldNumber x) = Encode.fixed32 fieldNumber x

-- | Parse a length, then call a parser which takes one length as its argument.
parseLenDel
  :: forall m a
   . MonadEffect m
  => (Int -> ParserT DataView m a)
  -> ParserT DataView m a
parseLenDel p = p <<< UInt.toInt =<< Decode.varint32

putLenDel
  :: forall m a
   . MonadEffect m
  => (a -> PutM m Unit)
  -> FieldNumber -> a -> PutM m Unit
putLenDel p fieldNumber x = do
  b <- subBuilder $ p x
  Encode.builder fieldNumber b

putOptional
  :: forall m a
   . MonadEffect m
  => FieldNumberInt
  -> Maybe a
  -> (a -> Boolean) -- isDefault predicate. Put nothing if this is true.
  -> (FieldNumber -> a -> PutM m Unit)
  -> PutM m Unit
putOptional _ Nothing _ _ = pure unit
putOptional fieldNumber (Just x) isDefault encoder = do
  when (not $ isDefault x) $ encoder (UInt.fromInt fieldNumber) x

putRepeated
  :: forall m a
   . MonadEffect m
  => FieldNumberInt
  -> Array a
  -> (FieldNumber -> a -> PutM m Unit)
  -> PutM m Unit
putRepeated fieldNumber xs encoder = flip traverse_ xs $ encoder $ UInt.fromInt fieldNumber

putPacked
  :: forall m a
   . MonadEffect m
  => FieldNumberInt
  -> Array a
  -> (a -> PutM m Unit)
  -> PutM m Unit
putPacked _ [] _ = pure unit
putPacked fieldNumber xs encoder = do
  b <- subBuilder $ traverse_ encoder xs
  Encode.builder (UInt.fromInt fieldNumber) b

putEnum
  :: forall m a
   . MonadEffect m
  => BoundedEnum a
  => FieldNumber -> a -> PutM m Unit
putEnum fieldNumber x = do
  Encode.tag32 fieldNumber VarInt
  putEnum' x

putEnum' :: forall m a. MonadEffect m => BoundedEnum a => a -> PutM m Unit
putEnum' x = Encode.varint64 (fromLowHighBits x_low x_high :: Long Unsigned)
 where
  x_int = fromEnum x
  x_slong = signedLongFromInt x_int :: Long Signed
  x_high = highBits x_slong
  x_low = lowBits x_slong


parseEnum :: forall m a. MonadEffect m => BoundedEnum a => ParserT DataView m a
parseEnum = do
  -- “Enumerator constants must be in the range of a 32-bit integer.”
  -- Protobuf Enums can be negative.
  -- https://developers.google.com/protocol-buffers/docs/proto3#enum
  x <- Decode.varint64
  case toEnum (Long.Unsigned.lowBits x) of
    Nothing -> fail $ "Enum " <> show x <> " out of bounds."
    Just e -> pure e

-- | If parsing fails inside this labelled context, then prepend the `String`
-- | to the error `String` in the `ParseError`. Use this to establish
-- | context for parsing failure error messages.
label :: forall m s a. Monad m => String -> ParserT s m a -> ParserT s m a
label messagePrefix p = catchError p $ \ (ParseError message pos) ->
  throwError $ ParseError (messagePrefix <> message) pos

mergeWith :: forall a. (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mergeWith f (Just l) (Just r) = Just (f l r)
mergeWith _ l Nothing = l
mergeWith _ Nothing r = r
mergeWith _ _ _ = Nothing

-- | This is not quite the `Alt` class, because we need to flip the list append for `alt`.
class Mergeable a
 where
  -- | Merge the new left with the old right.
  mergeScalar :: a -> a -> a

instance mergableMaybe :: Mergeable (Maybe a)
 where
  mergeScalar (Just l) _ = Just l
  mergeScalar _ (Just r) = Just r
  mergeScalar _ _ = Nothing

instance mergableArray :: Mergeable (Array a)
 where
  mergeScalar ls rs = rs <> ls