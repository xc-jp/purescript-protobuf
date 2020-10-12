-- | This module is for import by the generated .purs message modules.
-- |
-- | Do not import this module.
-- | See package README for explanation.
module Protobuf.Runtime
( parseMessage
, UnknownField
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
)
where

import Prelude

import Control.Monad.Error.Class (throwError, catchError)
import Data.Array (fromFoldable, snoc)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Builder (PutM, subBuilder)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (DataView)
import Data.Enum (class BoundedEnum, toEnum, fromEnum)
import Data.Foldable (foldl, traverse_)
import Data.List (List(..), (:))
import Data.Long.Unsigned (Long, toInt)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Class (class MonadEffect)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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
  => ParserT DataView m a
  -> Int -- byte length
  -> ParserT DataView m (Array a)
manyLength p len = do
  posBegin' <- positionZero
  fromFoldable <$> go posBegin'
  -- TODO It would be faster if we could accumulate the Array with
  -- https://pursuit.purescript.org/packages/purescript-arrays/5.3.1/docs/Data.Array.ST#v:push
  -- instead of copying from a List?
 where
  -- https://github.com/purescript-contrib/purescript-parsing/blob/e801a0ef42f3211b1602a94a269eef7ce551423f/src/Text/Parsing/Parser/Combinators.purs#L188
  go posBegin = do
    pos <- positionZero
    case compare (pos - posBegin) len of
      GT -> fail "manyLength consumed too many bytes."
      EQ -> pure Nil
      LT -> do
        x <- p
        xs <- go posBegin
        pure $ x:xs


data UnknownField
  = UnknownVarInt FieldNumber Long
  | UnknownBits64 FieldNumber Long
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
      len <- toInt <$> Decode.varint64
      case len of
        Nothing -> fail "Length-delimited value of unknown field was too long."
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
  -> (FieldNumber -> a -> PutM m Unit)
  -> PutM m Unit
putOptional _ Nothing _ = pure unit
putOptional fieldNumber (Just x) encoder = encoder (UInt.fromInt fieldNumber) x

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
putEnum' x = Encode.varint32 $ UInt.fromInt $ fromEnum x

parseEnum :: forall m a. MonadEffect m => BoundedEnum a => ParserT DataView m a
parseEnum = do
  x <- Decode.varint32
  case toEnum $ UInt.toInt x of
    Nothing -> fail $ "Enum " <> show x <> " out of bounds."
    Just e -> pure e

-- | https://github.com/purescript-contrib/purescript-parsing/pull/96
-- | If parsing fails inside this labelled context, then prepend the `String`
-- | to the error `String` in the `ParseError`.
label :: forall m s a. Monad m => String -> ParserT s m a -> ParserT s m a
label messagePrefix p = catchError p $ \ (ParseError message pos) ->
  throwError $ ParseError (messagePrefix <> message) pos