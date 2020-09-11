-- | This module is for import by the generated .purs message modules.
module Protobuf.Runtime
( parseMessage
, parseFieldUnknown
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
)
where

import Prelude
-- import Effect (Effect)
import Effect.Class (class MonadEffect)
import Control.Monad.Writer.Trans (tell)
import Data.Enum (class BoundedEnum, toEnum, fromEnum)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.Long.Unsigned (toInt)
import Data.UInt as UInt
import Data.UInt (UInt)
import Data.List (List(..), (:))
import Data.Array (fromFoldable)
import Data.Foldable (traverse_)
import Text.Parsing.Parser (ParserT, fail, position)
import Text.Parsing.Parser.Pos (Position(..))
import Data.ArrayBuffer.Types (DataView)
import Text.Parsing.Parser.DataView (takeN, eof)
import Text.Parsing.Parser.Combinators (manyTill)
import Data.ArrayBuffer.Builder (PutM, subBuilder)
import Data.ArrayBuffer.Builder as ABBuilder
import Record.Builder as RecordB
import Record.Builder (build, modify)
import Protobuf.Common (FieldNumber, WireType(..))
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode

-- | We don't recognize the field number, so consume the field and throw it away
-- | and then return a no-op Record builder.
parseFieldUnknown
  :: forall m r
   . MonadEffect m
  => WireType
  -> ParserT DataView m (RecordB.Builder (Record r) (Record r))
parseFieldUnknown wireType = pure identity <* case wireType of
  VarInt -> void Decode.varint64
  Bits64 -> void $ takeN 8
  LenDel -> do
        len <- toInt <$> Decode.varint64
        case len of
          Nothing -> fail "Length-delimited value of unknown field was too long."
          Just l -> void $ takeN l
  Bits32 -> void $ takeN 4

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
  -- builders <- manyTill applyParser eof
  builders <- manyLength applyParser length
  pure $ construct $ build (foldl (>>>) identity builders) default
 where
  applyParser = do
    Tuple fieldNumber wireType <- Decode.tag32
    parseField (UInt.toInt fieldNumber) wireType

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
  -- TODO It would be faster if we could accumulate the Array directly with
  -- https://pursuit.purescript.org/packages/purescript-arrays/5.3.1/docs/Data.Array.ST#v:push
  -- instead of copying from a List?
 where
  -- https://github.com/purescript-contrib/purescript-parsing/blob/e801a0ef42f3211b1602a94a269eef7ce551423f/src/Text/Parsing/Parser/Combinators.purs#L188
  go posBegin = do
    pos <- positionZero
    case compare (pos - posBegin) len of
      GT -> fail "Length-delimited repeated field consumed too many bytes."
      EQ -> pure Nil
      LT -> do
        x <- p
        xs <- go posBegin
        pure $ x:xs


-- -- | Call a parser once and check that exactly *N* bytes have been consumed.
-- -- | Will fail if too many or too few bytes are consumed.
-- -- | TODO Deprecate? I don't think we need this.
-- onceLength
--   :: forall a
--    . ParserT DataView Effect a
--   -> Int -- byte length
--   -> ParserT DataView Effect a
-- onceLength p len = do
--   posBegin <- positionZero
--   x <- p
--   posEnd <- positionZero
--   case compare (posEnd - posBegin) len of
--     LT -> fail "Length-delimited field consumed too few bytes."
--     EQ -> pure x
--     GT -> fail "Length-delimited field consumed too many bytes."

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
  => FieldNumberInt -> a -> PutM m Unit
putEnum fieldNumber x = do
  Encode.tag32 (UInt.fromInt fieldNumber) VarInt
  putEnum' x

putEnum' :: forall m a. MonadEffect m => BoundedEnum a => a -> PutM m Unit
putEnum' x = Encode.varint32 $ UInt.fromInt $ fromEnum x

parseEnum :: forall m a. MonadEffect m => BoundedEnum a => ParserT DataView m a
parseEnum = do
  x <- Decode.varint32
  case toEnum $ UInt.toInt x of
    Nothing -> fail "Enum out bounds" -- TODO better error
    Just e -> pure e
