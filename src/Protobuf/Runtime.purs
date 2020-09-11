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
)
where

import Prelude
import Effect (Effect)
import Control.Monad.Writer.Trans (tell)
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
import Data.ArrayBuffer.Builder (Put, subBuilder)
import Data.ArrayBuffer.Builder as ABBuilder
import Record.Builder as RecordB
import Record.Builder (build, modify)
import Protobuf.Common (FieldNumber, WireType(..))
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode

-- | We don't recognize the field number, so consume the field and throw it away
-- | and then return a no-op Record builder.
parseFieldUnknown
  :: forall r
   . WireType
  -> ParserT DataView Effect (RecordB.Builder (Record r) (Record r))
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
  :: forall a r
   . (Record r -> a)
  -> (Record r)
  -> (FieldNumberInt -> WireType -> ParserT DataView Effect (RecordB.Builder (Record r) (Record r)))
  -> Int
  -> ParserT DataView Effect a
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
  :: forall a
   . ParserT DataView Effect a
  -> Int -- byte length
  -> ParserT DataView Effect (Array a)
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
  :: forall a
   . (Int -> ParserT DataView Effect a)
  -> ParserT DataView Effect a
parseLenDel p = p <<< UInt.toInt =<< Decode.varint32

putLenDel :: forall a . (a -> Put Unit) -> UInt -> a -> Put Unit
putLenDel p fieldNumber x = do
  b <- subBuilder $ p x
  Encode.builder fieldNumber b

putOptional :: forall a. FieldNumberInt -> Maybe a -> (UInt -> a -> Put Unit) -> Put Unit
putOptional _ Nothing _ = pure unit
putOptional fieldNumber (Just x) encoder = encoder (UInt.fromInt fieldNumber) x

putRepeated :: forall a. FieldNumberInt -> Array a -> (UInt -> a -> Put Unit) -> Put Unit
putRepeated fieldNumber xs encoder = flip traverse_ xs $ encoder $ UInt.fromInt fieldNumber

putPacked :: forall a. FieldNumberInt -> Array a -> (a -> Put Unit) -> Put Unit
putPacked _ [] _ = pure unit
putPacked fieldNumber xs encoder = do
  b <- subBuilder $ traverse_ encoder xs
  Encode.builder (UInt.fromInt fieldNumber) b
