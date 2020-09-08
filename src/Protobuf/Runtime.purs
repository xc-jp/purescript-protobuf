-- | This module is for import by the generated .purs message modules.
module Protobuf.Runtime
( parseMessage
, parseFieldUnknown
, Pos
, FieldNumberInt
, positionZero
, addPosCol
)
where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.Long.Unsigned (toInt)
import Data.UInt as UInt
import Text.Parsing.Parser (ParserT, fail, position)
import Text.Parsing.Parser.Pos (Position(..))
import Data.ArrayBuffer.Types (DataView)
import Text.Parsing.Parser.DataView (takeN, eof)
import Text.Parsing.Parser.Combinators (manyTill)
import Record.Builder as RecordB
import Record.Builder (build, modify)
import Protobuf.Common (FieldNumber, WireType(..))
import Protobuf.Decode as Decode
-- import Protobuf.Encode as Encode

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
   -- . Pos
   . (Record r -> a)
  -> (Record r)
  -> (FieldNumberInt -> WireType -> ParserT DataView Effect (RecordB.Builder (Record r) (Record r)))
  -- -> (Pos -> FieldNumberInt -> WireType -> ParserT DataView Effect (RecordB.Builder (Record r) (Record r)))
  -> ParserT DataView Effect a
parseMessage construct default parseField = do
  builders <- manyTill applyParser eof
  pure $ construct $ build (foldl (>>>) identity builders) default
 where
  applyParser = do
    Tuple fieldNumber wireType <- Decode.tag32
    parseField (UInt.toInt fieldNumber) wireType

-- | Parse position, zero-based, unlike Text.Parsing.Parser.Position which is one-based.
type Pos = Int

-- | We want Int FieldNumber to pass to parseField so that we can pattern
-- | match on Int literals.
type FieldNumberInt = Int

-- | Add an offset to a parser column Position and return the new Position.
-- | We need this so we can report accurate positions of parsing errors
-- | in Length-Delimited message fields, for which we recursively call
-- | runParserT on a DataView which frames the Length-Delimited message field.
-- | Here we have to take advantage of our knowledge that
-- | Text.Parsing.Parser.DataView only uses the column field of Position.
-- | We also note that Text.Parsing.Parser starts counting at 1, not 0.
-- | https://github.com/purescript-contrib/purescript-parsing/issues/94
addPosCol :: Pos -> Position -> Position
addPosCol p (Position {column,line}) = Position {column: column + p, line}

-- | Zero-based position in the parser.
positionZero :: forall s m. Monad m => ParserT s m Pos
positionZero = do
  Position {column,line} <- position
  pure $ column - 1
