-- | This module is for import by the generated .purs message modules.
module Protobuf.Runtime
( parseMessage
, parseFieldUnknown
)
where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.Long.Unsigned (toInt)
import Data.UInt as UInt
import Text.Parsing.Parser (ParserT, fail)
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
   . (Record r -> a)
  -> (Record r)
  -- -> (FieldNumber -> WireType -> ParserT DataView Effect (RecordB.Builder (Record r) (Record r)))
  -> (Int -> WireType -> ParserT DataView Effect (RecordB.Builder (Record r) (Record r)))
  -> ParserT DataView Effect a
parseMessage construct default parseField = do
  builders <- manyTill applyParser eof
  pure $ construct $ build (foldl (>>>) identity builders) default
 where
  applyParser = do
    Tuple fieldNumber wireType <- Decode.tag32
    parseField (UInt.toInt fieldNumber) wireType

