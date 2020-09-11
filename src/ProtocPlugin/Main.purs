module ProtocPlugin.Main where

import Prelude

import Effect (Effect)
import Control.Monad.Writer.Trans (tell)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
-- import Control.Category (identity)
import Data.Array (snoc, concatMap, fromFoldable)
import Data.Array as Array
-- import Data.Foldable (foldl)
import Data.Symbol (SProxy(..))
-- import Data.Tuple (Tuple(..))
-- import Data.Ord (class Ord)
-- import Data.Eq (class Eq)
-- import Data.Bounded (class Bounded)
import Data.Enum (class Enum, class BoundedEnum, toEnum, fromEnum)
import Data.Generic.Rep(class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericPred, genericSucc, genericCardinality)
import Data.Generic.Rep.Ord (genericCompare)
-- import Data.Long.Unsigned (toInt)
-- import Data.UInt (UInt)
import Data.String as String
import Data.String.Pattern as String.Pattern
import Data.UInt as UInt
-- import Control.Monad.Trans.Class (lift)

import Text.Parsing.Parser (ParserT, runParserT, fail)
-- import Text.Parsing.Parser.Combinators (manyTill)
-- import Text.Parsing.Parser.DataView (eof, takeN)
import Data.ArrayBuffer.Builder (Put, execPut)

import Record.Builder (modify)
import Record.Builder as RecordB

import Protobuf.Decode as Decode
import Protobuf.Encode as Encode
import Protobuf.Common (WireType(..))

import Node.Process (stdin, stdout, stderr)
import Node.Stream (read, write, writeString, onReadable)
import Node.Buffer (toArrayBuffer, fromArrayBuffer)
import Node.Encoding (Encoding(..))
import Node.Path (basenameWithoutExt)
-- import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (DataView)

import Protobuf.Runtime
  ( parseMessage
  , parseFieldUnknown
  , parseLenDel
  , FieldNumberInt
  , manyLength
  -- , putString
  , putLenDel
  , putOptional
  , putRepeated
  , putPacked
  )

main :: Effect Unit
main = do
  onReadable stdin $ do
    stdinbufMay <- read stdin Nothing
    case stdinbufMay of
      Nothing -> pure unit
      Just stdinbuf -> do
        stdinab <- toArrayBuffer stdinbuf
        let stdinview = DV.whole stdinab
        requestParsed <- runParserT stdinview $ parseCodeGeneratorRequest $ DV.byteLength stdinview
        case requestParsed of
          Left err -> void $ writeString stderr UTF8 (show err) (pure unit)
          Right request -> do
            void $ writeString stderr UTF8 (show request) (pure unit)
            let response = generate request
            responseab <- execPut $ putCodeGeneratorResponse response
            responsebuffer <- fromArrayBuffer responseab
            void $ write stdout responsebuffer (pure unit)


capitalize s = String.toUpper (String.take 1 s) <> String.drop 1 s

generate :: CodeGeneratorRequest -> CodeGeneratorResponse
generate (CodeGeneratorRequest{file_to_generate,parameter,proto_file,compiler_version}) =
  CodeGeneratorResponse
    { error: Nothing
    , file: map genFile proto_file
    }

type NameSpace = Array String

-- | A message descriptor, plus the names of all parent messages.
data ScopedMsg = ScopedMsg NameSpace DescriptorProto


genFile :: FileDescriptorProto -> CodeGeneratorResponse_File
genFile (FileDescriptorProto
  { name
  , package
  , dependency
  , public_dependency
  , message_type
  , enum_type
  , syntax
  }) = CodeGeneratorResponse_File
    { name : Just $ moduleName <> ".purs"
    , insertion_point : Nothing
    , content : Just content
    }
 where
  baseName = case name of
    Nothing -> "GeneratedMessages"
    Just "" -> "GeneratedMessages"
    Just n -> basenameWithoutExt n ".proto"
  moduleName = capitalize baseName
  messages = flattenMessages [] message_type
  moduleNamespace = case package of
    Nothing -> []
    -- Just ps -> String.joinWith "." $ map capitalize $ String.split "." ps
    Just ps -> String.split (String.Pattern.Pattern ".") ps
  content = String.joinWith "\n" $
    -- [ "module " <> (String.joinWith "." $ map capitalize moduleNamespace) <> moduleName
    [ "module " <> (String.joinWith "." ((map capitalize moduleNamespace) <> [moduleName]))
    , "( " <> String.joinWith "\n, " (map genMessageExport messages)
    , """)
where

import Prelude
import Effect as Effect
import Protobuf.Runtime as Runtime
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode
import Protobuf.Common as Common
import Record.Builder as Record.Builder
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Generic as Generic
import Data.Symbol as Symbol
import Text.Parsing.Parser as Parser
import Data.ArrayBuffer.Builder as ArrayBuffer.Builder
import Data.ArrayBuffer.Types as ArrayBuffer.Types
import Data.UInt as UInt
import Data.Long.Internal as Long
"""
    ]
    <>
    (map genMessage messages )



  -- | Pull all of the nested messages out of of the messages and bring them
  -- | to the top, with their namespace.
  flattenMessages :: NameSpace -> Array DescriptorProto -> Array ScopedMsg
  flattenMessages namespace msgarray = concatMap go msgarray
   where
    go :: DescriptorProto -> Array ScopedMsg
    go (DescriptorProto r@{name: Just msgName, nested_type}) =
      [ScopedMsg namespace (DescriptorProto r)]
        <> flattenMessages (namespace <> [msgName]) nested_type
    go _ = [] -- error no name


  genMessageExport :: ScopedMsg -> String
  genMessageExport (ScopedMsg namespace (DescriptorProto {name: Just msgName})) =
    let tname = mkTypeName $ namespace <> [msgName]
    in tname <> ", " <> tname <> "R, parse" <> tname <> ", put" <> tname
  genMessageExport _ = "" -- error, no name


  genMessage :: ScopedMsg -> String
  genMessage (ScopedMsg namespace (DescriptorProto {name: Just msgName, field})) =
    let tname = mkTypeName $ namespace <> [msgName]
    in
    String.joinWith "\n"
      [ "\ntype " <> tname <> "R ="
      , "  { " <> String.joinWith "\n  , " (map genFieldRecord field)
      , "  }"
      , "newtype " <> tname <> " = " <> tname <> " " <> tname <> "R"
      , "derive instance generic " <> tname <> " :: Generic.Generic " <> tname <> " _"
      , "put" <> tname <> " :: " <> tname <> " -> ArrayBuffer.Builder.Put Unit"
      , "put" <> tname <> " (" <> tname <> " r) = do"
      , String.joinWith "\n" (map genFieldPut field)
      , "parse" <> tname <> " :: Int -> Parser.ParserT ArrayBuffer.Types.DataView Effect.Effect " <> tname
      , "parse" <> tname <> " length ="
      , "  Runtime.parseMessage " <> tname <> " default parseField length"
      , """ where
  parseField
    :: Runtime.FieldNumberInt
    -> Common.WireType"""
      , "    -> Parser.ParserT ArrayBuffer.Types.DataView Effect.Effect (Record.Builder.Builder " <> tname <> "R " <> tname <> "R)"
      , String.joinWith "\n" (map genFieldParser field)
      , "  default = "
      , "    { " <> String.joinWith "\n    , " (map genFieldDefault field)
      , "    }"

      ]
  genMessage _ = ""


  genFieldPut :: FieldDescriptorProto -> String
  genFieldPut (FieldDescriptorProto
    { name: Just fname
    , number: Just fnumber
    , label: Just flabel
    , type_: Just ftype
    , type_name
    }) = f flabel ftype type_name
   where
    -- For repeated fields of primitive numeric types, always put the packed
    -- encoding.
    -- https://developers.google.com/protocol-buffers/docs/encoding?hl=en#packed
    f LABEL_REPEATED TYPE_DOUBLE _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.double'"
    f _ TYPE_DOUBLE _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.double"
    f LABEL_REPEATED TYPE_FLOAT _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.double'"
    f _ TYPE_FLOAT _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.float"
    f LABEL_REPEATED TYPE_INT64 _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.int64'"
    f _ TYPE_INT64 _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.int64"
    f LABEL_REPEATED TYPE_UINT64 _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.uint64'"
    f _ TYPE_UINT64 _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.uint64"
    f LABEL_REPEATED TYPE_INT32 _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.int32'"
    f _ TYPE_INT32 _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.int32"
    f LABEL_REPEATED TYPE_FIXED64 _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.fixed64'"
    f _ TYPE_FIXED64 _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.fixed64"
    f LABEL_REPEATED TYPE_FIXED32 _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.fixed32'"
    f _ TYPE_FIXED32 _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.fixed32"
    f LABEL_REPEATED TYPE_BOOL _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.bool'"
    f _ TYPE_BOOL _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.bool"
    f LABEL_REPEATED TYPE_STRING _ =
      "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.string"
    f _ TYPE_STRING _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.string"
    f LABEL_REPEATED TYPE_MESSAGE (Just tname) =
      "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " $ Runtime.putLenDel put" <> mkFieldName tname
    f _ TYPE_MESSAGE (Just tname) =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " $ Runtime.putLenDel put" <> mkFieldName tname
    f LABEL_REPEATED TYPE_BYTES _ =
      "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " Encode.bytes"
    f _ TYPE_BYTES _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.bytes"
    f LABEL_REPEATED TYPE_UINT32 _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.uint32'"
    f _ TYPE_UINT32 _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.uint32"
    f LABEL_REPEATED TYPE_ENUM (Just tname) =
      "  Runtime.putRepeated " <> show fnumber <> " r." <> fname <> " $ Runtime.putLenDel put" <> mkFieldName tname
    f _ TYPE_ENUM (Just tname) =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " $ Runtime.putLenDel put" <> mkFieldName tname
    f LABEL_REPEATED TYPE_SFIXED32 _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.sfixed32'"
    f _ TYPE_SFIXED32 _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.sfixed32"
    f LABEL_REPEATED TYPE_SFIXED64 _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.sfixed64'"
    f _ TYPE_SFIXED64 _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.sfixed64"
    f LABEL_REPEATED TYPE_SINT32 _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.sint32'"
    f _ TYPE_SINT32 _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.sint32"
    f LABEL_REPEATED TYPE_SINT64 _ =
      "  Runtime.putPacked " <> show fnumber <> " r." <> fname <> " Encode.sint64'"
    f _ TYPE_SINT64 _ =
      "  Runtime.putOptional " <> show fnumber <> " r." <> fname <> " Encode.sint64"

    f _ _ _ = "" -- error, maybe its a TYPE_GROUP
  genFieldPut _ = "" -- error, not enough information

  genFieldParser :: FieldDescriptorProto -> String
  genFieldParser (FieldDescriptorProto
    { name: Just fname
    , number: Just fnumber
    , label: Just flabel
    , type_: Just ftype
    , type_name
    }) = f flabel ftype type_name
   where
    -- For repeated fields of primitive numeric types, also parse the packed
    -- encoding.
    -- https://developers.google.com/protocol-buffers/docs/encoding?hl=en#packed
    f LABEL_REPEATED TYPE_DOUBLE _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " Bits64 = do"
      , "    x <- Decode.double"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_DOUBLE _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.double"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_FLOAT _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " Bits32 = do"
      , "    x <- Decode.float"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_FLOAT _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.float"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_INT64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.int64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_INT64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.int64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_UINT64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.uint64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_UINT64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.uint64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_INT32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.int32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_INT32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.int32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_FIXED64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " Bits64 = do"
      , "    x <- Decode.fixed64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_FIXED64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.fixed64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_FIXED32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " Bits32 = do"
      , "    x <- Decode.fixed32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_FIXED32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.fixed32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_BOOL _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.bool"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_BOOL _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.bool"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_STRING _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- Decode.string"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_MESSAGE (Just tname) = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel parse" <> mkFieldName tname
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_BYTES _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- Decode.bytes"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_UINT32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.uint32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_UINT32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.uint32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_ENUM (Just tname) = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- parse" <> mkFieldName tname
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_ENUM (Just tname) = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel parse" <> mkFieldName tname
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_SFIXED32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " Bits32 = do"
      , "    x <- Decode.sfixed32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_SFIXED32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.sfixed32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_SFIXED64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " Bits64 = do"
      , "    x <- Decode.sfixed64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_SFIXED64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.sfixed64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_SINT32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.sint32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_SINT32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.sint32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f LABEL_REPEATED TYPE_SINT64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.sint64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.snoc x"
      ]
    f LABEL_REPEATED TYPE_SINT64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- parseLenDel $ manyLength Decode.sint64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ flip Array.append x"
      ]
    f _ TYPE_DOUBLE _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " Bits64 = do"
      , "    x <- Decode.double"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_FLOAT _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " Bits32 = do"
      , "    x <- Decode.float"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_INT64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.int64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x "
      ]
    f _  TYPE_INT32 _ = String.joinWith "\n" -- "Array.Array Int"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.int32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_FIXED64 _ = String.joinWith "\n" -- "Array.Array (Long.Long Long.Unsigned)"
      [ "  parseField " <> show fnumber <> " Bits64 = do"
      , "    x <- Decode.fixed64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_FIXED32 _ = String.joinWith "\n" -- "Array.Array UInt"
      [ "  parseField " <> show fnumber <> " Bits32 = do"
      , "    x <- Decode.fixed32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Justx"
      ]
    f _ TYPE_BOOL _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.bool"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_STRING _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- Decode.string"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_MESSAGE (Just tname) = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- Runtime.parseLenDel parse" <> mkFieldName tname
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_BYTES _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " LenDel = do"
      , "    x <- Decode.bytes"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_UINT32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.uint32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_ENUM (Just tname) = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- parse" <> mkFieldName tname
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_SFIXED32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " Bits32 = do"
      , "    x <- Decode.sfixed32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_SFIXED64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " Bits64 = do"
      , "    x <- Decode.sfixed64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ TYPE_SINT32 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.sint32"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f LABEL_REPEATED TYPE_SINT64 _ = String.joinWith "\n"
      [ "  parseField " <> show fnumber <> " VarInt = do"
      , "    x <- Decode.sint64"
      , "    pure $ modify (Symbol.SProxy :: Symbol.SProxy \"" <> fname <> "\") $ const $ Maybe.Just x"
      ]
    f _ _ _ = "" -- error, maybe its a TYPE_GROUP
  genFieldParser _ = "" -- error, not enough information

  genFieldRecord :: FieldDescriptorProto -> String
  genFieldRecord (FieldDescriptorProto
    { name: Just fname
    , number: Just fnumber
    , label: Just flabel
    , type_: Just ftype
    , type_name
    }) = fname <> ": " <> ptype flabel ftype type_name
   where
    ptype LABEL_REPEATED TYPE_DOUBLE _ = "Array.Array Number"
    ptype LABEL_REPEATED TYPE_FLOAT _ = "Array.Array Float32.Float32"
    ptype LABEL_REPEATED TYPE_INT64 _ = "Array.Array (Long.Long Long.Signed)"
    ptype LABEL_REPEATED TYPE_UINT64 _ = "Array.Array (Long.Long Long.Unsigned)"
    ptype LABEL_REPEATED TYPE_INT32 _ = "Array.Array Int"
    ptype LABEL_REPEATED TYPE_FIXED64 _ = "Array.Array (Long.Long Long.Unsigned)"
    ptype LABEL_REPEATED TYPE_FIXED32 _ = "Array.Array UInt"
    ptype LABEL_REPEATED TYPE_BOOL _ = "Array.Array Boolean"
    ptype LABEL_REPEATED TYPE_STRING _ = "Array.Array String"
    ptype LABEL_REPEATED TYPE_MESSAGE (Just tname) = "Array.Array " <> mkFieldName tname
    ptype LABEL_REPEATED TYPE_BYTES _ = "Array.Array DataView.DataView"
    ptype LABEL_REPEATED TYPE_UINT32 _ = "Array.Array UInt.UInt"
    ptype LABEL_REPEATED TYPE_ENUM (Just tname) = "Array.Array " <> mkFieldName tname
    ptype LABEL_REPEATED TYPE_SFIXED32 _ = "Array.Array Int"
    ptype LABEL_REPEATED TYPE_SFIXED64 _ = "Array.Array (Long.Long Long.Signed)"
    ptype LABEL_REPEATED TYPE_SINT32 _ = "Array.Array Int"
    ptype LABEL_REPEATED TYPE_SINT64 _ = "Array.Array (Long.Long Long.Signed)"
    ptype _ TYPE_DOUBLE _ = "Maybe.Maybe Number"
    ptype _ TYPE_FLOAT _ = "Maybe.Maybe Float32.Float32"
    ptype _ TYPE_INT64 _ = "Maybe.Maybe (Long.Long Long.Signed)"
    ptype _ TYPE_UINT64 _ = "Maybe.Maybe (Long.Long Long.Unsigned)"
    ptype _ TYPE_INT32 _ = "Maybe.Maybe Int"
    ptype _ TYPE_FIXED64 _ = "Maybe.Maybe (Long.Long Long.Unsigned)"
    ptype _ TYPE_FIXED32 _ = "Maybe.Maybe UInt"
    ptype _ TYPE_BOOL _ = "Maybe.Maybe Boolean"
    ptype _ TYPE_STRING _ = "Maybe.Maybe String"
    ptype _ TYPE_MESSAGE (Just tname) = "Maybe.Maybe " <> mkFieldName tname
    ptype _ TYPE_BYTES _ = "Maybe.Maybe DataView.DataView"
    ptype _ TYPE_UINT32 _ = "Maybe.Maybe UInt.UInt"
    ptype _ TYPE_ENUM (Just tname) = "Maybe.Maybe " <> mkFieldName tname
    ptype _ TYPE_SFIXED32 _ = "Maybe.Maybe Int"
    ptype _ TYPE_SFIXED64 _ = "Maybe.Maybe (Long.Long Long.Signed)"
    ptype _ TYPE_SINT32 _ = "Maybe.Maybe Int"
    ptype _ TYPE_SINT64 _ = "Maybe.Maybe (Long.Long Long.Signed)"
    ptype _ _ _ = "" -- error, maybe its a TYPE_GROUP
  genFieldRecord _ = "" -- error, not enough information

  genFieldDefault :: FieldDescriptorProto -> String
  genFieldDefault (FieldDescriptorProto
    { name: Just fname
    , label: Just flabel
    }) = fname <> ": " <> dtype flabel
   where
    dtype LABEL_REPEATED = "[]"
    dtype _              = "Maybe.Nothing"
  genFieldDefault _ = "" -- error, not enough information

  mkFieldName :: String -> String
  mkFieldName n = mkTypeName $ trimPackage $ parseFieldName n

  -- mkTypeName :: NameSpace -> String -> String
  -- mkTypeName ns n = String.joinWith "" (map (\s -> capitalize s <> "_") ns) <> capitalize n
  mkTypeName :: Array String -> String
  mkTypeName ns = String.joinWith "_" $ map capitalize ns

  parseFieldName :: String -> Array String
  parseFieldName fname = Array.dropWhile (_ == "") $ String.split (String.Pattern.Pattern ".") fname

  trimPackage :: Array String -> Array String
  trimPackage ns =
    let len = Array.length moduleNamespace
    in
    if Array.take len ns == moduleNamespace
      then Array.drop len ns
      else ns



      -- IMPORTANT For embedded message fields, the parser merges multiple instances of the same field,
      -- https://developers.google.com/protocol-buffers/docs/encoding?hl=en#optional

      -- IMPORTANT In proto3, repeated fields of scalar numeric types are packed by default.
      -- https://developers.google.com/protocol-buffers/docs/encoding?hl=en#packed

-- | Record type for a CodeGenerationRequest message.
-- | https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.compiler.plugin.pb
type CodeGeneratorRequestR =
  { file_to_generate :: Array String -- 1
  , parameter :: Maybe String -- 2
  , proto_file :: Array FileDescriptorProto -- 15
  , compiler_version :: Maybe Version -- 3
  }
newtype CodeGeneratorRequest = CodeGeneratorRequest CodeGeneratorRequestR
derive instance genericCodeGeneratorRequest :: Generic CodeGeneratorRequest _
instance showCodeGeneratorRequest :: Show CodeGeneratorRequest where show = genericShow
parseCodeGeneratorRequest :: Int -> ParserT DataView Effect CodeGeneratorRequest
parseCodeGeneratorRequest length =
  parseMessage CodeGeneratorRequest default parseField length
 where
  parseField
    :: FieldNumberInt
    -> WireType
    -> ParserT DataView Effect (RecordB.Builder CodeGeneratorRequestR CodeGeneratorRequestR)
  parseField 1 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "file_to_generate") $ flip snoc x
  parseField 2 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "parameter") $ const $ Just x
  parseField 15 LenDel = do
    -- len <- UInt.toInt <$> Decode.varint32 -- faster than int32
    -- pos' <- positionZero
    -- dview <- takeN len
    -- lift (runParserT dview (parseFileDescriptorProto $ pos + pos')) >>= case _ of
    --   Left (ParseError s pos'') -> failWithPosition s $ addPosCol pos pos''
    --   Right x -> pure $ modify (SProxy :: SProxy "proto_file") $ flip snoc x
    -- x <- parseFileDescriptorProto len
    x <- parseLenDel parseFileDescriptorProto
    pure $ modify (SProxy :: SProxy "proto_file") $ flip snoc x
  parseField 3 LenDel = do
    -- len <- UInt.toInt <$> Decode.varint32 -- faster than int32
    -- pos' <- positionZero
    -- dview <- takeN len
    -- lift (runParserT dview (parseVersion $ pos + pos')) >>= case _ of
    --   Left (ParseError s pos'') -> failWithPosition s $ addPosCol pos pos''
    --   Right x -> pure $ modify (SProxy :: SProxy "compiler_version") $ const $ Just x
    -- x <- parseVersion len
    x <- parseLenDel parseVersion
    pure $ modify (SProxy :: SProxy "compiler_version") $ const $ Just x
  parseField _ wireType = parseFieldUnknown wireType
  default :: CodeGeneratorRequestR
  default =
    { file_to_generate: []
    , parameter: Nothing
    , proto_file: []
    , compiler_version: Nothing
    }


-- The plugin writes an encoded CodeGeneratorResponse to stdout.
type CodeGeneratorResponseR =
  { error :: Maybe String
  , file :: Array CodeGeneratorResponse_File
  }
newtype CodeGeneratorResponse = CodeGeneratorResponse CodeGeneratorResponseR
derive instance genericCodeGeneratorResponse :: Generic CodeGeneratorResponse _
instance showCodeGeneratorResponse :: Show CodeGeneratorResponse where show = genericShow
-- parseCodeGeneratorResponse :: Int -> ParserT DataView Effect CodeGeneratorResponse
-- parseCodeGeneratorResponse length =
--   parseMessage CodeGeneratorResponse default parseField length
--  where
--   parseField
--     :: FieldNumberInt
--     -> WireType
--     -> ParserT DataView Effect (RecordB.Builder CodeGeneratorResponseR CodeGeneratorResponseR)
--   parseField 1 LenDel = do
--     x <- Decode.string
--     pure $ modify (SProxy :: SProxy "error") $ const $ Just x
--   parseField 15 LenDel = do
--     x <- parseLenDel parseCodeGeneratorResponse_File
--     pure $ modify (SProxy :: SProxy "file") $ flip snoc x
--   parseField _ wireType = parseFieldUnknown wireType
--   default :: CodeGeneratorResponseR
--   default =
--     { error: Nothing
--     , file: []
--     }
putCodeGeneratorResponse :: CodeGeneratorResponse -> Put Unit
putCodeGeneratorResponse (CodeGeneratorResponse r) = do
  -- putString 1 r.error
  -- putLenDel 15 $ traverse_ putCodeGeneratorResponse_File r.file
  putOptional 1 r.error Encode.string
  putRepeated 15 r.file $ putLenDel putCodeGeneratorResponse_File


-- Represents a single generated file.
type CodeGeneratorResponse_FileR =
  { name :: Maybe String
  , insertion_point :: Maybe String
  , content :: Maybe String
  }
newtype CodeGeneratorResponse_File = CodeGeneratorResponse_File CodeGeneratorResponse_FileR
derive instance genericCodeGeneratorResponse_File :: Generic CodeGeneratorResponse_File _
instance showCodeGeneratorResponse_File :: Show CodeGeneratorResponse_File where show = genericShow
-- parseCodeGeneratorResponse_File :: Int -> ParserT DataView Effect CodeGeneratorResponse_File
-- parseCodeGeneratorResponse_File length =
--   parseMessage CodeGeneratorResponse_File default parseField length
--  where
--   parseField
--     :: FieldNumberInt
--     -> WireType
--     -> ParserT DataView Effect (RecordB.Builder CodeGeneratorResponse_FileR CodeGeneratorResponse_FileR)
--   parseField 1 LenDel = do
--     x <- Decode.string
--     pure $ modify (SProxy :: SProxy "name") $ const $ Just x
--   default :: CodeGeneratorResponseR
--   default =
--     { name: Nothing
--     , insertion_point: Nothing
--     , content: Nothing
--     }
putCodeGeneratorResponse_File :: CodeGeneratorResponse_File -> Put Unit
putCodeGeneratorResponse_File (CodeGeneratorResponse_File r) = do
  putOptional 1 r.name Encode.string
  putOptional 2 r.insertion_point Encode.string
  putOptional 15 r.content Encode.string

-- IMPORTANT We need to wrap our structural record types in a nominal
-- data type so that we can nest records.
-- https://github.com/purescript/documentation/blob/master/errors/CycleInTypeSynonym.md
-- And so that we can assign instances?


-- | The version number of protocol compiler.
-- | https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.descriptor.pb
type VersionR =
  { major :: Maybe Int --1
  , minor :: Maybe Int --2
  , patch :: Maybe Int --3
  , suffix :: Maybe String -- 4
  }
newtype Version = Version VersionR
derive instance genericVersion :: Generic Version _
instance showVersion :: Show Version where show = genericShow
parseVersion :: Int -> ParserT DataView Effect Version
parseVersion length =
  parseMessage Version default parseField length
 where
  parseField
    :: FieldNumberInt
    -> WireType
    -> ParserT DataView Effect (RecordB.Builder VersionR VersionR)
  parseField 1 VarInt = do
    x <- Decode.int32
    pure $ modify (SProxy :: SProxy "major") $ const $ Just x
  parseField 2 VarInt = do
    x <- Decode.int32
    pure $ modify (SProxy :: SProxy "minor") $ const $ Just x
  parseField 3 VarInt = do
    x <- Decode.int32
    pure $ modify (SProxy :: SProxy "patch") $ const $ Just x
  parseField 4 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "suffix") $ const $ Just x
  parseField _ wireType = parseFieldUnknown wireType
  default =
    { major: Nothing
    , minor: Nothing
    , patch: Nothing
    , suffix: Nothing
    }

-- | Describes a complete .proto file.
-- | https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.descriptor.pb
-- | The syntax for decoding this is "proto2"?
type FileDescriptorProtoR =
  { name :: Maybe String -- 1
  , package :: Maybe String -- 2
  , dependency :: Array String -- 3
  , public_dependency :: Array Int -- 10
  , message_type :: Array DescriptorProto -- 4
  , enum_type :: Array EnumDescriptorProto -- 5
  -- TODO , service :: Array ServiceDescriptorProto -- 6
  -- , extension :: Array FieldDescriptorProto -- 7
  -- TODO , options :: Maybe FileOptions -- 8
  -- TODO , source_code_info :: Maybe SourceCodeInfo -- 9
  , syntax :: Maybe String -- 12
  }
newtype FileDescriptorProto = FileDescriptorProto FileDescriptorProtoR
derive instance genericFileDescriptorProto :: Generic FileDescriptorProto _
instance showFileDescriptorProto :: Show FileDescriptorProto where show = genericShow
parseFileDescriptorProto :: Int -> ParserT DataView Effect FileDescriptorProto
parseFileDescriptorProto length =
  parseMessage FileDescriptorProto default parseField length
 where
  parseField
    :: FieldNumberInt
    -> WireType
    -> ParserT DataView Effect (RecordB.Builder FileDescriptorProtoR FileDescriptorProtoR)
  parseField 1 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "name") $ const $ Just x
  parseField 2 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "package") $ const $ Just x
  parseField 3 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "dependency") $ flip snoc x
  parseField 10 Bits32 = do -- then this repeated int32 field is nonpacked
    x <- Decode.int32
    pure $ modify (SProxy :: SProxy "public_dependency") $ flip snoc x
  parseField 10 LenDel = do -- then this repeated int32 field is packed
    xs <- parseLenDel $ manyLength Decode.int32
    pure $ modify (SProxy :: SProxy "public_dependency") $ flip append xs
  parseField 4 LenDel = do
    x <- parseLenDel parseDescriptorProto
    pure $ modify (SProxy :: SProxy "message_type") $ flip snoc x
  parseField 5 LenDel = do
    x <- parseLenDel parseEnumDescriptorProto
    pure $ modify (SProxy :: SProxy "enum_type") $ flip snoc x
  parseField 12 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "syntax") $ const $ Just x
  parseField _ wireType = parseFieldUnknown wireType
  default =
    { name : Nothing
    , package : Nothing
    , dependency : []
    , public_dependency : []
    , message_type : []
    , enum_type : []
    -- TODO , service :
    -- , extension : []
    -- TODO , options : []
    -- TODO , source_code_info : []
    , syntax : Nothing
    }






-- | Describes a message type.
type DescriptorProtoR =
  { name :: Maybe String -- 1
  , field :: Array FieldDescriptorProto -- 2
  -- , extension :: Array FieldDescriptorProto -- 6
  , nested_type :: Array DescriptorProto -- 3
  , enum_type :: Array EnumDescriptorProto -- 4
  -- , extension_range :: Array DescriptorProto_ExtensionRange -- 5
  , oneof_decl :: Array OneofDescriptorProto -- 8
  -- TODO , options :: Maybe MessageOptions -- 7
  -- TODO eh who cares about reserved ranges
  }
newtype DescriptorProto = DescriptorProto DescriptorProtoR
derive instance genericDescriptorProto :: Generic DescriptorProto _
instance showDescriptorProto :: Show DescriptorProto where
  show (DescriptorProto{name,field}) = -- TODO genericShow, doesn't work because https://github.com/purescript/documentation/blob/master/errors/CycleInDeclaration.md
    "DescriptorProto { name: " <> show name <> ", field: " <> show field <> "}"
parseDescriptorProto :: Int -> ParserT DataView Effect DescriptorProto
parseDescriptorProto length =
  parseMessage DescriptorProto default parseField length
 where
  parseField
    :: FieldNumberInt
    -> WireType
    -> ParserT DataView Effect (RecordB.Builder DescriptorProtoR DescriptorProtoR)
  parseField 1 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "name") $ const $ Just x
  parseField 2 LenDel = do
    x <- parseLenDel parseFieldDescriptorProto
    pure $ modify (SProxy :: SProxy "field") $ flip snoc x
  parseField 3 LenDel = do
    x <- parseLenDel parseDescriptorProto
    pure $ modify (SProxy :: SProxy "nested_type") $ flip snoc x
  parseField 4 lenDel = do
    x <- parseLenDel parseEnumDescriptorProto
    pure $ modify (SProxy :: SProxy "enum_type") $ flip snoc x
  parseField 8 lenDel = do
    x <- parseLenDel parseOneofDescriptorProto
    pure $ modify (SProxy :: SProxy "oneof_decl") $ flip snoc x
  parseField _ wireType = parseFieldUnknown wireType
  default =
    { name: Nothing
    , field: []
    , nested_type: []
    , enum_type: []
    , oneof_decl: []
    }


-- newtype DescriptorProto_ExtensionRange = DescriptorProto_ExtensionRange DescriptorProto_ExtensionRangeR
-- derive instance genericDescriptorProto_ExtensionRange :: Generic DescriptorProto_ExtensionRange _
-- instance showDescriptorProto_ExtensionRange :: Show DescriptorProto_ExtensionRange where show = genericShow
-- type DescriptorProto_ExtensionRangeR =
--   { start :: Maybe Int -- 1
--   , end :: Maybe Int -- 2
--   -- TODO , options :: Maybe ExtensionRangeOptions
--   }

-- TODO
-- data ExtensionRangeOptions = ExtensionRangeOptions
--   { uninterpreted_option :: Array UninterpretedOption
--   -- ,  TODO extensions ::
--   }

data FieldDescriptorProto_Label = LABEL_OPTIONAL | LABEL_REQUIRED | LABEL_REPEATED
derive instance genericFieldDescriptorProto_Label :: Generic FieldDescriptorProto_Label _
derive instance eqFieldDescriptorProto_Label :: Eq FieldDescriptorProto_Label
instance showFieldDescriptorProto_Label :: Show FieldDescriptorProto_Label where show = genericShow
instance ordFieldDescriptorProto_Label :: Ord FieldDescriptorProto_Label
 where
  compare = genericCompare
instance boundedFieldDescriptorProto_Label :: Bounded FieldDescriptorProto_Label
 where
  -- bottom = LABEL_OPTIONAL
  -- top = LABEL_REPEATED
  bottom = genericBottom
  top = genericTop
instance enumFieldDescriptorProto_Label :: Enum FieldDescriptorProto_Label
 where
  succ = genericSucc
  pred = genericPred
  -- succ LABEL_OPTIONAL = Just LABEL_REQUIRED
  -- succ LABEL_REQUIRED = Just LABEL_REPEATED
  -- succ LABEL_REPEATED = Nothing
  -- pred LABEL_OPTIONAL = Nothing
  -- pred LABEL_REQUIRED = Just LABEL_OPTIONAL
  -- pred LABEL_REPEATED = Just LABEL_REQUIRED
instance boundedEnumFieldDescriptorProto_Label :: BoundedEnum FieldDescriptorProto_Label
 where
  -- cardinality = Cardinality 3
  cardinality = genericCardinality
  toEnum 1 = Just LABEL_OPTIONAL
  toEnum 2 = Just LABEL_REQUIRED
  toEnum 3 = Just LABEL_REPEATED
  toEnum _ = Nothing
  fromEnum LABEL_OPTIONAL = 1
  fromEnum LABEL_REQUIRED = 2
  fromEnum LABEL_REPEATED = 3
parseFieldDescriptorProto_Label :: ParserT DataView Effect FieldDescriptorProto_Label
parseFieldDescriptorProto_Label = do
  x <- Decode.varint32
  case toEnum $ UInt.toInt x of
    Nothing -> fail $ "Out of range FieldDescriptorProto_Label " <> show x
    Just e -> pure e

data FieldDescriptorProto_Type
  = TYPE_DOUBLE
  | TYPE_FLOAT
  | TYPE_INT64
  | TYPE_UINT64
  | TYPE_INT32
  | TYPE_FIXED64
  | TYPE_FIXED32
  | TYPE_BOOL
  | TYPE_STRING
  | TYPE_GROUP
  | TYPE_MESSAGE
  | TYPE_BYTES
  | TYPE_UINT32
  | TYPE_ENUM
  | TYPE_SFIXED32
  | TYPE_SFIXED64
  | TYPE_SINT32
  | TYPE_SINT64
derive instance genericFieldDescriptorProto_Type :: Generic FieldDescriptorProto_Type  _
derive instance eqFieldDescriptorProto_Type :: Eq FieldDescriptorProto_Type
instance showFieldDescriptorProto_Type :: Show FieldDescriptorProto_Type where show = genericShow
instance ordFieldDescriptorProto_Type :: Ord FieldDescriptorProto_Type
 where
  compare = genericCompare
instance boundedFieldDescriptorProto_Type :: Bounded FieldDescriptorProto_Type
 where
  bottom = genericBottom
  top = genericTop
instance enumFieldDescriptorProto_Type :: Enum FieldDescriptorProto_Type
 where
  succ = genericSucc
  pred = genericPred
instance boundedEnumFieldDescriptorProto_Type :: BoundedEnum FieldDescriptorProto_Type
 where
  -- cardinality = Cardinality 3
  cardinality = genericCardinality
  toEnum 1 = Just TYPE_DOUBLE
  toEnum 2 = Just TYPE_FLOAT
  toEnum 3 = Just TYPE_INT64
  toEnum 4 = Just TYPE_UINT64
  toEnum 5 = Just TYPE_INT32
  toEnum 6 = Just TYPE_FIXED64
  toEnum 7 = Just TYPE_FIXED32
  toEnum 8 = Just TYPE_BOOL
  toEnum 9 = Just TYPE_STRING
  toEnum 10 = Just TYPE_GROUP
  toEnum 11 = Just TYPE_MESSAGE
  toEnum 12 = Just TYPE_BYTES
  toEnum 13 = Just TYPE_UINT32
  toEnum 14 = Just TYPE_ENUM
  toEnum 15 = Just TYPE_SFIXED32
  toEnum 16 = Just TYPE_SFIXED64
  toEnum 17 = Just TYPE_SINT32
  toEnum 18 = Just TYPE_SINT64
  toEnum _ = Nothing
  fromEnum TYPE_DOUBLE   = 1
  fromEnum TYPE_FLOAT    = 2
  fromEnum TYPE_INT64    = 3
  fromEnum TYPE_UINT64   = 4
  fromEnum TYPE_INT32    = 5
  fromEnum TYPE_FIXED64  = 6
  fromEnum TYPE_FIXED32  = 7
  fromEnum TYPE_BOOL     = 8
  fromEnum TYPE_STRING   = 9
  fromEnum TYPE_GROUP    = 10
  fromEnum TYPE_MESSAGE  = 11
  fromEnum TYPE_BYTES    = 12
  fromEnum TYPE_UINT32   = 13
  fromEnum TYPE_ENUM     = 14
  fromEnum TYPE_SFIXED32 = 15
  fromEnum TYPE_SFIXED64 = 16
  fromEnum TYPE_SINT32   = 17
  fromEnum TYPE_SINT64   = 18
parseFieldDescriptorProto_Type :: ParserT DataView Effect FieldDescriptorProto_Type
parseFieldDescriptorProto_Type = do
  x <- Decode.varint32
  case toEnum $ UInt.toInt x of
    Nothing -> fail $ "Out of range FieldDescriptorProto_Type " <> show x
    Just e -> pure e

-- | Describes a field within a message.
newtype FieldDescriptorProto = FieldDescriptorProto FieldDescriptorProtoR
derive instance genericFieldDescriptorProto :: Generic FieldDescriptorProto _
instance showFieldDescriptorProto :: Show FieldDescriptorProto where show = genericShow
type FieldDescriptorProtoR =
  { name :: Maybe String -- 1
  , number :: Maybe Int -- 3
  , label :: Maybe FieldDescriptorProto_Label -- 4
  , type_ :: Maybe FieldDescriptorProto_Type -- 5
  , type_name :: Maybe String -- 6
  -- , extendee :: Maybe String -- 2
  -- , default_value :: Maybe String -- 7 -- TODO meh support this?
  , oneof_index :: Maybe Int -- 9
  , json_name :: Maybe String -- 10
  -- TODO , options :: Maybe FieldOptions --8
  }
parseFieldDescriptorProto :: Int -> ParserT DataView Effect FieldDescriptorProto
parseFieldDescriptorProto length =
  parseMessage FieldDescriptorProto default parseField length
 where
  parseField
    :: FieldNumberInt
    -> WireType
    -> ParserT DataView Effect (RecordB.Builder FieldDescriptorProtoR FieldDescriptorProtoR)
  parseField 1 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "name") $ const $ Just x
  parseField 3 VarInt = do
    x <- Decode.int32
    pure $ modify (SProxy :: SProxy "number") $ const $ Just x
  parseField 4 VarInt = do
    x <- parseFieldDescriptorProto_Label
    pure $ modify (SProxy :: SProxy "label") $ const $ Just x
  parseField 5 VarInt = do
    x <- parseFieldDescriptorProto_Type
    pure $ modify (SProxy :: SProxy "type_") $ const $ Just x
  parseField 6 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "type_name") $ const $ Just x
  parseField 9 VarInt = do
    x <- Decode.int32
    pure $ modify (SProxy :: SProxy "oneof_index") $ const $ Just x
  parseField 10 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "json_name") $ const $ Just x
  parseField _ wireType = parseFieldUnknown wireType
  default =
    { name: Nothing
    , number: Nothing
    , label: Nothing
    , type_: Nothing
    , type_name: Nothing
    , oneof_index: Nothing
    , json_name: Nothing
    }


-- | Describes a oneof.
type OneofDescriptorProtoR =
  { name :: Maybe String -- 1
  -- TODO , options :: Maybe OneofOptions -- 2
  }
newtype OneofDescriptorProto = OneofDescriptorProto OneofDescriptorProtoR
derive instance genericOneofDescriptorProto :: Generic OneofDescriptorProto _
instance showOneofDescriptorProto :: Show OneofDescriptorProto where show = genericShow
parseOneofDescriptorProto :: Int -> ParserT DataView Effect OneofDescriptorProto
parseOneofDescriptorProto length =
  parseMessage OneofDescriptorProto default parseField length
 where
  parseField
    :: FieldNumberInt
    -> WireType
    -> ParserT DataView Effect (RecordB.Builder OneofDescriptorProtoR OneofDescriptorProtoR)
  parseField 1 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "name") $ const $ Just x
  parseField _ wireType = parseFieldUnknown wireType
  default =
    { name: Nothing
    }



-- | Describes an enum type.
type EnumDescriptorProtoR =
  { name :: Maybe String -- 1
  , value :: Array EnumValueDescriptorProto -- 2
  -- TODO , options :: Maybe EnumOptions -- 3
  -- TODO , reserved_range :: Array EnumReservedRange -- 4
  -- TODO , reserved_name :: Array String -- 5
  }
newtype EnumDescriptorProto = EnumDescriptorProto EnumDescriptorProtoR
derive instance genericEnumDescriptorProto :: Generic EnumDescriptorProto _
instance showEnumDescriptorProto :: Show EnumDescriptorProto where show = genericShow
parseEnumDescriptorProto :: Int -> ParserT DataView Effect EnumDescriptorProto
parseEnumDescriptorProto length =
  parseMessage EnumDescriptorProto default parseField length
 where
  parseField
    :: FieldNumberInt
    -> WireType
    -> ParserT DataView Effect (RecordB.Builder EnumDescriptorProtoR EnumDescriptorProtoR)
  parseField 1 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "name") $ const $ Just x
  parseField 2 LenDel = do
    x <- parseLenDel parseEnumValueDescriptorProto
    pure $ modify (SProxy :: SProxy "value") $ flip snoc x
  parseField _ wireType = parseFieldUnknown wireType
  default =
    { name: Nothing
    , value: []
    }


-- | Describes a value within an enum.
type EnumValueDescriptorProtoR =
  { name :: Maybe String -- 1
  , number :: Maybe Int -- 2
  -- TODO , options :: Maybe EnumValueOptions -- 3
  }
newtype EnumValueDescriptorProto = EnumValueDescriptorProto EnumValueDescriptorProtoR
derive instance genericEnumValueDescriptorProto :: Generic EnumValueDescriptorProto _
instance showEnumValueDescriptorProto :: Show EnumValueDescriptorProto where show = genericShow
parseEnumValueDescriptorProto :: Int -> ParserT DataView Effect EnumValueDescriptorProto
parseEnumValueDescriptorProto length =
  parseMessage EnumValueDescriptorProto default parseField length
 where
  parseField
    :: FieldNumberInt
    -> WireType
    -> ParserT DataView Effect (RecordB.Builder EnumValueDescriptorProtoR EnumValueDescriptorProtoR)
  parseField 1 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "name") $ const $ Just x
  parseField 2 VarInt = do
    x <- Decode.int32
    pure $ modify (SProxy :: SProxy "number") $ const $ Just x
  parseField _ wireType = parseFieldUnknown wireType
  default =
    { name: Nothing
    , number: Nothing
    }
