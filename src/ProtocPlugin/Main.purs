module ProtocPlugin.Main where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
-- import Control.Category (identity)
import Data.Array (snoc)
import Data.Foldable (foldl)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Ord (class Ord)
import Data.Eq (class Eq)
import Data.Bounded (class Bounded)
import Data.Enum (class Enum, class BoundedEnum, toEnum, fromEnum)
import Data.Generic.Rep(class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericPred, genericSucc, genericCardinality)
import Data.Generic.Rep.Ord (genericCompare)
-- import Data.Long.Unsigned (toInt)
import Data.UInt (UInt)
import Data.UInt as UInt
import Control.Monad.Trans.Class (lift)

import Text.Parsing.Parser (ParserT, runParserT, ParseError(..), fail)
import Text.Parsing.Parser.Combinators (manyTill)
-- import Text.Parsing.Parser.DataView (eof, takeN)

import Record.Builder (build, modify)
import Record.Builder as RecordB

import Protobuf.Decode as Decode
-- import Protobuf.Encode as Encode
import Protobuf.Common (WireType(..))

import Node.Process (stdin, stdout, stderr)
import Node.Stream (read, writeString, onReadable)
import Node.Buffer (toArrayBuffer)
import Node.Encoding (Encoding(..))
-- import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (DataView)

import Protobuf.Runtime
  ( parseMessage
  , parseFieldUnknown
  , parseLenDel
  , FieldNumberInt
  , onceLength
  , manyLength
  )

main :: Effect Unit
main = do
  onReadable stdin $ do
    stdinbufMay <- read stdin Nothing
    case stdinbufMay of
      Nothing -> pure unit
      Just stdinbuf -> do
        stdinab <- toArrayBuffer stdinbuf
        -- void $ writeString stdout UTF8 (show $ AB.byteLength stdinab) (pure unit)
        let stdinview = DV.whole stdinab
        request <- runParserT stdinview $ parseCodeGeneratorRequest $ DV.byteLength stdinview
        void $ writeString stderr UTF8 (show request) (pure unit)

-- parseCodeGeneratorRequest :: ParserT DataView Effect CodeGeneratorRequest
-- parseCodeGeneratorRequest = do
--   builders <- manyTill parseField eof
--   pure $ CodeGeneratorRequest $ build (foldl (>>>) identity builders) defaultCodeGeneratorRequest
--  where
--   parseField :: ParserT DataView Effect (RecordB.Builder CodeGeneratorRequestR CodeGeneratorRequestR)
--   parseField = do
--     Tuple fieldNumber wireType <- Decode.tag32
--     case unit of
--       _ | fieldNumber == fn_CodeGeneratorRequest_file_to_generate -> do
--             x <- Decode.string
--             pure $ modify fs_CodeGeneratorRequest_file_to_generate $ flip snoc x
--         | fieldNumber == fn_CodeGeneratorRequest_parameter -> do
--             x <- Decode.string
--             pure $ modify fs_CodeGeneratorRequest_parameter $ const $ Just x
--         -- | fieldNumber == fn_CodeGeneratorRequest_proto_file -> do
--         --     x <- parseFileDescriptorProto
--         --     pure $ modify fs_CodeGeneratorRequest_proto_file $ flip snoc x
--         -- | fieldNumber == fn_CodeGeneratorRequest_compiler_version -> do
--         --     x <- parseVersion
--         --     pure $ modify fs_CodeGeneratorRequest_compiler_version $ const $ Just x
--       _ -> parseFieldUnknown wireType



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
    len <- UInt.toInt <$> Decode.varint32 -- faster than int32
    -- pos' <- positionZero
    -- dview <- takeN len
    -- lift (runParserT dview (parseFileDescriptorProto $ pos + pos')) >>= case _ of
    --   Left (ParseError s pos'') -> failWithPosition s $ addPosCol pos pos''
    --   Right x -> pure $ modify (SProxy :: SProxy "proto_file") $ flip snoc x
    x <- parseFileDescriptorProto len
    pure $ modify (SProxy :: SProxy "proto_file") $ flip snoc x
  parseField 3 LenDel = do
    len <- UInt.toInt <$> Decode.varint32 -- faster than int32
    -- pos' <- positionZero
    -- dview <- takeN len
    -- lift (runParserT dview (parseVersion $ pos + pos')) >>= case _ of
    --   Left (ParseError s pos'') -> failWithPosition s $ addPosCol pos pos''
    --   Right x -> pure $ modify (SProxy :: SProxy "compiler_version") $ const $ Just x
    x <- parseVersion len
    pure $ modify (SProxy :: SProxy "compiler_version") $ const $ Just x
  parseField _ wireType = parseFieldUnknown wireType

  default :: CodeGeneratorRequestR
  default =
    { file_to_generate: []
    , parameter: Nothing
    , proto_file: []
    , compiler_version: Nothing
    }





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
  , extension :: Array FieldDescriptorProto -- 7
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
  parseField 10 LenDel = do
    -- then this repeated int32 field is packed
    -- https://developers.google.com/protocol-buffers/docs/encoding?hl=en#packed
    -- xs <- manyLength Decode.int32 <<< UInt.toInt =<< Decode.varint32
    xs <- parseLenDel $ manyLength Decode.int32
    pure $ modify (SProxy :: SProxy "public_dependency") $ flip append xs
  parseField 4 LenDel = do
    -- x <- parseDescriptorProto <<< Uint.toInt =<< Decode.varint32
    x <- parseLenDel parseDescriptorProto
    pure $ modify (SProxy :: SProxy "message_type") $ flip snoc x
  parseField _ wireType = parseFieldUnknown wireType
  default =
    { name : Nothing
    , package : Nothing
    , dependency : []
    , public_dependency : []
    , message_type : []
    , enum_type : []
    -- TODO , service :
    , extension : []
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
    -- x <- parseFieldDescriptorProto <<< Uint.toInt =<< Decode.varint32
    x <- parseLenDel parseFieldDescriptorProto
    pure $ modify (SProxy :: SProxy "field") $ flip snoc x
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
  -- , oneof_index :: Maybe Int -- 9
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
    , json_name: Nothing
    }


-- | Describes a oneof.
newtype OneofDescriptorProto = OneofDescriptorProto OneofDescriptorProtoR
derive instance genericOneofDescriptorProto :: Generic OneofDescriptorProto _
instance showOneofDescriptorProto :: Show OneofDescriptorProto where show = genericShow
type OneofDescriptorProtoR =
  { name :: Maybe String -- 1
  -- TODO , options :: Maybe OneofOptions -- 2
  }

-- | Describes an enum type.
newtype EnumDescriptorProto = EnumDescriptorProto EnumDescriptorProtoR
derive instance genericEnumDescriptorProto :: Generic EnumDescriptorProto _
instance showEnumDescriptorProto :: Show EnumDescriptorProto where show = genericShow
type EnumDescriptorProtoR =
  { name :: Maybe String -- 1
  , value :: Array EnumValueDescriptorProto -- 2
  -- TODO , options :: Maybe EnumOptions -- 3
  -- TODO , reserved_range :: Array EnumReservedRange -- 4
  -- TODO , reserved_name :: Array String -- 5
  }

-- | Describes a value within an enum.
newtype EnumValueDescriptorProto = EnumValueDescriptorProto EnumValueDescriptorProtoR
derive instance genericEnumValueDescriptorProto :: Generic EnumValueDescriptorProto _
instance showEnumValueDescriptorProto :: Show EnumValueDescriptorProto where show = genericShow
type EnumValueDescriptorProtoR =
  { name :: Maybe String -- 1
  , number :: Maybe Int -- 2
  -- TODO , options :: Maybe EnumValueOptions -- 3
  }

-- TODO data ServiceDescriptorProto

-- TODO data MethodDescriptorProto
