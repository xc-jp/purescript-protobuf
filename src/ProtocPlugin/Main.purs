module ProtocPlugin.Main where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
-- import Control.Category (identity)
import Data.Array (snoc)
import Data.Foldable (foldl)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Generic.Rep(class Generic)
import Data.Generic.Rep.Show (genericShow)
-- import Data.Long.Unsigned (toInt)
import Data.UInt (UInt)
import Data.UInt as UInt

import Text.Parsing.Parser (ParserT, fail, runParserT)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.DataView (eof)

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
        request <- runParserT stdinview parseCodeGeneratorRequest
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

parseCodeGeneratorRequest :: ParserT DataView Effect CodeGeneratorRequest
parseCodeGeneratorRequest =
  parseMessage CodeGeneratorRequest default parseField
 where
  parseField
    :: Int
    -> WireType
    -> ParserT DataView Effect (RecordB.Builder CodeGeneratorRequestR CodeGeneratorRequestR)
  parseField 1 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "file_to_generate") $ flip snoc x
  parseField 2 LenDel = do
    x <- Decode.string
    pure $ modify (SProxy :: SProxy "parameter") $ const $ Just x
  parseField _ wireType = parseFieldUnknown wireType

  default :: CodeGeneratorRequestR
  default =
    { file_to_generate: []
    , parameter: Nothing
    , proto_file: []
    , compiler_version: Nothing
    }

  -- fs_CodeGeneratorRequest_file_to_generate =
  -- fs_CodeGeneratorRequest_parameter =
  -- fs_CodeGeneratorRequest_proto_file = SProxy :: SProxy "proto_file"
  -- fs_CodeGeneratorRequest_compiler_version = SProxy :: SProxy "compiler_version"
  -- fn_CodeGeneratorRequest_file_to_generate = UInt.fromInt 1 :: UInt
  -- fn_CodeGeneratorRequest_parameter = UInt.fromInt 2 :: UInt
  -- fn_CodeGeneratorRequest_proto_file = UInt.fromInt 15 :: UInt
  -- fn_CodeGeneratorRequest_compiler_version = UInt.fromInt 3 :: UInt




-- IMPORTANT We need to wrap our structural record types in a nominal
-- data type so that we can nest records.
-- https://github.com/purescript/documentation/blob/master/errors/CycleInTypeSynonym.md
-- And so that we can assign instances?
newtype CodeGeneratorRequest = CodeGeneratorRequest CodeGeneratorRequestR
derive instance genericCodeGeneratorRequest :: Generic CodeGeneratorRequest _
instance showCodeGeneratorRequest :: Show CodeGeneratorRequest where show = genericShow
-- | Data type for a CodeGenerationRequest message.
-- | https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.compiler.plugin.pb
type CodeGeneratorRequestR =
  { file_to_generate :: Array String -- 1
  , parameter :: Maybe String -- 2
  , proto_file :: Array FileDescriptorProto -- 15
  , compiler_version :: Maybe Version -- 3
  }
-- | The version number of protocol compiler.
newtype Version = Version VersionR
derive instance genericVersion :: Generic Version _
instance showVersion :: Show Version where show = genericShow
type VersionR =
  { major :: Maybe Int --1
  , minor :: Maybe Int --2
  , patch :: Maybe Int --3
  , suffix :: Maybe String -- 4
  }

newtype FileDescriptorProto = FileDescriptorProto FileDescriptorProtoR
derive instance genericFileDescriptorProto :: Generic FileDescriptorProto _
instance showFileDescriptorProto :: Show FileDescriptorProto where show = const "" -- TODO genericShow
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
  , extension :: Array FileDescriptorProto -- 7
  -- TODO , options :: Maybe FileOptions -- 8
  -- TODO , source_code_info :: Maybe SourceCodeInfo -- 9
  , syntax :: Maybe String -- 12
  }

newtype DescriptorProto = DescriptorProto DescriptorProtoR
derive instance genericDescriptorProto :: Generic DescriptorProto _
instance showDescriptorProto :: Show DescriptorProto where show = const "" -- TODO genericShow
-- | Describes a message type.
type DescriptorProtoR =
  { name :: Maybe String -- 1
  , field :: Array FieldDescriptorProto -- 2
  , extension :: Array FieldDescriptorProto -- 6
  , nested_type :: Array DescriptorProto -- 3
  , enum_type :: Array EnumDescriptorProto -- 4
  , extension_range :: Array DescriptorProto_ExtensionRange -- 5
  , oneof_decl :: Array OneofDescriptorProto -- 8
  -- TODO , options :: Maybe MessageOptions -- 7
  -- TODO eh who cares about reserved ranges
  }

newtype DescriptorProto_ExtensionRange = DescriptorProto_ExtensionRange DescriptorProto_ExtensionRangeR
derive instance genericDescriptorProto_ExtensionRange :: Generic DescriptorProto_ExtensionRange _
instance showDescriptorProto_ExtensionRange :: Show DescriptorProto_ExtensionRange where show = genericShow
type DescriptorProto_ExtensionRangeR =
  { start :: Maybe Int -- 1
  , end :: Maybe Int -- 2
  -- TODO , options :: Maybe ExtensionRangeOptions
  }

-- TODO
-- data ExtensionRangeOptions = ExtensionRangeOptions
--   { uninterpreted_option :: Array UninterpretedOption
--   -- ,  TODO extensions ::
--   }

data FieldDescriptorProto_Label = OPTIONAL | REQUIRED | REPEATED
derive instance genericFieldDescriptorProto_Label :: Generic FieldDescriptorProto_Label _
instance showFieldDescriptorProto_Label :: Show FieldDescriptorProto_Label where show = genericShow

-- | Describes a field within a message.
newtype FieldDescriptorProto = FieldDescriptorProto FieldDescriptorProtoR
derive instance genericFieldDescriptorProto :: Generic FieldDescriptorProto _
instance showFieldDescriptorProto :: Show FieldDescriptorProto where show = genericShow
type FieldDescriptorProtoR =
  { name :: Maybe String -- 1
  , number :: Maybe Int -- 3
  , label :: Maybe FieldDescriptorProto_Label -- 4
  , type_ :: Maybe WireType -- 5
  , type_name :: Maybe String -- 6
  , extendee :: Maybe String -- 2
  , default_value :: Maybe String -- 7
  , oneof_index :: Maybe Int -- 9
  , json_name :: Maybe String -- 10
  -- TODO , options :: Maybe FieldOptions --8
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
