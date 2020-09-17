-- | Entry point for the code-generating executable `protoc` plugin. See the
-- | package README for instructions on how to run the code generator.
-- |
-- | The funny thing about writing a `protoc` compiler plugin codec is that it
-- | bootstraps itself. We just have to write enough of the compiler plugin codec
-- | that it can handle the `plugin.proto` and `descriptor.proto` files, and
-- | then we call the compiler plugin on these `.proto` files and the compiler
-- | plugin codec generates the rest of itself.
-- |
-- | Then we can delete the hand-written code and generate code to replace it
-- | with this command.
-- |
-- |     protoc --purescript_out=./src/ProtocPlugin google/protobuf/compiler/plugin.proto
-- |
-- | See
-- | * https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.compiler.plugin.pb
-- | * https://developers.google.com/protocol-buffers/docs/reference/cpp/google.protobuf.descriptor.pb
module Conformance.Main (main) where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..)) --,fromMaybe,maybe)
import Data.Either (Either(..))
-- import Data.Array (concatMap, mapMaybe)
-- import Data.Array as Array
-- import Data.String as String
-- import Data.String.Pattern as String.Pattern

import Text.Parsing.Parser (runParserT)
import Text.Parsing.Parser.DataView (anyInt32le)
import Data.ArrayBuffer.Builder (execPut, subBuilder, putInt32le)
import Data.ArrayBuffer.Builder as Builder
import Control.Monad.Writer (tell)
import Data.ArrayBuffer.DataView as DV
-- import Data.ArrayBuffer.ArrayBuffer as AB
import Node.Process (stdin, stdout, stderr, exit)
import Node.Stream (read, write, writeString, onReadable)
import Node.Buffer (toArrayBuffer, fromArrayBuffer)
import Node.Encoding (Encoding(..))
-- import Node.Path (basenameWithoutExt)

-- import Google.Protobuf.Compiler.Plugin
--   ( CodeGeneratorRequest(..), parseCodeGeneratorRequest
--   , CodeGeneratorResponse(..), putCodeGeneratorResponse
--   , CodeGeneratorResponse_File(..)
--   )
--
-- import Google.Protobuf.Descriptor
--   ( FileDescriptorProto(..)
--   , DescriptorProto(..)
--   , FieldDescriptorProto(..)
--   , OneofDescriptorProto(..)
--   , EnumDescriptorProto(..)
--   , EnumValueDescriptorProto(..)
--   , FieldDescriptorProto_Type(..)
--   , FieldDescriptorProto_Label(..)
--   )

import Conformance.Conformance
  ( ConformanceRequest(..), parseConformanceRequest
  , ConformanceResponse(..), putConformanceResponse
  , ConformanceResponse_Result(..)
  )

bailOutMaybe :: forall a. String -> Effect (Maybe a) -> Effect a
bailOutMaybe err thing =
  thing >>= case _ of
    Nothing -> do
      -- writeString stderr UTF8 err (void $ exit 1)
      void $ writeString stderr UTF8 err (pure unit)
      exit 1
    Just x -> pure x

bailOutEither :: forall a l. Show l => Effect (Either l a) -> Effect a
bailOutEither thing = thing >>= case _ of
    Left err -> do
      void $ writeString stderr UTF8 (show err) (pure unit)
      exit 1
    Right x -> pure x


-- https://github.com/protocolbuffers/protobuf/blob/master/conformance/conformance_test_runner.cc
main :: Effect Unit
main = do
  -- void $ writeString stderr UTF8 "Beginning Conformance Check" (pure unit)
  onReadable stdin $ do
    -- void $ writeString stderr UTF8 "stdin readable" (pure unit)
    msglenbuf <- bailOutMaybe "No message length" $ read stdin (Just 4)
    msglendv <- DV.whole <$> toArrayBuffer msglenbuf
    msglen <- bailOutEither $ runParserT msglendv $ anyInt32le
    -- void $ writeString stderr UTF8 (show msglen) (pure unit)

    stdinbuf <- bailOutMaybe "No stdin" $ read stdin (Just msglen)
    -- stdinbufMay <- read stdin Nothing
    -- case stdinbufMay of
    --   Nothing -> pure unit
    --   Just stdinbuf -> do
    stdinab <- toArrayBuffer stdinbuf
    let stdinview = DV.whole stdinab
    request <- bailOutEither $ runParserT stdinview $ parseConformanceRequest $ DV.byteLength stdinview

    -- case requestParsed of
    --   Left err -> void $ writeString stderr UTF8 (show err) (pure unit)
    --   Right request -> do
        -- Uncomment this line to write the parsed declarations to stderr.
        -- void $ writeString stderr UTF8 (show request) (pure unit)

    let response = reply request
    responseab <- execPut $ do
       responsesub <- subBuilder $ putConformanceResponse response
       putInt32le $ Builder.length responsesub
       tell responsesub
    responsebuffer <- fromArrayBuffer responseab
    void $ write stdout responsebuffer (pure unit)

reply :: ConformanceRequest -> ConformanceResponse
reply (ConformanceRequest
  { requested_output_format: Nothing -- :: Maybe.Maybe WireFormat
  , message_type: Nothing -- :: Maybe.Maybe String
  , test_category: Nothing -- :: Maybe.Maybe TestCategory
  , jspb_encoding_options: Nothing -- :: Maybe.Maybe JspbEncodingConfig
  , print_unknown_fields: Nothing -- :: Maybe.Maybe Boolean
  , payload: Nothing -- :: Maybe.Maybe ConformanceRequest_Payload
  }) = ConformanceResponse
  { result: Nothing
  }

reply _ = ConformanceResponse
  { result: Just $ ConformanceResponse_Result_Skipped "just because"
  }

