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

import Conformance.Conformance (ConformanceRequest(..), ConformanceResponse(..), ConformanceResponse_Result(..), parseConformanceRequest, putConformanceResponse)
import Conformance.Conformance as C
import Control.Monad.Writer (tell)
import Data.ArrayBuffer.Builder (execPut, subBuilder, putInt32le)
import Data.ArrayBuffer.Builder as Builder
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Buffer (toArrayBuffer, fromArrayBuffer)
import Node.Encoding (Encoding(..))
import Node.Process (stdin, stdout, stderr, exit)
import Node.Stream (onFinish, onReadable, read, write, writeString)
import ProtobufTestMessages.Proto3.TestMessagesProto3 as T3
import Text.Parsing.Parser (runParserT)
import Text.Parsing.Parser.DataView (anyInt32le)

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
    -- msglenbuf <- bailOutMaybe "No message length" $ read stdin (Just 4)
    -- msglendv <- DV.whole <$> toArrayBuffer msglenbuf
    msglendv <- map DV.whole $ toArrayBuffer =<< (bailOutMaybe "No message length" $ read stdin (Just 4))
    msglen <- bailOutEither $ runParserT msglendv $ anyInt32le
    -- void $ writeString stderr UTF8 (show msglen) (pure unit)

    stdinview <- map DV.whole $ toArrayBuffer =<< (bailOutMaybe "No stdin" $ read stdin (Just msglen))
    -- stdinbufMay <- read stdin Nothing
    -- case stdinbufMay of
    --   Nothing -> pure unit
    --   Just stdinbuf -> do
    request <- bailOutEither $ runParserT stdinview $ parseConformanceRequest $ DV.byteLength stdinview

    -- case requestParsed of
    --   Left err -> void $ writeString stderr UTF8 (show err) (pure unit)
    --   Right request -> do
        -- Uncomment this line to write the parsed declarations to stderr.
        -- void $ writeString stderr UTF8 (show request) (pure unit)

    response <- reply request

    case response of
      (C.ConformanceResponse
        { result:  Just (ConformanceResponse_Result_Runtime_error err)
        }) -> void $ writeString stderr UTF8 (err <> "\n") (pure unit)
      _ -> pure unit
    responseab <- execPut $ do
       responsesub <- subBuilder $ putConformanceResponse response
       putInt32le $ Builder.length responsesub
       tell responsesub
    responsebuffer <- fromArrayBuffer responseab
    void $ write stdout responsebuffer (pure unit)

reply :: ConformanceRequest -> Effect ConformanceResponse
reply (ConformanceRequest
  { requested_output_format: Just C.WireFormat_PROTOBUF
  , message_type: Just "protobuf_test_messages.proto3.TestAllTypesProto3" -- :: Maybe.Maybe String
  , test_category: _ -- :: Maybe.Maybe TestCategory
  , jspb_encoding_options: _ -- :: Maybe.Maybe JspbEncodingConfig
  , print_unknown_fields: _ -- :: Maybe.Maybe Boolean
  , payload: Just (C.ConformanceRequest_Payload_Protobuf_payload receipt_payload)
  }) = do
    let dv = DV.whole receipt_payload
    parsed <- runParserT dv $ T3.parseTestAllTypesProto3 $ DV.byteLength dv
    case parsed of
      Left err ->
        pure $ ConformanceResponse
          { result: Just $ ConformanceResponse_Result_Parse_error $ show err
          }
      Right x -> do
        reply_payload <- execPut $ T3.putTestAllTypesProto3 x
        pure $ ConformanceResponse
          { result: Just $ ConformanceResponse_Result_Protobuf_payload reply_payload
          }
reply (ConformanceRequest
  { requested_output_format: _ -- :: Maybe.Maybe WireFormat
  , message_type: Just "protobuf_test_messages.proto2.TestAllTypesProto2" -- :: Maybe.Maybe String
  , test_category: _ -- :: Maybe.Maybe TestCategory
  , jspb_encoding_options: _ -- :: Maybe.Maybe JspbEncodingConfig
  , print_unknown_fields: _ -- :: Maybe.Maybe Boolean
  , payload: _ -- Just (C.ConformanceRequest_Payload_Protobuf_payload arraybuffer)
  }) = pure $ ConformanceResponse
  -- { result: Just $ ConformanceResponse_Result_Runtime_error $ "runtime " <> message_type
  { result: Just $ ConformanceResponse_Result_Skipped "Skipped proto2"
  }

reply _ = pure $ ConformanceResponse
  { result: Just $ ConformanceResponse_Result_Skipped "Not enough information"
  }

