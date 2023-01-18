-- | Entry point for the conformance test program.
module Conformance.Main (main) where

import Prelude

import Conformance.Conformance (ConformanceRequest(..), ConformanceResponse, ConformanceResponse_Result(..), mkConformanceResponse, parseConformanceRequest, putConformanceResponse)
import Conformance.Conformance as C
import Control.Monad.Rec.Class (class MonadRec, untilJust)
import Control.Monad.Writer (tell)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Builder (DataBuff(..), execPutM, putInt32le, subBuilder, toView)
import Data.ArrayBuffer.Builder as Builder
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (error, runAff_, throwError)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Node.Buffer (Buffer, concat, fromArrayBuffer, toArrayBuffer)
import Node.Process (stdin, stdout)
import Node.Stream.Aff (readN, write)
import Parsing (runParserT)
import Parsing.DataView (anyInt32le)
import Protobuf.Library (Bytes(..))
import ProtobufTestMessages.Proto3.TestMessagesProto3 as T3
import Unsafe.Coerce (unsafeCoerce)


-- https://github.com/protocolbuffers/protobuf/blob/5e7b709564403f1fe22476f7efa1eeff61e995cb/conformance/conformance_test_runner.cc#L48-L54
--
-- Every test consists of a ConformanceRequest/ConformanceResponse
-- request/reply pair.  The protocol on the pipe is simply:
--
--   1. tester sends 4-byte length N (little endian)
--   2. tester sends N bytes representing a ConformanceRequest proto
--   3. testee sends 4-byte length M (little endian)
--   4. testee sends M bytes representing a ConformanceResponse proto

main :: Effect Unit
main = runAff_ (either (unsafeCoerce >>> Console.error) (\_ -> pure unit)) $ untilJust do
  {buffers:msgLenBuf, readagain:readagain1} <- readN stdin 4
  msgLenAB :: ArrayBuffer <- liftEffect $ toArrayBuffer =<< concat msgLenBuf
  if AB.byteLength msgLenAB == 0 || not readagain1 then
    pure (Just unit)
  else do
    runParserT (DV.whole msgLenAB) anyInt32le >>= case _ of
      Left err -> throwError $ unsafeCoerce err
      Right msgLen | msgLen < 0 || msgLen > 1000000000 -> do
        throwError $ error $ "Error msgLen " <> show msgLen
      Right msgLen -> do
        {buffers:msgBuf} <- readN stdin msgLen
        msgAB <- liftEffect $ toArrayBuffer =<< concat msgBuf
        runParserT (DV.whole msgAB) (parseConformanceRequest msgLen) >>= case _ of
          Left err -> throwError $ unsafeCoerce err
          Right request -> do
            response <- reply request
            responseAB :: ArrayBuffer <- execPutM $ do
              responsesub <- subBuilder $ putConformanceResponse response
              putInt32le $ Builder.length responsesub
              tell responsesub
            responseBuf :: Buffer <- liftEffect $ fromArrayBuffer responseAB
            write stdout [responseBuf]
    pure Nothing

reply :: forall m. MonadEffect m => MonadRec m => ConformanceRequest -> m ConformanceResponse

reply (ConformanceRequest
  { requested_output_format: _ -- :: Maybe.Maybe WireFormat
  , message_type: Just "protobuf_test_messages.proto2.TestAllTypesProto2"
  , test_category: _ -- :: Maybe.Maybe TestCategory
  , jspb_encoding_options: _ -- :: Maybe.Maybe JspbEncodingConfig
  , print_unknown_fields: _ -- :: Maybe.Maybe Boolean
  , payload: _ -- Just (C.ConformanceRequest_Payload_Protobuf_payload arraybuffer)
  }) = pure $ mkConformanceResponse
  { result: Just $ ConformanceResponse_Result_Skipped "Skipped proto2"
  }

reply (ConformanceRequest
  { requested_output_format: Just C.WireFormat_PROTOBUF
  , message_type: _ -- Just "protobuf_test_messages.proto3.TestAllTypesProto3"
  , test_category: Just C.TestCategory_BINARY_TEST
  , jspb_encoding_options: _ -- :: Maybe.Maybe JspbEncodingConfig
  , print_unknown_fields: _ -- :: Maybe.Maybe Boolean
  , payload: Just (C.ConformanceRequest_Payload_Protobuf_payload (Bytes receipt_payload))
  }) = do
    let dv = toView receipt_payload
    parsed <- runParserT dv $ T3.parseTestAllTypesProto3 $ DV.byteLength dv
    case parsed of
      Left err ->
        pure $ mkConformanceResponse
          { result: Just $ ConformanceResponse_Result_Parse_error $ show err
          }
      Right x -> do
        reply_payload <- execPutM $ T3.putTestAllTypesProto3 x
        pure $ mkConformanceResponse
          { result: Just $ ConformanceResponse_Result_Protobuf_payload (Bytes $ Buff reply_payload)
          }

reply _ = pure $ mkConformanceResponse
  { result: Just $ ConformanceResponse_Result_Skipped "Not enough information"
  }

