-- | Entry point for the conformance test runner.
module Conformance.Main (main) where

import Prelude

import Conformance.Conformance (ConformanceRequest(..), ConformanceResponse, ConformanceResponse_Result(..), mkConformanceResponse, parseConformanceRequest, parseConformanceResponse, putConformanceResponse)
import Conformance.Conformance as C
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.Writer (tell)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Builder (DataBuff(..), execPut, execPutM, putInt32le, subBuilder, toView)
import Data.ArrayBuffer.Builder as Builder
import Data.ArrayBuffer.DataView as DV
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_, message, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer, toArrayBuffer, fromArrayBuffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS (BufferLength, FileDescriptor, FileFlags(..))
import Node.FS.Aff (fdAppend, fdNext, fdOpen, fdRead, fdWrite)
import Parsing (ParseError(..), runParserT)
import Parsing.DataView (anyInt32le)
import Protobuf.Library (Bytes(..))
import ProtobufTestMessages.Proto3.TestMessagesProto3 as T3
import Unsafe.Coerce (unsafeCoerce)

-- bailOutMaybe :: forall a. String -> Effect (Maybe a) -> Effect a
-- bailOutMaybe err thing =
--   thing >>= case _ of
--     Nothing -> do
--       void $ writeString stderr UTF8 err (\_ -> pure unit)
--       exit 1
--     Just x -> pure x
--
-- bailOutEither :: forall a l. Show l => Effect (Either l a) -> Effect a
-- bailOutEither thing = thing >>= case _ of
--     Left err -> do
--       void $ writeString stderr UTF8 (show err) (\_ -> pure unit)
--       exit 1
--     Right x -> pure x

-- readFull :: FileDescriptor -> BufferLength -> Aff Buffer
-- readFull fd len = do
--   buf <- liftEffect $ Buffer.create len
--   _ <- fdNext fd buf
--   pure buf

-- appendFull :: FileDescriptor -> Buffer -> Aff Unit
-- appendFull fd buf = do
--   void $ fdAppend fd buf

-- readFull :: FileDescriptor -> BufferLength -> Effect Buffer
-- readFull fd len = do
--   buf <- Buffer.create len
--   flip tailRecM 0 \red ->
--     if red < len then do
--       r <- fdRead fd buf red (len - red) Nothing
--       pure $ Loop (red + r)
--     else
--       pure $ Done buf
--
-- appendFull :: FileDescriptor -> Buffer -> Effect Unit
-- appendFull fd buf = do
--   len <- Buffer.size buf
--   flip tailRecM 0 \rot ->
--     if rot < len then do
--       r <- fdWrite fd buf rot (len - rot) Nothing
--       pure $ Loop (rot + r)
--     else
--       pure $ Done unit

readFull :: FileDescriptor -> BufferLength -> Aff Buffer
readFull fd len = do
  buf <- liftEffect1 Buffer.create len
  flip tailRecM 0 \red ->
    if red < len then do
      r <- fdRead fd buf red (len - red) Nothing
      pure $ Loop (red + r)
    else
      pure $ Done buf

appendFull :: FileDescriptor -> Buffer -> Aff Unit
appendFull fd buf = do
  len <- liftEffect1 Buffer.size buf
  flip tailRecM 0 \rot ->
    if rot < len then do
      r <- fdWrite fd buf rot (len - rot) Nothing
      pure $ Loop (rot + r)
    else
      pure $ Done unit

-- infixr 1 map as $<<

liftEffect1 :: forall a b. (a -> Effect b) -> a -> Aff b
liftEffect1 f x = liftEffect (f x)

liftEffect2 :: forall a b c. (a -> b -> Effect c) -> a -> b -> Aff c
liftEffect2 f x y = liftEffect (f x y)

stdin :: FileDescriptor
stdin = unsafeCoerce 0
stdout :: FileDescriptor
stdout = unsafeCoerce 1
stderr :: FileDescriptor
stderr = unsafeCoerce 2

-- https://github.com/protocolbuffers/protobuf/blob/master/conformance/conformance_test_runner.cc
main :: Effect Unit
main = launchAff_ do
  action <- try do
    -- stdin <- fdOpen "/dev/stdin" R Nothing
    -- stdout <- fdOpen "/dev/stdout" W Nothing
    -- stderr <- fdOpen "/dev/stderr" W Nothing
    -- stdin <- liftEffect fdStdin
    -- stdout <- liftEffect fdStdout
    -- stderr <- liftEffect fdStderr


    msglenView <- map DV.whole $ liftEffect1 toArrayBuffer =<< readFull stdin 4
    -- msglenView <- DV.whole $<< toArrayBuffer =<< readFull stdin 4
    -- msglenBuf <- liftEffect $ Buffer.create 4
    -- _ <- fdNext stdin msglenBuf
    -- msglenView <- map DV.whole $ toArrayBuffer msglenBuf
    runParserT msglenView anyInt32le >>= case _ of
      Left err ->
        -- appendFull stderr =<< liftEffect2 Buffer.fromString (show err <> "\n") UTF8
        -- void $ fdAppend stderr =<< Buffer.fromString (show err <> "\n") UTF8
        pure unit
      Right msglen -> do
        -- void $ fdAppend stderr =<< Buffer.fromString ("Message length " <> show msglen <> "\n") UTF8
        -- appendFull stderr =<< liftEffect2 Buffer.fromString ("Message length " <> show msglen <> "\n") UTF8
        msgView <- map DV.whole $ liftEffect1 toArrayBuffer =<< readFull stdin msglen
        -- msgBuf <- Buffer.create msglen
        -- _ <- fdNext stdin msgBuf
        -- msgView <- map DV.whole $ toArrayBuffer msgBuf
        -- appendFull stderr =<< liftEffect2 Buffer.fromString ("msgView " <> show (DV.byteLength msgView) <> "\n") UTF8
        runParserT msgView (parseConformanceRequest $ DV.byteLength msgView) >>= case _ of
          Left err ->
            -- void $ appendFull stderr =<< liftEffect2 Buffer.fromString (show err <> "\n") UTF8
            pure unit
          Right request -> do
            -- appendFull stderr =<< liftEffect2 Buffer.fromString (show request <> "\n") UTF8
            Tuple errMay response <- reply request

            fromMaybe (pure unit) $ errMay <#> \err -> do
              -- appendFull stderr =<< liftEffect2 Buffer.fromString (show err <> "\n") UTF8
              -- appendFull stderr =<< liftEffect2 Buffer.fromString "" UTF8
              pure unit

            appendFull stdout =<< liftEffect1 fromArrayBuffer =<< execPutM do
            -- responsesize <- fdAppend stdout =<< liftEffect1 fromArrayBuffer =<< execPutM do
              responsesub <- subBuilder $ putConformanceResponse response
              putInt32le $ Builder.length responsesub
              -- liftEffect $ appendFull stderr =<< Buffer.fromString ("responsesub " <> show (Builder.length responsesub) <> "\n") UTF8
              -- liftAff $ appendFull stderr =<< liftEffect2 Buffer.fromString ("responsesub " <> show (Builder.length responsesub) <> "\n") UTF8
              tell responsesub

            -- appendFull stdout =<< liftEffect2 Buffer.fromString "" UTF8
            -- appendFull stderr =<< liftEffect2 Buffer.fromString ("responsesize " <> show responsesize <> "\n") UTF8

            -- fdFlush stdout
            -- delay (Milliseconds 1000.0)

            -- ab <- execPutM do
            --   responsesub <- subBuilder $ putConformanceResponse response
            --   putInt32le $ Builder.length responsesub
            --   -- liftEffect $ appendFull stderr =<< Buffer.fromString ("responsesub " <> show (Builder.length responsesub) <> "\n") UTF8
            --   liftAff $ appendFull stderr =<< liftEffect2 Buffer.fromString ("responsesub " <> show (Builder.length responsesub) <> "\n") UTF8
            --   tell responsesub

            -- resp <- runParserT (DV.whole ab) do
            --   l <- anyInt32le
            --   parseConformanceResponse l

            -- appendFull stderr =<< liftEffect2 Buffer.fromString (show resp <> "\n") UTF8

            -- appendFull stdout =<< liftEffect1 fromArrayBuffer ab
    delay $ Milliseconds 1000.0
  case action of
    Right _ -> do
      appendFull stderr =<< liftEffect2 Buffer.fromString ("SUCCESS\n") UTF8
      pure unit
    Left err -> do
      appendFull stderr =<< liftEffect2 Buffer.fromString ("FAILURE " <> message err <> "\n") UTF8
      pure unit






  -- onReadable stdin $ do
  --   msglendv <- map DV.whole $ toArrayBuffer =<< (bailOutMaybe "No message length" $ read stdin (Just 4))
  --   msglen <- bailOutEither $ runParserT msglendv $ anyInt32le
  --   stdinview <- map DV.whole $ toArrayBuffer =<< (bailOutMaybe "No stdin" $ read stdin (Just msglen))
  --   request <- bailOutEither $ runParserT stdinview $ parseConformanceRequest $ DV.byteLength stdinview
  --   response <- reply request

  --   responseab <- execPut $ do
  --      responsesub <- subBuilder $ putConformanceResponse response
  --      putInt32le $ Builder.length responsesub
  --      tell responsesub

  --   responsebuffer <- fromArrayBuffer responseab
  --   void $ write stdout responsebuffer (\_ -> pure unit)

reply :: ConformanceRequest -> Aff (Tuple (Maybe ParseError) ConformanceResponse)

reply (ConformanceRequest
  { requested_output_format: _ -- Just C.WireFormat_PROTOBUF
  , message_type: Just "protobuf_test_messages.proto2.TestAllTypesProto2"
  , test_category: _ -- Just C.TestCategory_BINARY_TEST
  , jspb_encoding_options: _ -- :: Maybe.Maybe JspbEncodingConfig
  , print_unknown_fields: _ -- :: Maybe.Maybe Boolean
  , payload: _ -- Just (C.ConformanceRequest_Payload_Protobuf_payload arraybuffer)
  }) = do
    -- appendFull stderr =<< Buffer.fromString (show (unwrap request).message_type <> "\n") UTF8
    -- appendFull stderr =<< liftEffect2 Buffer.fromString ("Skipped proto2\n") UTF8
    pure $ Tuple Nothing $ mkConformanceResponse
      { result: Just $ ConformanceResponse_Result_Skipped "Skipped proto2\n"
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

    -- appendFull stderr =<< liftEffect2 Buffer.fromString ("receipt_payload " <> show (DV.byteLength dv) <> "\n") UTF8

    parsed <- runParserT dv $ T3.parseTestAllTypesProto3 $ DV.byteLength dv
    case parsed of
      Left err ->
        pure $ Tuple (Just err) $ mkConformanceResponse
          { result: Just $ ConformanceResponse_Result_Parse_error $ show err
          }
      Right x -> do
        reply_payload <- execPutM $ T3.putTestAllTypesProto3 x

        -- appendFull stderr =<< Buffer.fromString ("reply_payload " <> show (AB.byteLength reply_payload) <> "\n") UTF8

        pure $ Tuple Nothing $ mkConformanceResponse
          { result: Just $ ConformanceResponse_Result_Protobuf_payload (Bytes $ Buff reply_payload)
          }

reply _ = pure $ Tuple Nothing $ mkConformanceResponse
  { result: Just $ ConformanceResponse_Result_Skipped "Not enough information"
  }

