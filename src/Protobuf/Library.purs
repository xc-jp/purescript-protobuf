-- | Module for types to be imported by a progam which uses __protobuf__.
module Protobuf.Library
  ( parseMaybe
  , parseEither
  , parseExceptT
  , parseAnyMessage
  , parseAnyField
  , module ReCommon
  , module ReRuntime
  )
  where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Builder (DataBuff(..))
import Data.ArrayBuffer.Types (DataView)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UInt64 as UInt64
import Effect (Effect)
import Parsing (ParserT, fail)
import Parsing.DataView (takeN)
import Protobuf.Internal.Common (Bytes(..), WireType(..), manyArray)
import Protobuf.Internal.Common (class Default, Bytes(..), FieldNumber, label, toDefault) as ReCommon
import Protobuf.Internal.Decode as Decode
import Protobuf.Internal.Runtime (UnknownField(..))
import Protobuf.Internal.Runtime (UnknownField(..)) as ReRuntime

-- | Lift an `ExceptT String m` computation into a `ParserT`.
-- |
-- | Consumes no parsing input, does not change the parser state at all.
-- | If the `ExceptT` computation is `Left`, then this will `fail` in the
-- | `ParserT` monad at the current input `Position`.
parseExceptT :: forall s m a. (Monad m) => ExceptT String m a -> ParserT s m a
parseExceptT f =
  lift (runExceptT f)
    >>= case _ of
        Left err -> fail err
        Right x -> pure x

-- | Lift an `Either String` computation into a `ParserT`.
-- |
-- | Consumes no parsing input, does not change the parser state at all.
-- | If the `Either` computation is `Left`, then this will `fail` in the
-- | `ParserT` monad at the current input `Position`.
parseEither :: forall s m a. (Monad m) => Either String a -> ParserT s m a
parseEither f = case f of
  Left err -> fail err
  Right x -> pure x

-- | Lift a `Maybe` computation into a `ParserT`, with a note for
-- | the `ParseError` message in case of `Nothing`.
-- |
-- | Consumes no parsing input, does not change the parser state at all.
-- | If the `Maybe` computation is `Nothing`, then this will `fail` in the
-- | `ParserT` monad at the current input `Position`.
parseMaybe :: forall s m a. (Monad m) => String -> Maybe a -> ParserT s m a
parseMaybe message f = case f of
  Nothing -> fail message
  Just x -> pure x


-- | Parse one Protobuf field without the `.proto` definition.
parseAnyField :: ParserT DataView Effect UnknownField
parseAnyField = do
  Tuple fieldNumber wireType <- Decode.decodeTag32
  case wireType of
    VarInt -> UnknownVarInt fieldNumber <$> Decode.decodeUint64
    Bits64 -> UnknownBits64 fieldNumber <$> Bytes <$> View <$> takeN 8
    LenDel -> do
      len <- UInt64.toInt <$> Decode.decodeVarint64
      case len of
        Nothing -> fail "Length-delimited value of unknown field was too long."
        Just l -> UnknownLenDel fieldNumber <$> Bytes <$> View <$> takeN l
    Bits32 -> UnknownBits32 fieldNumber <$> Bytes <$> View <$> takeN 4

-- | Parse one Protobuf message without the `.proto` definition.
-- |
-- | If you have a `DataView` which you know contains one Protobuf message
-- | but you don’t have the compile-time `.proto` definition for the message,
-- | you can use this function to parse the message.
-- |
-- | If one of the `UnknownField`s is a `LenDel` field, and you know
-- | from the `FieldNumber` that
-- | the field should be a nested message field, then you can
-- | try to recursively `parseAnyMessage` on the `LenDel` field value.
parseAnyMessage :: ParserT DataView Effect (Array UnknownField)
parseAnyMessage = manyArray $ parseAnyField
