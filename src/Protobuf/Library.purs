-- | Module for types to be imported by a progam which uses __protobuf__.
module Protobuf.Library
( parseExceptT
, parseEither
, parseMaybe
, module Protobuf.Common
, module Protobuf.Runtime
)
where

import Protobuf.Common (Bytes(..))
import Protobuf.Runtime (label)
import Text.Parsing.Parser (ParserT, fail)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad (class Monad, (>>=), pure)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | Lift an `ExceptT String m` computation into a `ParserT`.
parseExceptT :: forall s m a. (Monad m) => ExceptT String m a -> ParserT s m a
parseExceptT f = lift (runExceptT f) >>= case _ of
  Left err -> fail err
  Right x -> pure x

-- | Lift an `Either String` computation into a `ParserT`.
parseEither :: forall s m a. (Monad m) => Either String a -> ParserT s m a
parseEither f = case f of
  Left err -> fail err
  Right x -> pure x

-- | Lift a `Maybe` computation into a `ParserT`, with a note for
-- | the `ParseError` message in case of `Nothing`.
parseMaybe :: forall s m a. (Monad m) => String -> Maybe a -> ParserT s m a
parseMaybe message f = case f of
  Nothing -> fail message
  Just x -> pure x
