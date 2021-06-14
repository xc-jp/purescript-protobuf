-- | Module for types to be imported by a progam which uses __protobuf__.
module Protobuf.Library
  ( parseExceptT
  , parseEither
  , parseMaybe
  , module Protobuf.Common
  , module Protobuf.Runtime
  ) where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Protobuf.Common (Bytes(..), class Default, default, isDefault, fromDefault, toDefault)
import Protobuf.Runtime (label, manyLength)
import Text.Parsing.Parser (ParserT, fail)

-- | Lift an `ExceptT String m` computation into a `ParserT`.
-- |
-- | Consumes no parsing input, does not change the parser state at all.
-- | If the `ExceptT` computation is `Left`, then this will `fail` in the
-- | `ParserT` monad.
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
-- | `ParserT` monad.
parseEither :: forall s m a. (Monad m) => Either String a -> ParserT s m a
parseEither f = case f of
  Left err -> fail err
  Right x -> pure x

-- | Lift a `Maybe` computation into a `ParserT`, with a note for
-- | the `ParseError` message in case of `Nothing`.
-- |
-- | Consumes no parsing input, does not change the parser state at all.
-- | If the `Maybe` computation is `Nothing`, then this will `fail` in the
-- | `ParserT` monad.
parseMaybe :: forall s m a. (Monad m) => String -> Maybe a -> ParserT s m a
parseMaybe message f = case f of
  Nothing -> fail message
  Just x -> pure x
