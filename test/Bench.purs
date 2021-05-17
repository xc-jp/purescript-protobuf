module Test.Bench where

import Prelude

import Data.Array (range, replicate)
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.Builder (execPut)
import Data.ArrayBuffer.DataView (whole)
import Data.Float32 (fromNumber')
import Data.Foldable (for_)
import Data.Unfoldable (replicateA)
import Effect (Effect, forE)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Performance.Minibench (bench)
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode
import Protobuf.Runtime (manyLength)
import Text.Parsing.Parser (runParserT)

main :: Effect Unit
main = do

  log "\nWarmup bench"
  bench \_ -> forE 0 10000 (\_ -> pure unit)

  buf1000 <- execPut do
    -- void $ replicateA 10000 $ Encode.float' (fromNumber' 1.0)
    for_ (range 1 1000) $ \_ -> Encode.float' (fromNumber' 1.0)

  log "\nmanyLength float 1000"
  bench $ \_ -> void $ unsafePerformEffect $ runParserT (whole buf1000) do
    manyLength (Decode.float) (byteLength buf1000)

  buf5000 <- execPut do
    for_ (range 1 5000) $ \_ -> Encode.float' (fromNumber' 1.0)

  log "\nmanyLength float 5000"
  bench $ \_ -> void $ unsafePerformEffect $ runParserT (whole buf5000) do
    manyLength (Decode.float) (byteLength buf5000)