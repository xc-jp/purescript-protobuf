module Test.Bench where

import Prelude

import Data.Array (range, replicate)
import Data.ArrayBuffer.ArrayBuffer (byteLength, empty)
import Data.ArrayBuffer.Builder (execPut)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed as Typed
import Data.ArrayBuffer.Types (Float32Array)
import Data.Float32 (fromNumber')
import Data.Foldable (for_)
import Data.Unfoldable (replicateA)
import Effect (Effect, forE)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Performance.Minibench (bench, benchWith)
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode
import Protobuf.Runtime (manyLength)
import Text.Parsing.Parser (runParserT)

main :: Effect Unit
main = do

  log "\nWarmup bench"
  bench \_ -> forE 0 10000 (\_ -> pure unit)

  buf10e3 <- execPut do
    -- void $ replicateA 10000 $ Encode.float' (fromNumber' 1.0)
    for_ (range 1 999) $ \_ -> Encode.float' (fromNumber' 1.0)

  log "\nmanyLength float 10e3"
  benchWith 100 $ \_ -> void $ unsafePerformEffect $ runParserT (whole buf10e3) do
    manyLength (Decode.float) (byteLength buf10e3)

  -- buf5000 <- execPut do
  --   for_ (range 1 5000) $ \_ -> Encode.float' (fromNumber' 1.0)

  -- log "\nmanyLength float 5000"
  -- bench $ \_ -> void $ unsafePerformEffect $ runParserT (whole buf5000) do
  --   manyLength (Decode.float) (byteLength buf5000)

  buf10e4 <- empty (4*10000)
  buf10e4Float :: Float32Array <- Typed.whole buf10e4
  Typed.fill (fromNumber' 1.0) 0 9999 buf10e4Float

  log "\nmanyLength float 10e4"
  benchWith 100 $ \_ -> void $ unsafePerformEffect $ runParserT (whole buf10e4) do
    manyLength (Decode.float) (byteLength buf10e4)

  buf10e5 <- empty (4*100000)
  buf10e5Float :: Float32Array <- Typed.whole buf10e5
  Typed.fill (fromNumber' 1.0) 0 99999 buf10e5Float
  -- buf10e5
    -- void $ replicateA 10000 $ Encode.float' (fromNumber' 1.0)
    -- for_ (range 1 1000) $ \_ -> Encode.float' (fromNumber' 1.0)
    -- forE 0 99999 $ \_ ->

  log "\nmanyLength float 10e5"
  benchWith 100 $ \_ -> void $ unsafePerformEffect $ runParserT (whole buf10e5) do
    manyLength (Decode.float) (byteLength buf10e5)