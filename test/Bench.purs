module Test.Bench where

import Prelude
import Data.Array (range)
import Data.ArrayBuffer.ArrayBuffer (byteLength, empty)
import Data.ArrayBuffer.Builder (execPut)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed as Typed
import Data.ArrayBuffer.Types (Float32Array)
import Data.Float32 (fromNumber')
import Data.Foldable (for_)
import Effect (Effect, forE)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Performance.Minibench (bench, benchWith)
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode
import Protobuf.Runtime as Runtime
import Text.Parsing.Parser (runParserT)

main :: Effect Unit
main = do
  log "\nWarmup bench"
  bench \_ -> forE 0 10000 (\_ -> pure unit)
  buf10e3 <-
    execPut do
      for_ (range 1 999) $ \_ -> Encode.encodeFloat (fromNumber' 1.0)
  log "\nfloatArray 10e3"
  benchWith 100
    $ \_ ->
        void $ unsafePerformEffect
          $ runParserT (whole buf10e3) (Decode.decodeFloatArray (byteLength buf10e3))
  buf10e4 <- empty (4 * 10000)
  buf10e4Float :: Float32Array <- Typed.whole buf10e4
  Typed.fill (fromNumber' 1.0) 0 9999 buf10e4Float
  log "\nfloatArray 10e4"
  benchWith 100
    $ \_ ->
        void $ unsafePerformEffect
          $ runParserT (whole buf10e4) (Decode.decodeFloatArray (byteLength buf10e4))
  log "\nmanyLength float 10e4"
  benchWith 100
    $ \_ ->
        void $ unsafePerformEffect
          $ runParserT (whole buf10e4) (Runtime.manyLength Decode.decodeFloat (byteLength buf10e4))
  buf10e5 <- empty (4 * 100000)
  buf10e5Float :: Float32Array <- Typed.whole buf10e5
  Typed.fill (fromNumber' 1.0) 0 99999 buf10e5Float
  log "\nfloatArray 10e5"
  benchWith 100
    $ \_ ->
        void $ unsafePerformEffect
          $ runParserT (whole buf10e5) (Decode.decodeFloatArray (byteLength buf10e5))
  log "\nmanyLength float 10e5"
  benchWith 100
    $ \_ ->
        void $ unsafePerformEffect
          $ runParserT (whole buf10e5) (Runtime.manyLength Decode.decodeFloat (byteLength buf10e5))
  buf10e6 <- empty (4 * 1000000)
  buf10e6Float :: Float32Array <- Typed.whole buf10e6
  Typed.fill (fromNumber' 7.0) 0 1000000 buf10e6Float
  log "\nfloatArray 1e6"
  benchWith 100 \_ ->
    unsafePerformEffect do
      void $ runParserT (whole buf10e6) (Decode.decodeFloatArray (byteLength buf10e6))
