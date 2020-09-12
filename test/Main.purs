module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Text.Parsing.Parser (ParserT, runParserT)
import Data.ArrayBuffer.Builder (Put, execPut)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (DataView, ArrayBuffer)
import Data.Float32 as Float32
import Data.Long.Internal as Long
import Data.UInt as UInt
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

import My.Module.Test as Test
import Test.Assert (assert')

billion' :: Int
billion' = -1000000000
billion :: UInt.UInt
billion = UInt.fromInt 1000000000
billion2' :: Long.Long Long.Signed
billion2' = Long.fromLowHighBits billion' billion'
billion2 :: Long.Long Long.Unsigned
billion2 = Long.fromLowHighBits 1000000000 1000000000

-- asBytes :: ArrayBuffer -> Effect (Array Int)
-- asBytes x = do
--   x' :: Uint8Array <- AT.whole x
--   map UInt.toInt <$> AT.toArray x'

main :: Effect Unit
main = do
  -- log $ show $ UInt.fromInt billion'
  -- log $ show $ UInt.fromInt <<< negate <<< UInt.toInt $ UInt.fromInt 1
  -- log "🍝"
  -- log "You should add some tests."

  -- ab <- execPut $ putInt32be billion'

  let msg1 = (Test.Msg1
    { f1: Just 1234.5
    , f2: Float32.fromNumber 345.6
    , f3: Just billion'
    , f4: Just billion2'
    , f5: Just billion
    , f6: Just billion2
    , f7: Just billion'
    , f8: Just billion2'
    , f9: Just billion
    , f10: Just billion2
    , f11: Just billion2'
    , f12: Just true
    , f13: Just "🍝"
    -- -- , f14 :: Maybe.Maybe ArrayBuffer.Types.ArrayBuffer
    , f15: Just Test.Msg1_E1_Three
    })

  ab <- execPut $ Test.putMsg1 msg1
  let dv = DV.whole ab

  parseResult <- runParserT dv $ Test.parseMsg1 $ DV.byteLength dv

  case parseResult of
    Left err -> log $ "Parse fail " <> show err
    Right msg1' -> assert' "msg1 roundtrip" $ genericEq msg1 msg1'



