module Test.Main where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Unfoldable (replicate)
import Data.Foldable (and)
import Data.Array (catMaybes, zipWith)

import Text.Parsing.Parser (runParserT)
import Data.ArrayBuffer.Builder (execPut)
import Data.ArrayBuffer.DataView as DV
import Data.Float32 as Float32
import Data.Long.Internal as Long
import Data.UInt as UInt
import Data.Generic.Rep.Eq (genericEq)

import Generated.TestMsgs as TestMsgs
import Test.Assert (assert')

billion' :: Int
billion' = -1000000000
billion :: UInt.UInt
billion = UInt.fromInt 1000000000
billion2' :: Long.Long Long.Signed
billion2' = Long.fromLowHighBits billion' billion'
billion2 :: Long.Long Long.Unsigned
billion2 = Long.fromLowHighBits 1000000000 1000000000

main :: Effect Unit
main = do

  let msg1 = (TestMsgs.Msg1
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
    , f15: Just TestMsgs.Msg1_E1_Three
    })

  ab1 <- execPut $ TestMsgs.putMsg1 msg1
  let dv1 = DV.whole ab1
  parseResult1 <- runParserT dv1 $ TestMsgs.parseMsg1 $ DV.byteLength dv1
  case parseResult1 of
    Left err -> assert' ("msg1 parse " <> show err) false
    Right msg1' -> assert' "msg1 roundtrip" $ genericEq msg1 msg1'

  let msg2 = (TestMsgs.Msg2
    { f1: replicate 3 1234.5
    , f2: catMaybes [Float32.fromNumber 345.6, Float32.fromNumber 345.6]
    , f3: replicate 3 billion'
    , f4: replicate 3 billion2'
    , f5: replicate 3 billion
    , f6: replicate 3 billion2
    , f7: replicate 3 billion'
    , f8: replicate 3 billion2'
    , f9: replicate 3 billion
    , f10: replicate 3 billion2
    , f11: replicate 3 billion2'
    , f12: replicate 3 true
    , f13: replicate 3 "🍝"
    -- -- , f14 :: Maybe.Maybe ArrayBuffer.Types.ArrayBuffer
    , f15: replicate 3 TestMsgs.Msg2_E1_Three
    })

  ab2 <- execPut $ TestMsgs.putMsg2 msg2
  let dv2 = DV.whole ab2
  parseResult2 <- runParserT dv2 $ TestMsgs.parseMsg2 $ DV.byteLength dv2
  case parseResult2 of
    Left err -> assert' ("msg2 parse " <> show err) false
    Right msg2' -> assert' "msg2 roundtrip" $ genericEq msg2 msg2'

  let msg3 = (TestMsgs.Msg3
    { f1: replicate 3 msg1
    , f2: replicate 3 msg2
    -- , f4: replicate 3 (TestMsgs.Msg3_Msg4 { f1: Just 1 })
    })

  ab3 <- execPut $ TestMsgs.putMsg3 msg3
  let dv3 = DV.whole ab3
  parseResult3 <- runParserT dv3 $ TestMsgs.parseMsg3 $ DV.byteLength dv3
  case parseResult3 of
    Left err -> assert' ("msg3 parse " <> show err) false
    Right (TestMsgs.Msg3 {f1:f1',f2:f2'}) -> assert' "msg3 roundtrip" $
      (and $ zipWith genericEq f1 f1') && (and $ zipWith genericEq f2 f2')
     where
      TestMsgs.Msg3 {f1,f2} = msg3
