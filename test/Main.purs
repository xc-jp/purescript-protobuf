module Test.Main where

import Prelude

import Data.Array (catMaybes, zipWith)
import Data.ArrayBuffer.Builder (execPut)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as TA
import Data.Either (Either(..))
import Data.Float32 as Float32
import Data.Foldable (and)
import Data.Long.Internal as Long
import Data.Maybe (Maybe(..))
import Data.UInt as UInt
import Data.Unfoldable (replicate)
import Effect (Effect)
import Pack.Msg1 as Pack1
import Pack.Msg2 as Pack2
import Pack3.Msg3 as Pack3
import Pack4.Msg4 as Pack4
import Pack5.Msg5 as Pack5
import Test.Assert (assert')
import Text.Parsing.Parser (runParserT)
import Data.TextEncoding (encodeUtf8)
import Protobuf.Common (Bytes(..))

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

  let somebytes = TA.buffer $ encodeUtf8 "some bytes"

  let msgxx = Pack1.mkMsg1 { f1: Just 1.0 }

  let msg4 = (Pack4.mkMsg2 -- this is just a compile test
    { f1: Just $ Pack4.mkMsg1_Msg2 { nested: Just "nested" }
    })

  let msg6 = (Pack5.mkMsg2 -- this is just a compile test
    { sum1: Just $ Pack5.Msg2_Sum1_F1 $ Pack5.mkMsg1
      { sum1: Just $ Pack5.Msg1_Sum1_F3 billion'
      }
    })

  let msg1 = (Pack1.mkMsg1
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
    , f14: Just $ Bytes somebytes
    , f15: Just Pack1.Msg1_E1_Three
    })

  ab1 <- execPut $ Pack1.putMsg1 msg1
  let dv1 = DV.whole ab1
  parseResult1 <- runParserT dv1 $ Pack1.parseMsg1 $ DV.byteLength dv1
  case parseResult1 of
    Left err -> assert' ("msg1 parse " <> show err) false
    Right msg1' -> assert' "msg1 roundtrip" $ msg1 == msg1'

  let msg2 = (Pack2.mkMsg2
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
    , f14: replicate 3 $ Bytes somebytes
    , f15: replicate 3 Pack2.Msg2_E1_Three
    })

  ab2 <- execPut $ Pack2.putMsg2 msg2
  let dv2 = DV.whole ab2
  parseResult2 <- runParserT dv2 $ Pack2.parseMsg2 $ DV.byteLength dv2
  case parseResult2 of
    Left err -> assert' ("msg2 parse " <> show err) false
    Right msg2' -> assert' "msg2 roundtrip" $ msg2 == msg2'

  let msg3 = (Pack3.mkMsg3
    { f1: replicate 3 msg1
    , f2: replicate 3 msg2
    , f4: replicate 3 (Pack3.mkMsg3_Msg4 { f1: Just 1 })
    })

  ab3 <- execPut $ Pack3.putMsg3 msg3
  let dv3 = DV.whole ab3
  parseResult3 <- runParserT dv3 $ Pack3.parseMsg3 $ DV.byteLength dv3
  case parseResult3 of
    Left err -> assert' ("msg3 parse " <> show err) false
    Right msg3' -> assert' "msg3 roundtrip" $ msg3 == msg3'


