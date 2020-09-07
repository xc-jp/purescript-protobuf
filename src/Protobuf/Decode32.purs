-- | Primitive UInt-based parsers for decoding Google Protocol Buffers.
module Protobuf.Decode32
( zigzag32
, tag32
, varint32
)

import Prelude
import Effect (Effect, liftEffect)
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.DataView as Parse
import Data.UInt (UInt, fromInt, toInt, (.&.), (.|.), (.^.))
import Data.ArrayBuffer.Types (DataView)
import Protobuf.Common (FieldNumber, WireType)

-- | https://stackoverflow.com/questions/2210923/zig-zag-decoding
zigzag32 :: UInt -> Int
zigzag32 n = toInt $ (n `shr` 1) .^. (unegate (n .&. 1))
 where unegate = fromInt <<< negate << toInt
    -- unegate x = complement x + (fromInt 1) -- TODO switch to this definition?

tag32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber WireType)
tag = do
  n <- varint32
  pure $ Tuple (n `shr` 3) (toInt $ n .&. 3)

-- | https://developers.google.com/protocol-buffers/docs/encoding#varints
varint32 :: forall m. MonadEffect m => ParserT DataView m UInt
varint32 = do
  n_0 <- Parse.anyUInt8
  if n_0 < u0x80
    then pure n_0
    else do
      let acc_0 = n_0 .&. u0x7F
      n_1 <- Parse.anyUInt8
      if n_1 < u0x80
        then pure $ acc_0 .|. (n_1 `shl` u7)
        else do
          let acc_1 = ((n_1 .&. u0x7F) `shl` u7) .|. acc_0
          n_2 <- Parse.anyUInt8
          if n_2 < u0x80
            then pure $ acc_1 .|. (n_2 `shl` u14)
            else do
              let acc_2 = ((n_2 .&. u0x7F) `shl` u14) .|. acc_1
              n_3 <- Parse.anyUInt8
              if n_3 < u0x80
                then pure $ acc_2 .|. (n_3 `shl` u21)
                else do
                  let acc_3 = ((n_3 .&. u0x7F) `shl` u21) .|. acc_2
                  n_4 <- Parse.anyUint8
                  if n_4 < u0x10
                    then pure $ acc_3 .|. (n_4 `shl` u28)
                    else fail "varint32 overflow. Please report this as a bug."
                    -- There is no varint32 in the Protbuf spec, this is
                    -- just a performance-improving assumption we make
                    -- in cases where only a deranged lunatic would use a value
                    -- bigger than 32 bits, such as in field numbers.
                    -- We think this is worth the risk because UInt is
                    -- represented as a native Javascript Number whereas
                    -- Long is a composite library type, so we expect the
                    -- performance difference to be significant.
 where
  u7    = fromInt 7
  u14   = fromInt 14
  u21   = fromInt 21
  u28   = fromInt 28
  u0x10 = fromInt 0x10
  u0x7F = fromInt 0x7F
  u0x80 = fromInt 0x80

