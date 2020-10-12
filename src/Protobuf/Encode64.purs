-- | Primitive `Long`-based builders for encoding Google Protocol Buffers.
-- |
-- | Do not import this module.
-- | See package README for explanation.
module Protobuf.Encode64
( zigzag64
, varint64
)
where

import Prelude
import Effect.Class (class MonadEffect)
import Data.ArrayBuffer.Builder as Builder
import Data.Long (toUnsigned)
import Data.Long.Internal (Long, Signed, Unsigned, lowBits, unsafeFromInt, shl, shr, zshr)
import Data.Long.Bits ((.^.))
import Data.UInt (UInt, (.&.), (.|.))
import Data.UInt as UInt

-- | https://developers.google.com/protocol-buffers/docs/encoding#signed_integers
zigzag64 :: Long Signed -> Long Unsigned
zigzag64 n = toUnsigned $ (n `shl` (unsafeFromInt 1)) .^. (n `shr` (unsafeFromInt 63))

varint64 :: forall m. MonadEffect m => Long Unsigned -> Builder.PutM m Unit
varint64 n_0 = do
  let group_0 = takeGroup n_0
      n_1     = n_0 `zshr` u7
  if n_1 == u0
    then Builder.putUint8 group_0
    else do
      Builder.putUint8 $ contGroup group_0
      let group_1 = takeGroup n_1
          n_2     = n_1 `zshr` u7
      if n_2 == u0
        then Builder.putUint8 group_1
        else do
          Builder.putUint8 $ contGroup group_1
          let group_2 = takeGroup n_2
              n_3     = n_2 `zshr` u7
          if n_3 == u0
            then Builder.putUint8 group_2
            else do
              Builder.putUint8 $ contGroup group_2
              let group_3 = takeGroup n_3
                  n_4     = n_3 `zshr` u7
              if n_4 == u0
                then Builder.putUint8 group_3
                else do
                  Builder.putUint8 $ contGroup group_3
                  let group_4 = takeGroup n_4
                      n_5     = n_4 `zshr` u7
                  if n_5 == u0
                    then Builder.putUint8 group_4
                    else do
                      Builder.putUint8 $ contGroup group_4
                      let group_5 = takeGroup n_5
                          n_6     = n_5 `zshr` u7
                      if n_6 == u0
                        then Builder.putUint8 group_5
                        else do
                          Builder.putUint8 $ contGroup group_5
                          let group_6 = takeGroup n_6
                              n_7     = n_6 `zshr` u7
                          if n_7 == u0
                            then Builder.putUint8 group_6
                            else do
                              Builder.putUint8 $ contGroup group_6
                              let group_7 = takeGroup n_7
                                  n_8     = n_7 `zshr` u7
                              if n_8 == u0
                                then Builder.putUint8 group_7
                                else do
                                  Builder.putUint8 $ contGroup group_7
                                  let group_8 = takeGroup n_8
                                      n_9     = n_8 `zshr` u7
                                  if n_9 == u0
                                    then Builder.putUint8 group_8
                                    else do
                                      Builder.putUint8 $ contGroup group_8
                                      Builder.putUint8 $ takeGroup n_9
 where
  -- copy the low seven bits group from a Long
  takeGroup :: Long Unsigned -> UInt
  takeGroup n = (UInt.fromInt $ lowBits n) .&. u0x7F
  -- Set the high eigth continuation bit of a group
  contGroup :: UInt -> UInt
  contGroup n = u0x80 .|. n

  u0    = unsafeFromInt 0
  u7    = unsafeFromInt 7
  u0x7F = UInt.fromInt 0x7F
  u0x80 = UInt.fromInt 0x80
