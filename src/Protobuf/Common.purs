module Protobuf.Common
( FieldNumber
, WireType
, wtVarint
, wt64
, wtLength
, wt32
)
where

import Data.UInt (UInt)
-- import Data.Enum

type FieldNumber = UInt
type WireType    = Int

wtVarint :: Int
wtVarint = 0

wt64 :: Int
wt64 = 1

wtLength :: Int
wtLength = 2

wt32 :: Int
wt32 = 5


-- data WireType
--   = Varint
--   | EightBytes
--   | LengthDelimited
--   | StartGroup
--   | EndGroup
--   | FourBytes
--
-- instance enumWireType :: Enum WireType where
--   succ Varint = Just EightBytes
--   succ EightBytes = Just
