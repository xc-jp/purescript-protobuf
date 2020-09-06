module Protobuf.Common
( FieldNumber
)
where

import Data.UInt (UInt)
-- import Data.Enum

type FieldNumber = UInt
type WireType    = Int

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
