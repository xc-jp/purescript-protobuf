-- | You almost never need to `import` this module.
-- | See package README for explanation.
module Protobuf.Common
( FieldNumber
, WireType(..)
, Bytes(..)
, class Default
, default
, isDefault
, fromDefault
, toDefault
)
where

import Prelude

import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), fromEnum, toEnum)
import Data.Float32 (Float32)
import Data.Float32 as Float32
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Long.Internal (Long, Signed, Unsigned, fromLowHighBits)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.String as String
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)

type FieldNumber = UInt

-- | https://developers.google.com/protocol-buffers/docs/encoding#structure
data WireType
  = VarInt
  | Bits64
  | LenDel
  | Bits32

derive instance eqWireType :: Eq WireType
derive instance genericWireType :: Generic WireType _

instance ordWireType :: Ord WireType
 where
  compare l r = compare (fromEnum l) (fromEnum r)

instance boundedWireType :: Bounded WireType
 where
  top = Bits32
  bottom = VarInt

instance enumWireType :: Enum WireType
 where
  succ VarInt = Just Bits64
  succ Bits64 = Just LenDel
  succ LenDel = Just Bits32
  succ Bits32 = Nothing
  pred VarInt = Nothing
  pred Bits64 = Just VarInt
  pred LenDel = Just Bits64
  pred Bits32 = Just LenDel

instance boundedEnumWireType :: BoundedEnum WireType
 where
  cardinality = Cardinality 4
  toEnum 0 = Just VarInt
  toEnum 1 = Just Bits64
  toEnum 2 = Just LenDel
  toEnum 5 = Just Bits32
  toEnum _ = Nothing
  fromEnum VarInt = 0
  fromEnum Bits64 = 1
  fromEnum LenDel = 2
  fromEnum Bits32 = 5

instance showWireType :: Show WireType where show = genericShow

-- | Representation of a __bytes__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
-- | field.
newtype Bytes = Bytes ArrayBuffer

instance showBytes :: Show Bytes
 where
  show (Bytes ab) = "<Bytes length " <> show (AB.byteLength ab) <> ">"

instance eqBytes :: Eq Bytes
 where
  eq (Bytes l) (Bytes r) = unsafePerformEffect $ do
    l' <- TA.whole l :: Effect Uint8Array
    r' <- TA.whole r :: Effect Uint8Array
    TA.eq l' r'

derive instance newtypeBytes :: Newtype Bytes _

-- | In Protobuf, [zero values are “default values”](https://developers.google.com/protocol-buffers/docs/proto3#default)
-- | and have special semantics.
class Default a
 where
  default :: a
  isDefault :: a -> Boolean

-- We use instance chains
-- https://github.com/purescript/documentation/blob/master/language/Type-Classes.md#instance-chains
-- so that we can define instance defaultBoundedEnum without overlapping.
-- Discussion: https://github.com/purescript/purescript/issues/3596

instance defaultString :: Default String
 where
  default = ""
  isDefault = String.null

else instance defaultInt :: Default Int
 where
  default = 0
  isDefault x = x == 0

else instance defaultNumber :: Default Number
 where
  default = 0.0
  isDefault x = x == 0.0

else instance defaultLongSigned :: Default (Long Signed)
 where
  default = fromLowHighBits 0 0
  isDefault x = x == default

else instance defaultLongUnsigned :: Default (Long Unsigned)
 where
  default = fromLowHighBits 0 0
  isDefault x = x == default

else instance defaultFloat32 :: Default Float32
 where
  default = Float32.fromNumber' 0.0
  isDefault x = x == default

else instance defaultBoolean :: Default Boolean
 where
  default = false
  isDefault x = not x

else instance defaultUInt :: Default UInt
 where
  default = UInt.fromInt 0
  isDefault x = x == default

else instance defaultBytes :: Default Bytes
 where
  default = Bytes $ unsafePerformEffect $ AB.empty 0
  isDefault (Bytes ab) = ArrayBuffer.byteLength ab == 0

else instance defaultBoundedEnum :: BoundedEnum a => Default a
 where
  default = unsafePartial $ fromJust $ toEnum 0 -- “There must be a zero value” https://developers.google.com/protocol-buffers/docs/proto3#enum
  isDefault x = x == default

-- Maybe we want to rip off? https://pursuit.purescript.org/packages/purescript-data-default/0.3.2/docs/Data.Default#t:GDefault

-- | Turns a `default` value into `Nothing`.
fromDefault :: forall a. Default a => Eq a => a -> Maybe a
fromDefault x = if x==default then Nothing else Just x

-- | Turns `Nothing` into a `default` value.
-- |
-- | The Protobuf spec requires that a field being set
-- | to zero (“default”) and a missing field are equivalent states.
-- | Because of this, there is
-- | no way to tell whether a missing field from a received message
-- | is really missing or if the sender meant that the field value is zero.
-- | We can use this function
-- | to manually choose whether we want
-- | a missing field to mean that it’s missing, or to mean that it’s zero.
toDefault :: forall a. Default a => Maybe a -> a
toDefault Nothing = default
toDefault (Just x) = x
