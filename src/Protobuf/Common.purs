-- | Do not import this module.
-- | See package README for explanation.
module Protobuf.Common
( FieldNumber
, WireType(..)
, Bytes(..)
, class Default
, default
, fromDefault
, toDefault
)
where

import Prelude

import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Enum (class Enum, fromEnum, class BoundedEnum, Cardinality(..))
import Data.Float32 (Float32)
import Data.Float32 as Float32
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Long.Internal (Long, Signed, Unsigned, fromLowHighBits)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)

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

-- | Default values
-- |
-- | https://developers.google.com/protocol-buffers/docs/proto3#default
class Default a
 where
  default :: a

instance defaultString :: Default String where default = ""
instance defaultInt :: Default Int where default = 0
instance defaultNumber :: Default Number where default = 0.0
instance defaultLongSigned :: Default (Long Signed) where default = fromLowHighBits 0 0
instance defaultLongUnsigned :: Default (Long Unsigned) where default = fromLowHighBits 0 0
instance defaultFloat32 :: Default Float32 where default = Float32.fromNumber' 0.0
instance defaultBoolean :: Default Boolean where default = false
instance defaultUInt :: Default UInt where default = UInt.fromInt 0
instance defaultArray :: Default a => Default (Array a) where default = []
instance defaultBytes :: Default Bytes where default = Bytes $ unsafePerformEffect $ AB.empty 0

-- Maybe we want to rip off? https://pursuit.purescript.org/packages/purescript-data-default/0.3.2/docs/Data.Default#t:GDefault

-- | Turns a `default` value into `Nothing`. Sometimes convenient for decoding.
fromDefault :: forall a. Default a => Eq a => a -> Maybe a
fromDefault x = if x==default then Nothing else Just x

-- | Turns `Nothing` into a `default` value. Sometimes convenient for encoding.
toDefault :: forall a. Default a => Maybe a -> a
toDefault Nothing = default
toDefault (Just x) = x