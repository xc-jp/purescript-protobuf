-- | Common utility definitions.
module Protobuf.Internal.Common
  ( FieldNumber
  , WireType(..)
  , Bytes(..)
  , label
  , class Default
  , default
  , isDefault
  , fromDefault
  , toDefault
  , mkUint8Array
  ) where

import Prelude

import Control.Monad.Error.Class (throwError, catchError)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Builder (DataBuff(..), toView)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types (DataView, Uint8Array)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), fromEnum)
import Data.Float32 (Float32)
import Data.Float32 as Float32
import Data.Generic.Rep (class Generic)
import Data.Int64 (Int64)
import Data.UInt64 (UInt64)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Parsing (ParserT, ParseError(..))

type FieldNumber
  = UInt

-- | https://developers.google.com/protocol-buffers/docs/encoding#structure
data WireType
  = VarInt
  | Bits64
  | LenDel
  | Bits32

derive instance eqWireType :: Eq WireType

derive instance genericWireType :: Generic WireType _

instance ordWireType :: Ord WireType where
  compare l r = compare (fromEnum l) (fromEnum r)

instance boundedWireType :: Bounded WireType where
  top = Bits32
  bottom = VarInt

instance enumWireType :: Enum WireType where
  succ VarInt = Just Bits64
  succ Bits64 = Just LenDel
  succ LenDel = Just Bits32
  succ Bits32 = Nothing
  pred VarInt = Nothing
  pred Bits64 = Just VarInt
  pred LenDel = Just Bits64
  pred Bits32 = Just LenDel

instance boundedEnumWireType :: BoundedEnum WireType where
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

instance showWireType :: Show WireType where
  show = genericShow

-- | Representation of a __bytes__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
-- | field.
-- |
-- | On a message which has been decoded,
-- | The wrapped `DataBuff` will usually be a `DataView`.
-- | In that case, the `DataView` is a view into the
-- | received message I/O buffer.
-- |
-- | For messages which you intend to encode,
-- | You may set it the `DataBuff` to `DataView` or `ArrayBuffer`,
-- | whichever seems best.
-- |
-- | The `ArrayBuffer` and `DataView` are mutable, so be careful not to mutate
-- | them if anything might read them again. Here we trade off typechecker
-- | guarantees for implementation simplicity.
newtype Bytes = Bytes DataBuff

instance showBytes :: Show Bytes where
  show (Bytes (Buff ab)) = "<ArrayBuffer length " <> show (AB.byteLength ab) <> ">"
  show (Bytes (View dv)) = "<DataView length " <> show (DV.byteLength dv) <> ">"

instance eqBytes :: Eq Bytes where
  eq (Bytes l) (Bytes r) =
    unsafePerformEffect
      $ do
          l' <- mkUint8Array $ toView l -- :: Effect Uint8Array
          r' <- mkUint8Array $ toView r -- :: Effect Uint8Array
          TA.eq l' r'

derive instance newtypeBytes :: Newtype Bytes _

-- | In Protobuf, [zero values are “default values”](https://developers.google.com/protocol-buffers/docs/proto3#default)
-- | and have special semantics.
class Default a where
  default :: a
  isDefault :: a -> Boolean

-- We use instance chains
-- https://github.com/purescript/documentation/blob/master/language/Type-Classes.md#instance-chains
-- so that we can define instance defaultBoundedEnum without overlapping.
-- Discussion: https://github.com/purescript/purescript/issues/3596
instance defaultString :: Default String where
  default = ""
  isDefault = String.null
else instance defaultInt :: Default Int where
  default = 0
  isDefault x = x == 0
else instance defaultNumber :: Default Number where
  default = 0.0
  isDefault x = x == 0.0
else instance defaultInt64 :: Default Int64 where
  default = zero
  isDefault x = x == default
else instance defaultUInt64 :: Default UInt64 where
  default = zero
  isDefault x = x == default
else instance defaultFloat32 :: Default Float32 where
  default = Float32.fromNumber' 0.0
  isDefault x = x == default
else instance defaultBoolean :: Default Boolean where
  default = false
  isDefault x = not x
else instance defaultUInt :: Default UInt where
  default = UInt.fromInt 0
  isDefault x = x == default
else instance defaultBytes :: Default Bytes where
  default = Bytes $ Buff $ unsafePerformEffect $ AB.empty 0
  isDefault (Bytes buf) = DV.byteLength (toView buf) == 0

-- | Turns a `default` value into `Nothing`.
fromDefault :: forall a. Default a => Eq a => a -> Maybe a
fromDefault x = if x == default then Nothing else Just x

-- | Turns `Nothing` into a `default` value.
-- |
-- | The Protobuf spec requires that a *no presence* field set
-- | to its “default” (zero) value must not be serialized to the wire.
-- | We can use this function to interpret
-- | a missing field as a “default” value.
toDefault :: forall a. Default a => Maybe a -> a
toDefault Nothing = default
toDefault (Just x) = x

-- | Make a `Uint8Array` Typed Array from a `DataView`. We can do this
-- | for the case of `Uint8` because byte arrays are always aligned.
mkUint8Array :: DataView -> Effect Uint8Array
mkUint8Array dv = TA.part (DV.buffer dv) (DV.byteOffset dv) (DV.byteLength dv)

-- | If parsing fails inside this labelled context, then prepend the `String`
-- | to the error `String` in the `ParseError`. Use this to establish
-- | context for parsing failure error messages.
label :: forall m s a. Monad m => String -> ParserT s m a -> ParserT s m a
label messagePrefix p =
  catchError p
    $ \(ParseError message pos) ->
        throwError $ ParseError (messagePrefix <> message) pos
