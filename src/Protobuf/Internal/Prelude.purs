-- | The prelude for generated code.
module Protobuf.Internal.Prelude
  ( module Prelude
  , module Control.Alt
  , module Record.Builder
  , module Data.Array
  , module Data.Bounded
  , module Data.Enum
  , module Data.Eq
  , module Data.Function
  , module Data.Float32
  , module Data.Show
  , module Data.Ord
  , module Data.Maybe
  , module Data.Newtype
  , module Data.Generic.Rep
  , module Data.Show.Generic
  , module Data.Bounded.Generic
  , module Data.Enum.Generic
  , module Data.Ord.Generic
  , module Data.Semigroup
  , module Data.String
  , module Type.Proxy
  , module Record
  , module Data.Traversable
  , module Data.Tuple
  , module Data.UInt
  , module Prim.Row
  , module Data.UInt64
  , module Data.Int64
  , module Parsing
  , module Data.ArrayBuffer.Builder
  , module Data.ArrayBuffer.Types
  , module Protobuf.Internal.Common
  , module Protobuf.Internal.Decode
  , module Protobuf.Internal.Encode
  , module Protobuf.Internal.Runtime
  , module Effect.Class
  , module MonadRec.Class
  ) where

import Type.Proxy (Proxy(..))
import Control.Alt (alt)
import Control.Monad.Rec.Class (class MonadRec) as MonadRec.Class
import Data.Array (snoc)
import Data.ArrayBuffer.Builder (PutM)
import Data.ArrayBuffer.Types (DataView, ByteLength)
import Data.Bounded (class Bounded)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Eq (class Eq, eq)
import Data.Float32 (Float32)
import Data.Function (flip)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericPred, genericSucc)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Int64 (Int64)
import Data.UInt64 (UInt64)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Data.String (joinWith)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.UInt (toInt, fromInt, UInt)
import Effect.Class (class MonadEffect) as Effect.Class
import Prelude ((>>>), ($), bind, (>>=), pure, discard, append, (<<<), Unit, unit, map, negate)
import Prim.Row (class Union, class Nub)
import Protobuf.Internal.Common (class Default, Bytes(..), FieldNumber, WireType(..), label, default, fromDefault, isDefault, toDefault)
import Protobuf.Internal.Decode (decodeBool, decodeBytes, decodeDouble, decodeDoubleArray, decodeFixed32, decodeFixed32Array, decodeFixed64, decodeFixed64Array, decodeFloat, decodeFloatArray, decodeInt32, decodeInt64, decodeSfixed32, decodeSfixed32Array, decodeSfixed64, decodeSfixed64Array, decodeSint32, decodeSint64, decodeString, decodeTag32, decodeUint32, decodeUint64, decodeVarint32, decodeVarint64, decodeZigzag32, decodeZigzag64)
import Protobuf.Internal.Encode (encodeBool, encodeBoolField, encodeBuilder, encodeBytesField, encodeDouble, encodeDoubleField, encodeFixed32, encodeFixed32Field, encodeFixed64, encodeFixed64Field, encodeFloat, encodeFloatField, encodeInt32, encodeInt32Field, encodeInt64, encodeInt64Field, encodeSfixed32, encodeSfixed32Field, encodeSfixed64, encodeSfixed64Field, encodeSint32, encodeSint32Field, encodeSint64, encodeSint64Field, encodeStringField, encodeTag32, encodeUint32, encodeUint32Field, encodeUint64, encodeUint64Field, encodeVarint32, encodeVarint64, encodeZigzag32, encodeZigzag64)
import Protobuf.Internal.Runtime (FieldNumberInt, Pos, UnknownField(..), manyLength, mergeWith, parseEnum, parseFieldUnknown, parseLenDel, parseMessage, positionZero, putEnum, putEnumField, putFieldUnknown, putLenDel, putOptional, putPacked, putRepeated)
import Record (merge)
import Record.Builder (modify, Builder)
import Parsing (ParserT)
