-- | Primitive parsers for decoding Google Protocol Buffers.
module Protobuf.Decode
( double
, float
, int32
, int64
, uint32
, uint64
, sint32
, sint64
, fixed32
, fixed64
, sfixed32
, sfixed64
, bool
, string
, bytes
, module Protobuf.Decode32
, module Protobuf.Decode64
) where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Trans.Class (lift)
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.DataView as Parse
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Long.Internal (Long, Signed, Unsigned, fromLowHighBits, unsignedToSigned)
import Data.Long as SLong
import Data.Float32 (Float32)
import Data.ArrayBuffer.Types (DataView, Uint8Array)
import Data.ArrayBuffer.Typed as AT
import Data.ArrayBuffer.DataView as DV
import Data.TextDecoding (decodeUtf8)
import Protobuf.Decode32 (varint32, zigzag32, tag32)
import Protobuf.Decode64 (varint64, zigzag64)

-- | __double__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
double :: forall m. MonadEffect m => ParserT DataView m Number
double = Parse.anyFloat64le

-- | __float__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
float :: forall m. MonadEffect m => ParserT DataView m Float32
float = Parse.anyFloat32le

-- | __int32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
int32 :: forall m. MonadEffect m => ParserT DataView m Int
int32 = do
  n <- varint64
  case SLong.toInt (unsignedToSigned n) of
    Just x -> pure x
    Nothing -> fail "int32 overflow. Please report this as a bug."
    -- But this is a problem with the Protobuf spec?
    -- “If you use int32 or int64 as the type for a negative number, the resulting
    -- varint is always ten bytes long”
    -- https://developers.google.com/protocol-buffers/docs/encoding#signed_integers
    -- So what are we supposed to do if the field type is int32 but the
    -- decoded varint is too big? There is no guarantee that either negative
    -- or positive numbers encoded as varints will be in range.

-- | __int64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
int64 :: forall m. MonadEffect m => ParserT DataView m (Long Signed)
int64 = unsignedToSigned <$> varint64

-- | __uint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint32 :: forall m. MonadEffect m => ParserT DataView m UInt
uint32 = do
  n <- varint64
  case SLong.toInt (unsignedToSigned n) of
    Just x -> pure $ UInt.fromInt x
    Nothing -> fail "uint32 overflow. Please report this as a bug."
    -- But this is a problem with the Protobuf spec?
    -- “If you use int32 or int64 as the type for a negative number, the resulting
    -- varint is always ten bytes long”
    -- https://developers.google.com/protocol-buffers/docs/encoding#signed_integers
    -- So what are we supposed to do if the field type is int32 but the
    -- decoded varint is too big? There is no guarantee that either negative
    -- or positive numbers encoded as varints will be in range.

-- | __uint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint64 :: forall m. MonadEffect m => ParserT DataView m (Long Unsigned)
uint64 = varint64

-- | __sint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sint32 :: forall m. MonadEffect m => ParserT DataView m Int
sint32 = zigzag32 <$> varint32

-- | __sint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sint64 :: forall m. MonadEffect m => ParserT DataView m (Long Signed)
sint64 = zigzag64 <$> varint64

-- | __fixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
fixed32 :: forall m. MonadEffect m => ParserT DataView m UInt
fixed32 = Parse.anyUint32le

-- | __fixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
fixed64 :: forall m. MonadEffect m => ParserT DataView m (Long Unsigned)
fixed64 = fromLowHighBits <$> Parse.anyInt32le <*> Parse.anyInt32le

-- | __sfixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sfixed32 :: forall m. MonadEffect m => ParserT DataView m Int
sfixed32 = Parse.anyInt32le

-- | __sfixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sfixed64 :: forall m. MonadEffect m => ParserT DataView m (Long Signed)
sfixed64 = fromLowHighBits <$> Parse.anyInt32le <*> Parse.anyInt32le

-- | __bool__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
bool :: forall m. MonadEffect m => ParserT DataView m Boolean
bool = do
  x <- varint32
  if x == UInt.fromInt 0
    then pure false
    else pure true

-- | __string__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
string :: forall m. MonadEffect m => ParserT DataView m String
string = do
  stringview <- varint32 >>= UInt.toInt >>> Parse.takeN
  stringarray <- lift $ liftEffect $ mkTypedArray stringview
  case decodeUtf8 stringarray of
    Left err -> fail $ "string decodeUtf8 failed. " <> show err
    Right s -> pure s
 where
  mkTypedArray :: DataView -> Effect Uint8Array
  mkTypedArray dv = do
    let buffer     = DV.buffer dv
        byteOffset = DV.byteOffset dv
        byteLength = DV.byteLength dv
    AT.part buffer byteOffset byteLength

-- | __bytes__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
bytes :: forall m. MonadEffect m => ParserT DataView m DataView
bytes = varint32 >>= UInt.toInt >>> Parse.takeN

