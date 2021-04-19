-- | Primitive parsers for decoding Google Protocol Buffers.
-- |
-- | You almost never need to import this module.
-- | See package README for explanation.
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

import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as AT
import Data.ArrayBuffer.Types (DataView, Uint8Array)
import Data.Either (Either(..))
import Data.Float32 (Float32)
import Data.Int (Radix, radix, toStringAs)
import Data.Long.Internal (Long, Signed, Unsigned, fromLowHighBits, highBits, lowBits, unsignedToSigned)
import Data.Long.Internal as Long
import Data.Maybe (Maybe(..), fromJust)
import Data.TextDecoding (decodeUtf8)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Partial.Unsafe (unsafePartial)
import Protobuf.Common (Bytes(..))
import Protobuf.Decode32 (varint32, zigzag32, tag32)
import Protobuf.Decode64 (varint64, zigzag64)
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.DataView as Parse

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
  pure $ lowBits n
    -- But this is a problem with the Protobuf spec?
    -- “If you use int32 or int64 as the type for a negative number, the resulting
    -- varint is always ten bytes long”
    -- https://developers.google.com/protocol-buffers/docs/encoding#signed_integers
    -- So what are we supposed to do if the field type is int32 but the
    -- decoded varint is too big? There is no guarantee that either negative
    -- or positive numbers encoded as varints will be in range.
    -- I suppose we could check that the high bits are all 0
    -- for positive or all 1 for negative.
    -- The conformance tests seem to like it if we just throw away
    -- the hight bits. So that's what we'll do.
    --
    -- conformance checker hates this:
    --   case Long.toInt n of
    --     Nothing -> fail "int32 value out of range."
    --     Just x -> pure x

-- | __int64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
int64 :: forall m. MonadEffect m => ParserT DataView m (Long Signed)
int64 = unsignedToSigned <$> varint64

-- | __uint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint32 :: forall m. MonadEffect m => ParserT DataView m UInt
uint32 = do
  n <- varint64
  pure $ UInt.fromInt $ lowBits n

-- | __uint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint64 :: forall m. MonadEffect m => ParserT DataView m (Long Unsigned)
uint64 = varint64

-- | __sint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sint32 :: forall m. MonadEffect m => ParserT DataView m Int
sint32 = do
  n <- zigzag32 <$> varint32
  pure n
  -- n <- zigzag64 <$> varint64
  -- -- pure $ lowBits n
  -- case Long.toInt n of
  --   -- Nothing -> fail "sint32 value out of range."
  --   Nothing -> fail $ "\nsint32 value out of range.\n lowBits " <> toStringAs (unsafePartial $ fromJust $ radix 16) (lowBits n) <> "\n highBits " <> toStringAs (unsafePartial $ fromJust $ radix 16) (highBits n)
  --   Just x -> pure x

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
  x <- varint64
  if lowBits x == 0 && highBits x == 0
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
bytes :: forall m. MonadEffect m => ParserT DataView m Bytes
bytes = do
  len <- UInt.toInt <$> varint32
  dv <- Parse.takeN len
  let ab = DV.buffer dv
  let begin = DV.byteOffset dv
  pure $ Bytes $ AB.slice begin (begin + len) ab


