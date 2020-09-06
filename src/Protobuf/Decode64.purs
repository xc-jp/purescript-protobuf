-- | Primitive parsers for decoding Google Protocol Buffers.
module Protobuf.Decode
( double
, float
, int32
, int64as32
, uint32
, uint64as32
, sint32
, sint64as32
, fixed32
, fixed64as32
, sfixed32
, sfixed64as32
, bool
, string
, bytes
)


import Prelude
import Effect (Effect, liftEffect)
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.DataView as Parse
import Data.UInt (UInt, fromInt, toInt, (.&.), (.|.), (.^.))
import Data.TextEncoding (encodeUtf8)
import Data.ArrayBuffer.Types (DataView, Uint8Array)
import Data.ArrayBuffer.Typed as AT
import Data.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Protobuf.Common (FieldNumber, WireType)

-- | __double__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
double :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber Number)
double = do
  Tuple fieldNumber wireType <- tag
  if wireType == 1
    then Tuple fieldNumber <$> Parse.anyFloat64le
    else fail $ "Expected double, but got wire type " <> show wireType

-- | __float__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
float :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber Float32)
float = do
  Tuple fieldNumber wireType <- tag
  if wireType == 5
    then Tuple fieldNumber <$> Parse.anyFloat32le
    else fail $ "Expected float, but got wire type " <> show wireType

-- | __int32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
int32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber Int)
int32 = do
  Tuple fieldNumber wireType <- tag
  if wireType == 0
    then Tuple fieldNumber <$> map toInt varint32
    -- TODO Will always fail on a negative number.
    else fail $ "Expected int32, but got wire type " <> show wireType

-- | __int64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
int64as32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber Int)
int64as32 = do
  Tuple fieldNumber wireType <- tag
  if wireType == 0
    then Tuple fieldNumber <$> map toInt varint32
    -- TODO Will always fail on a negative number.
    else fail $ "Expected int64, but got wire type " <> show wireType

-- | __uint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber UInt)
uint32 = do
  Tuple fieldNumber wireType <- tag
  if wireType == 0
    then Tuple fieldNumber <$> varint32
    else fail $ "Expected uint32, but got wire type " <> show wireType

-- | __uint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
uint64as32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber UInt)
uint64as32 = do
  Tuple fieldNumber wireType <- tag
  if wireType == 0
    then Tuple fieldNumber <$> varint32
    else fail $ "Expected uint64, but got wire type " <> show wireType

-- | __sint32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sint32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber Int)
sint32 = do
  Tuple fieldNumber wireType <- tag
  if wireType == 0
    then Tuple fieldNumber <$> map zigZag32 varint32
    else fail $ "Expected sint32, but got wire type " <> show wireType

-- | We don't have `Int64` in Purescript, but this will decode an `Int`
-- | from a __sint64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sint64as32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber Int)
sint64as32 = do
  Tuple fieldNumber wireType <- tag
  if wireType == 0
    then Tuple fieldNumber <$> map zigZag32 varint32
    else fail $ "Expected sint64, but got wire type " <> show wireType

-- | __fixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
fixed32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber UInt)
fixed32 = do
  Tuple fieldNumber wireType <- tag
  if wireType == 5
    then Tuple fieldNumber <$> Parse.anyUint32le
    else fail $ "Expected fixed32, but got wire type " <> show wireType

-- | We don't have `UInt64` in Purescript, but this will decode a `UInt`
-- | from a __fixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
fixed64as32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber UInt)
fixed64as32 = do
  Tuple fieldNumber wireType <- tag
  if wireType == 1
    then do
      lowbits  <- Parse.anyUint32le
      highbits <- Parse.anyUint32le
      if highbits == fromInt 0
        then pure $ Tuple fieldNumber lowbits
        else fail $ "fixed64as32 overflow"
    else fail $ "Expected fixed64, but got wire type " <> show wireType

-- | __sfixed32__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sfixed32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber Int)
sfixed32 = do
  Tuple fieldNumber wireType <- tag
  if wireType == 5
    then Tuple fieldNumber <$> Parse.anyInt32le
    else fail $ "Expected sfixed32, but got wire type " <> show wireType

-- | We don't have `Int64` in Purescript, but this will decode a `Int`
-- | from an __sfixed64__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
sfixed64as32 :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber Int)
sfixed64as32 = do
  Tuple fieldNumber wireType <- tag
  if wireType == 1
    then do
      lowbits <- Parse.anyInt32le
      highbits <- Parse.anyInt32le
      if highbits == 0
        then pure $ Tuple fieldNumber lowbits
        else fail $ "sfixed64as32 overflow"
    else fail $ "Expected sfixed64, but got wire type " <> show wireType

-- | __bool__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
bool :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber Boolean)
bool = do
  Tuple fieldNumber wireType <- tag
  if wireType == 0
    then do
      x <- varint32
      if x == fromInt 0
        then pure $ Tuple fieldNumber False
        else pure $ Tuple fieldNumber True
    else fail $ "Expected bool, but got wire type " <> show wireType


-- | __string__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
string :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber String)
string = do
  tag fieldNumber wireType <- tag
  if wireType == 2
    then do
      len <- varint32
      stringview <- Parse.takeN len
      stringarray <- liftEffect $ mkTypedArray stringview
      case decodeUtf8 stringarray of
        Left err -> fail "string failed to decode UTF8"
        Right s -> pure s
    else fail $ "Expected string, but got wire type " <> show wireType
 where
  mkTypedArray :: DataView -> Effect Uint8Array
  mkTypedArray dv = do
    let buffer     = DV.buffer dv
        byteOffset = DV.byteOffset dv
        byteLength = DV.byteLength dv
    AT.part buffer byteOffset byteLength

-- | __bytes__
-- | [Scalar Value Type](https://developers.google.com/protocol-buffers/docs/proto3#scalar)
bytes :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber DataView)
bytes = do
  tag fieldNumber wireType <- tag
  if wireType == 2
    then Parse.takeN =<< varint32
    else fail $ "Expected bytes, but got wire type " <> show wireType


-- | https://stackoverflow.com/questions/2210923/zig-zag-decoding
zigZag32 :: UInt -> Int
zigZag32 n = toInt $ (n `shr` 1) .^. (unegate (n .&. 1))
 where unegate = fromInt <<< negate << toInt
    -- unegate x = complement x + (fromInt 1) -- TODO switch to this definition

tag :: forall m. MonadEffect m => ParserT DataView m (Tuple FieldNumber WireType)
tag = do
  n <- varint32
  pure $ Tuple (n `shr` 3) (toInt $ n .&. 3)

-- | https://developers.google.com/protocol-buffers/docs/encoding#varints
varint32 :: forall m. MonadEffect m => ParserT DataView m UInt
varint32 = do
  n_0 <- anyUInt8
  if n_0 < u0x80
    then pure n_0
    else do
      n_1 <- anyUInt8
      if n_1 < u0x80
        then pure $ (n_0 .&. u0x7F) .|. (n_1 `shl` u7)
        else do
          n_2 <- anyUInt8
          if n_2 < u0x80
            then pure
              $   (n_0 .&. u0x7F)
              .|. ((n_1 .&. u0x7F) `shl` u7)
              .|. (n_2 `shl` u14)
            else do
              n_3 <- anyUInt8
              if n_3 < u0x80
                then pure
                  $   (n_0 .&. u0x7F)
                  .|. ((n_1 .&. u0x7F) `shl` u7)
                  .|. ((n_2 .&. u0x7F) `shl` u14)
                  .|. (n_3 `shl` u21)
                else do
                  n_4 <- anyUint8
                  if n_4 < u0x10
                    then pure
                      $   (n_0 .&. u0x7F)
                      .|. ((n_1 .&. u0x7F) `shl` u7)
                      .|. ((n_2 .&. u0x7F) `shl` u14)
                      .|. ((n_3 .&. u0x7F) `shl` u21)
                      .|. ((n_4 `shl` u28)
                    else fail "varint32 overflow"
 where
  u7    = fromInt 7
  u14   = fromInt 14
  u21   = fromInt 21
  u28   = fromInt 28
  u0x10 = fromInt 0x10
  u0x7F = fromInt 0x7F
  u0x80 = fromInt 0x80

