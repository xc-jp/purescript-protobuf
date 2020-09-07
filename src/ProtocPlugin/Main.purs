module ProtocPlugin.Main where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))

-- import Protobuf.Decode as Decode
-- import Protobuf.Encode as Encode

import Node.Process (stdin, stdout)
import Node.Stream (read, writeString, uncork, onReadable)
import Node.Buffer (toArrayBuffer)
import Node.Encoding (Encoding(..))
import Data.ArrayBuffer.ArrayBuffer as AB

main :: Effect Unit
main = do
  onReadable stdin $ do
    stdinbufMay <- read stdin Nothing
    case stdinbufMay of
      Nothing -> pure unit
      Just stdinbuf -> do
        stdinab <- toArrayBuffer stdinbuf
        void $ writeString stdout UTF8 (show $ AB.byteLength stdinab) (pure unit)

