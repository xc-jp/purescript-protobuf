module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Protobuf.Decode as Decode
import Protobuf.Encode as Encode

main :: Effect Unit
main = do
  log "🍝"
