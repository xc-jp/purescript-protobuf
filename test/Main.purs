module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import My.Module.Test
import Test.Assert (assertEqual')

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
