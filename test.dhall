-- Spago configuration for running the tests
--
-- See:
-- https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
--
-- Usage:
--
--     spago -x test.dhall test
--
-- Also use this for a repl:
--
--     spago -x test.dhall repl
--

let conf = ./spago.dhall

in conf //
  { sources = [ "test/**/*.purs", "src/Protobuf/*.purs" ]
  , dependencies = conf.dependencies #
    [ "assert"
    , "psci-support"
    , "minibench"
    ]
  }
