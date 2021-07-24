-- Spago configuration for testing.
--
-- See:
-- https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
--
-- Usage:
--
--     spago -x spago-test.dhall test
--

let conf = ./spago.dhall

in conf //
  -- don't include conformance/generated/*.purs in sources because it will conflict
  -- with test/generated/*.purs
  { sources = [ "test/**/*.purs", "src/Protobuf/**/*.purs",  ]
  , dependencies = conf.dependencies #
    [ "assert"
    , "psci-support"
    , "minibench"
    , "console"
    , "math"
    , "unfoldable"
    , "node-buffer"
    , "node-path"
    , "node-process"
    , "node-streams"
    , "debug"
    ]
  }
