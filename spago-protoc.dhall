-- Spago configuration the protoc compiler plugin.
--
-- See:
-- https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
--
-- Usage:
--
--     spago -x spago-protoc.dhall build
--

let conf = ./spago.dhall

in conf //
  -- don't include conformance/generated/*.purs in sources because it will conflict
  -- with test/generated/*.purs
  { sources = [ "src/**/*.purs" ]
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
