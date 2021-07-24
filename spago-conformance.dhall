-- Spago configuration for running the conformance checker
--
-- See:
-- https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
--
-- Usage:
--
--     spago -x spago-conformance.dhall build
--

let conf = ./spago.dhall

in conf //
  { sources = [ "conformance/**/*.purs", "src/**/*.purs" ]
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
