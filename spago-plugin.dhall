-- Spago configuration the protoc compiler plugin.
--
-- See:
-- https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
--
-- Usage:
--
--     spago -x spago-plugin.dhall build
--

let conf = ./spago.dhall

in conf //
  { sources = [ "src/**/*.purs", "plugin/**/*.purs" ]
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
