-- Spago configuration for development.
--
-- See:
-- https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
--
--

let conf = ./spago.dhall

in conf //
  { sources = [ "src/**/*.purs", "plugin/**/*.purs", "test/**/*.purs", "conformance/**/*.purs" ]
  , dependencies = conf.dependencies #
    [ "assert"
    , "minibench"
    , "console"
    , "unfoldable"
    , "node-buffer"
    , "node-path"
    , "node-process"
    , "numbers"
    , "node-streams-aff"
    , "aff"
    , "unsafe-coerce"
    , "unicode"
    ]
  }
