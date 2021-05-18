-- Spago configuration for running the conformance checker
--
-- See:
-- https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
--
-- Usage:
--
--     spago -x conformance.dhall build
--

let conf = ./spago.dhall

in conf //
  { sources = [ "conformance/**/*.purs", "src/Protobuf/*.purs" ]
  , dependencies = conf.dependencies #
    [ "debug"
    , "aff"
    , "aff-promise"
    ]
  }
