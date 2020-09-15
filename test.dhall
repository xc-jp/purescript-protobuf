-- Spago configuration for running the tests
--
-- See:
-- https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
--
-- Usage:
--
--     spago -x test.dhall test
--

let conf = ./spago.dhall

in conf //
  { sources = [ "test/**/*.purs", "src/Protobuf/*.purs" ]
  , dependencies = conf.dependencies # [ "assert" ]
  }
