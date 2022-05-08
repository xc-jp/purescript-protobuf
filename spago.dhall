-- Spago configuration for the library dependencies.
--
-- See:
-- https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
--
{ name = "protobuf"
, dependencies =
  [ "arraybuffer"
  , "arraybuffer-builder"
  , "arraybuffer-types"
  , "arrays"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "float32"
  , "foldable-traversable"
  , "functions"
  , "int64"
  , "maybe"
  , "newtype"
  , "parsing"
  , "parsing-dataview"
  , "prelude"
  , "record"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "uint"
  , "web-encoding"
  ]
, packages = ./packages.dhall
, sources = [ "src/Protobuf/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/xc-jp/purescript-protobuf"
}
