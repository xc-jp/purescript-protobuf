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
  , "longs"
  , "parsing"
  , "parsing-dataview"
  , "text-encoding"
  , "uint"
  , "arrays"
  , "control"
  , "effect"
  , "enums"
  , "float32"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "record"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "either"
  , "quickcheck" -- needed by longs v0.1.1
  ]
, packages = ./packages.dhall
, sources = [ "src/Protobuf/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/xc-jp/purescript-protobuf"
}
