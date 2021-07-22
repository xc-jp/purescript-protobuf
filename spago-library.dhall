-- Use this spago file to depend only on the library modules of __protobuf__.
-- This spago file will not pull in any of the code-generation dependencies.
-- The code-generation dependencies are not needed when compiling a program
-- which imports generated code.
let spagodhall = ./spago.dhall
in spagodhall //
  { dependencies =
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
    ]
  , sources = [ "src/Protobuf/*.purs" ]
  }
