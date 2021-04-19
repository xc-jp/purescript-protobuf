-- Use this spago file to depend only on the library modules of __protobuf__.
-- This spago file will not pull in any of the code-generation dependencies.
-- The code-generation dependencies are not needed when compiling a program
-- which imports generated code.

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
  ]
, packages = ./packages.dhall
, sources = [ "src/Protobuf/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/xc-jp/purescript-protobuf"
}
