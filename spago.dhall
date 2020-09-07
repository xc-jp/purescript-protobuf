{-
-}
{ name = "protobuf-library"
, dependencies =
  [ "parsing"
  , "parsing-dataview"
  , "arraybuffer-types"
  , "arraybuffer"
  , "arraybuffer-builder"
  , "uint"
  , "text-encoding"
  , "longs"
  , "node-streams"
  , "node-process"
  , "node-buffer"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/xc-jp/purescript-protobuf-library"
}
