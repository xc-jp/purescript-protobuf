{-
-}
{ name = "protobuf-library"
, dependencies =
  [ "arraybuffer"
  , "arraybuffer-builder"
  , "arraybuffer-types"
  , "longs"
  , "node-buffer"
  , "node-path"
  , "node-process"
  , "node-streams"
  , "parsing"
  , "parsing-dataview"
  , "text-encoding"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/xc-jp/purescript-protobuf-library"
}
