# Unit Tests

To test purescript-protobuf, run `nix-shell` from the top level directory
of the repo, then:

    npm install
    spago -x spago-protoc.dhall build
    protoc --purescript_out=./test/generated test/*.proto
    spago -x spago-test.dhall test

# Benchmarks

To run the benchmarks, run `nix-shell` from the top level directory, then:

    npm install
    spago -x spago-test.dhall run --main Test.Bench

