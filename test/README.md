# Unit Tests

To test purescript-protobuf, run `nix-shell` from the top level directory
of the repo, then:

    npm install
    spago build
    protoc --purescript_out=./test/generated test/*.proto
    spago -x test.dhall build
    spago -x test.dhall test

# Benchmarks

To run the benchmarks, run `nix-shell` then:

```
    npm install
    spago -x test.dhall run --main Test.Bench
```