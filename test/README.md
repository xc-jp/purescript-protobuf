# Unit Tests

To test purescript-protobuf, run `nix-shell` from the top level directory
of the repo, then:

    npm install
    spago build
    protoc --purescript_out=./test/generated test/*.proto
    spago -x test.dhall build
    spago -x test.dhall test