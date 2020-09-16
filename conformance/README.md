# Protocol Buffer Conformance Testing

[Conformance README](https://github.com/protocolbuffers/protobuf/tree/master/conformance)

The `protobuf.nix` file has a derivation attribute `conformance`.

Build the conformance test runner.

```
nix-build -A conformance protobuf.nix
```

From the `protobuf/conformance/Makefile.am`, here is an example of
[how to run the conformance test runner](https://github.com/protocolbuffers/protobuf/blob/f4aa17b28af168cb3168f029de796d5994c321f6/conformance/Makefile.am#L334-L336) for C++.

> ```
> ./conformance-test-runner --enforce_recommended --failure_list failure_list_cpp.txt ./conformance-cpp
> ```

We can do

```
./result/bin/conformance-test-runner --enforce_recommended ./result/bin/conformance-cpp
```

## Writing a conformance test program

See `./result/conformance/conformance.proto`.

