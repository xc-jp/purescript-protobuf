To regenerate `spago-packages.nix`, run from the top level directory:

```sh
spago2nix generate 5 -- -x spago-plugin.dhall
```

then `mv spago-packages.nix` into this directory.
