# Development Notes

## Generate `packages.json` from `packages.dhall` for `psc-package`

https://psc-package.readthedocs.io/en/latest/usage.html#local-package-sets

```
nix-shell
dhall-to-json –file packages.dhall –output packages.json
```

## Generate docs

```
nix-shell
spago docs
```

## Generate `spago-packages.nix`

https://github.com/justinwoo/spago2nix

https://github.com/purescript/spago/issues/547

```
nix-shell
spago2nix generate
mv spago-packages.nix nix/
```

## Publish

https://pursuit.purescript.org/help/authors

Because `pulp` still needs `bower`, we have to tag the version by hand with

```
git commit --allow-empty
```

then

```
nix-shell
bower install
pulp publish --no-push
```

