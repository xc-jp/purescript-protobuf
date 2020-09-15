# Development Notes

## Generate `packages.json` from `packages.dhall` for `psc-package`

https://psc-package.readthedocs.io/en/latest/usage.html#local-package-sets

```
dhall-to-json –file packages.dhall –output packages.json
```

## Generate docs

```
spago docs
```

## Publish

https://pursuit.purescript.org/help/authors

Because `pulp` still needs `bower`, we have to tag the version by hand with

```
git commit --allow-empty
```

then

```
bower install
pulp publish --no-push
```

