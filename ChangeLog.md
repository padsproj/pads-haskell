# Changelog for pads-haskell

## Unreleased changes

April 6, 2018:

* Removed type signature generation on Pads Tuple types (missing `Data`
  instance).
* Use [MWC](https://github.com/bos/mwc-random) instead of
  [normaldistribution](https://github.com/bjornbm/normaldistribution) (more
  recent, appears to have more frequent support).
* Lower case source directories and consolidated example files.
* Cabal-build based on an hpack package.yaml file.

