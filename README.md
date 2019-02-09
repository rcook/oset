[![Travis CI](https://img.shields.io/travis/rcook/oset/master.svg)](https://travis-ci.org/rcook/oset)
[![Hackage](https://img.shields.io/hackage/v/oset.svg)](http://hackage.haskell.org/package/oset)
[![Hackage dependencies](https://img.shields.io/hackage-deps/v/oset.svg)](http://hackage.haskell.org/package/oset)
[![Licence](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/rcook/oset/master/LICENSE)

# oset

An insertion-order-preserving set

* Mostly API-compatible with `OSet` [`ordered-containers`][ordered-containers]
* Adds class instance for `Data`
* Adds class instances for `Semigroup` and `Monoid` via `OSetL` and `OSetR` wrappers

## Incompatibilities with `OSet` from [`ordered-containers`][ordered-containers]

* `fromList` is renamed `fromListL`
* `fromListR` is introduced by analogy with `fromListL`

## Documentation

[View documentation on Hackage][docs]

## Licence

[MIT License](LICENSE)

[docs]: http://hackage.haskell.org/package/oset
[ordered-containers]: http://hackage.haskell.org/package/ordered-containers-0.1.1
