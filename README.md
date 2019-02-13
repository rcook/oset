[![Travis CI](https://img.shields.io/travis/rcook/oset/master.svg)](https://travis-ci.org/rcook/oset)
[![Code coverage](https://coveralls.io/repos/github/rcook/oset/badge.svg?branch=master)](https://coveralls.io/github/rcook/oset?branch=master)
[![Licence](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/rcook/oset/master/LICENSE)

[![Hackage](https://img.shields.io/hackage/v/oset.svg)](http://hackage.haskell.org/package/oset)
[![Hackage dependencies](https://img.shields.io/hackage-deps/v/oset.svg)](http://hackage.haskell.org/package/oset)
[![Stackage Nightly](http://stackage.org/package/oset/badge/nightly)](http://stackage.org/nightly/package/oset)
[![Stackage LTS](http://stackage.org/package/oset/badge/lts)](http://stackage.org/lts/package/oset)

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
