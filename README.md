[![Travis branch](https://img.shields.io/travis/rcook/oset/master.svg)](https://travis-ci.org/rcook/oset)
[![Hackage](https://img.shields.io/hackage/v/oset.svg)](http://hackage.haskell.org/package/oset)
[![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/rcook/oset/master/LICENSE)

# oset

An insertion-order-preserving set

* Implements subset of `OSet` API from [`ordered-containers`][ordered-containers] but is otherwise API-compatible with it
* Adds useful instances for `Semigroup`, `Monoid`, `Data`

## Documentation

[View documentation][docs]

## Build and test

```bash
stack clean
stack build --test
```

## Licence

[MIT License](LICENSE)

[docs]: http://rcook.github.io/oset
[ordered-containers]: http://hackage.haskell.org/package/ordered-containers-0.1.1
