leema
======

[!Build Status](https://github.com/mdg/leema/workflows/leema-unit/badge.svg)

Leema is a concurrent, error-tolerant, functional programming language.

## Build

Dependencies:
* Rust Stable
* Rust Nightly for formatting
* Nosetests for integration tests

Build the leema vm by running `make`

## Tests

All tests
```
make test
```

Unit tests only
```
cargo test
```

Integration tests only
```
make T
```

Copyright 2022 Matthew Graham
