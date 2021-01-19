leema
======

[![Build Status](https://api.travis-ci.org/mdg/leema.png?branch=master)](https://travis-ci.org/mdg/leema)

Leema is a concurrent, error-tolerant, functional programming language.

## Build

Dependencies:
* C/C++ std libraries
* llvm

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

Copyright 2021 Matthew Graham
