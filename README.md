leema
======

[![Build Status](https://api.travis-ci.org/mdg/leema.png?branch=master)](https://travis-ci.org/mdg/leema)

Leema is a concurrent, error-tolerant, functional programming language.

## Build

Dependencies:
* lemon_rust submodule
* C/C++ std libraries
* flex

Build the leema vm by running `make`

## Tests

All tests
```
make test
```

Unit tests only
```
make unit
```

Integration tests only
```
make T
```
