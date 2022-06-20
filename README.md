leema
======

![make unit](https://github.com/mdg/leema/workflows/leema-unit/badge.svg)
Rust Unit Tests (cargo t)
<br/>
![make format](https://github.com/mdg/leema/workflows/leema-fmt/badge.svg)
Code Formatting Check
<br/>
![make T](https://github.com/mdg/leema/workflows/makeT/badge.svg)
Integration Tests (pytest)
<br/>

Leema is a concurrent, error-tolerant, functional programming language.

## Build

Dependencies:
* Rust Stable
* Rust Nightly for formatting
* Pytest for integration tests

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

Copyright 2022 Matthew Graham
