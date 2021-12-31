
NOSETESTS=nosetests

WARNS=-D warnings -A dead-code -A non-camel-case-types -A non-upper-case-globals


default: build

build:
	cargo build --bin leema

run: build
	cargo run --bin leema

lib:
	cargo build --lib

test: unit T format

unit:
	cargo test --bin leema

T: build test.py
	${NOSETESTS} --with-xunit test.py

travis: unit travisT format

travisT: build test.py
	${NOSETESTS} --with-xunit --ignore-files=test_clientserver

warnings:
	cargo rustc --bin leema -- ${WARNS}

format:
	cargo +nightly fmt -- --check

reformat:
	cargo +nightly fmt

release:
	cargo build --bin leema --release

clean:
	cargo clean
