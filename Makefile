
WARNS=-D warnings -A dead-code -A non-camel-case-types -A non-upper-case-globals


default: build

build:
	cargo build --bin leema

run: build
	cargo run --bin leema

lib:
	cargo build --lib

test: format unit T

unit:
	cargo test --bin leema

T: build test.py
	pytest --verbose test.py

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
