
NOSETESTS=nosetests


default: build

build: lexer parser
	cargo build --bin leema

run: build
	cargo run --bin leema

leemaw: lexer parser
	cargo build --bin leemaw

lib: lexer parser
	cargo build --lib

test: unit T

unit: lexer parser
	cargo test --bin leema

T: build test.py
	${NOSETESTS} --with-xunit

warnings: lexer parser
	cargo rustc --bin leema -- -D warnings

# lexer
lexer: target/debug/deps/libleemalex.a

target/debug/deps/libleemalex.a: target/debug/deps lexparse/lex.o lexparse/libleemalex.o
	ar cr target/debug/deps/libleemalex.a lexparse/lex.o lexparse/libleemalex.o

target/debug/deps:
	mkdir -p target/debug/deps

lexparse/lex.o: lexparse/lex.c lexparse/leema.h lexparse/leema.h
	gcc -c -fPIC -o lexparse/lex.o lexparse/lex.c

lexparse/libleemalex.o: lexparse/libleemalex.c lexparse/leema.h
	gcc -c -fPIC -o lexparse/libleemalex.o lexparse/libleemalex.c

lexparse/lex.c: lexparse/leema.h lexparse/leema.l
	flex -t lexparse/leema.l > lexparse/lex.c
	sed -i -r "s/static int yy_start/int yy_start/" lexparse/lex.c

lexparse/leema.h: lexparse/leema.rs
	./lemon -s -H -Tlemon_rust/lempar.rs lexparse/leema.y

# parser
parser: src/leema/parse.rs lexparse/leema.h

src/leema/parse.rs: lexparse/leema.rs
	cp lexparse/leema.rs src/leema/parse.rs

lexparse/leema.rs: lemon lexparse/leema.y lemon_rust/lempar.rs
	./lemon -g lexparse/leema.y
	./lemon -s -H -Tlemon_rust/lempar.rs lexparse/leema.y

lemon: lemon_rust/lemon_rust.c
	gcc -o lemon lemon_rust/lemon_rust.c

clean:
	rm -f src/parse.rs lexparse/leema.h lexparse/lex.c lexparse/*.o
	rm -f lexparse/leema.rs
	rm -f target/debug/deps/libleemalex.a
