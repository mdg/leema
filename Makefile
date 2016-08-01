

default: build

build: lexer parser
	cargo build

run: build
	cargo run

test: unittest T

unittest: lexer parser
	cargo test

T: build test.py
	nosetests

# lexer
lexer: target/debug/deps/libleemalex.a

target/debug/deps/libleemalex.a: target/debug/deps lexparse/lex.o lexparse/libleemalex.o
	ar cr target/debug/deps/libleemalex.a lexparse/lex.o lexparse/libleemalex.o

target/debug/deps:
	mkdir -p target/debug/deps

lexparse/lex.o: lexparse/lex.c lexparse/leema.h lexparse/leema.h
	gcc -c -o lexparse/lex.o lexparse/lex.c

lexparse/libleemalex.o: lexparse/libleemalex.c lexparse/leema.h
	gcc -c -fPIC -o lexparse/libleemalex.o lexparse/libleemalex.c

lexparse/lex.c: lexparse/leema.h lexparse/leema.l
	flex -t lexparse/leema.l > lexparse/lex.c
	sed -i -r "s/static int yy_start/int yy_start/" lexparse/lex.c

lexparse/leema.h: lexparse/leema.rs
	-./lemon -s -H -Tlemon_rust/lempar.rs lexparse/leema.y

# parser
parser: src/parse.rs lexparse/leema.h

src/parse.rs: lexparse/leema.rs
	cp lexparse/leema.rs src/parse.rs

lexparse/leema.rs: lemon lexparse/leema.y lemon_rust/lempar.rs
	./lemon -g lexparse/leema.y
	-./lemon -s -H -Tlemon_rust/lempar.rs lexparse/leema.y

lemon: lemon_rust/lemon_rust.c
	gcc -o lemon lemon_rust/lemon_rust.c

clean:
	rm -f src/parse.rs lexparse/leema.h lexparse/lex.c lexparse/*.o
	rm -f lexparse/leema.rs
	rm -f target/debug/deps/libleemalex.a
