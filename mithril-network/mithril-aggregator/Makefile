.PHONY: all build test run help doc

CARGO = cargo

all: version test build

build:
	${CARGO} build --release
	cp ./target/release/mithril-aggregator .

run: build
	./mithril-aggregator

debug:
	${CARGO} run -- -vvv

test:
	${CARGO} test

help:
	@${CARGO} run -- -h

doc:
	${CARGO} doc --no-deps --open