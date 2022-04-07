.PHONY: all build test debug run help doc

CARGO = cargo

all: test build

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