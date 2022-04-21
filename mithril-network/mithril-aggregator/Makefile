.PHONY: all build test check debug run help doc

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

check:
	${CARGO} check --all-targets
	${CARGO} clippy --all-targets

help:
	@${CARGO} run -- -h

doc:
	${CARGO} doc --no-deps --open