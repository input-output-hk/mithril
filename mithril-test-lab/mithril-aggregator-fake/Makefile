.PHONY: all build test check debug run help doc

CARGO = cargo

all: test build

build:
	${CARGO} build --release
	cp ../../target/release/mithril-aggregator-fake .

run: build
	./mithril-aggregator-fake

debug:
	${CARGO} run -- -vvv

test:
	${CARGO} test

check:
	${CARGO} check --release --all-features --all-targets
	${CARGO} clippy --release --all-features --all-targets
	${CARGO} fmt --check

help:
	@${CARGO} run -- -h

doc:
	${CARGO} doc --no-deps --open
