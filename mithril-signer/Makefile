.PHONY: all build test check debug run help doc

CARGO = cargo

all: test build

build:
	${CARGO} build --release
	cp ../target/release/mithril-signer .

run: build
	./mithril-signer

debug:
	${CARGO} run -- -vvv

test:
	${CARGO} test

check:
	${CARGO} check --all-features
	${CARGO} clippy --all-features

help:
	@${CARGO} run -- -h

doc:
	${CARGO} doc --no-deps --open