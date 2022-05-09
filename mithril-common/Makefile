.PHONY: all build test check debug run help doc

CARGO = cargo

all: test build

build:
	${CARGO} build --release

test:
	${CARGO} test

check:
	${CARGO} check --all-features
	${CARGO} clippy --all-features

doc:
	${CARGO} doc --no-deps --open