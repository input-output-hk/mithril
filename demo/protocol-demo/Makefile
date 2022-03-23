.PHONY: all build test run help doc

CARGO = cargo

all: version test build

build:
	${CARGO} build --release
	cp ./target/release/mithrildemo .

run:
	${CARGO} run

test:
	${CARGO} test

help:
	@${CARGO} run -- -h

doc:
	${CARGO} doc