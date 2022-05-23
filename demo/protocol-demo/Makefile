%:
    @:

.PHONY: all build test check debug run clean help doc

args = `arg="$(filter-out $@,$(MAKECMDGOALS))" && echo $${arg:-${1}}`

CARGO = cargo

all: test build

build:
	${CARGO} build --release
	cp ../../target/release/mithrildemo .

run: build
	@./mithrildemo

debug:
	@${CARGO} run -- -vvv

test:
	${CARGO} test

check:
	${CARGO} check --all-features
	${CARGO} clippy --all-features

clean:
	${CARGO} clean

help:
	@${CARGO} run -- -h

doc:
	${CARGO} doc --no-deps --open
