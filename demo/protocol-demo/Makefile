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
	${CARGO} check --release --all-features --all-targets
	${CARGO} clippy --release --all-features
	${CARGO} fmt --check

clean:
	${CARGO} clean

help:
	@${CARGO} run -- -h

doc:
	${CARGO} doc --no-deps --open
