%:
    @:

.PHONY: all build test check debug run clean help doc

args = `arg="$(filter-out $@,$(MAKECMDGOALS))" && echo $${arg:-${1}}`

CARGO = cargo

all: test build

build:
	${CARGO} build --release
	cp ./target/release/mithril-client .

run: build
	@./mithril-client $(call args,defaultstring)

debug:
	@${CARGO} run -- -vvv $(call args,defaultstring)

test:
	${CARGO} test

check:
	${CARGO} check
	${CARGO} clippy

clean:
	${CARGO} clean

help:
	@${CARGO} run -- -h

doc:
	${CARGO} doc --no-deps --open