%:
    @:

.PHONY: all build test debug run clean help doc

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

clean:
	${CARGO} clean

help:
	@${CARGO} run -- -h

doc:
	${CARGO} doc --no-deps --open