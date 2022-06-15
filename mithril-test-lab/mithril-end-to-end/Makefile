%:
    @:

.PHONY: all build test checkclean doc

args = `arg="$(filter-out $@,$(MAKECMDGOALS))" && echo $${arg:-${1}}`

CARGO = cargo

all: test build

build:
	${CARGO} build --release
	cp ../../target/release/mithril-end-to-end .

test:
	${CARGO} test

check:
	${CARGO} check --all-features
	${CARGO} clippy --all-features

clean:
	${CARGO} clean

doc:
	${CARGO} doc --no-deps --open
