.PHONY: all build test check debug run help doc

args = `arg="$(filter-out $@,$(MAKECMDGOALS))" && echo $${arg:-${1}}`

CARGO = cargo
CARGO_BUILD_ARGS ?=

all: test build

build:
	${CARGO} build --release ${CARGO_BUILD_ARGS}
	cp ../target/release/mithril-setup .

run: build
	@./mithril-setup $(call args, "signer --unstable")

debug:
	@${CARGO} run -- -vvv $(call args, "signer --unstable")

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