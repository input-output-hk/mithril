.PHONY: all build test check doc check-all-features-set

CARGO = cargo

all: test build

build:
	${CARGO} build --release

test:
	${CARGO} test

check:
	${CARGO} check --release --all-features --all-targets
	${CARGO} clippy --release --all-features --all-targets
	${CARGO} fmt --check

doc:
	${CARGO} doc --no-deps --open

bench:
	${CARGO} bench --verbose
