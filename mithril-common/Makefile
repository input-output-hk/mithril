.PHONY: all build test check doc check-all-features-set

CARGO = cargo

all: test build

build:
	${CARGO} build --release --features full

test:
	${CARGO} test --features full

check:
	${CARGO} check --release --all-features --all-targets
	${CARGO} clippy --release --all-features --all-targets
	${CARGO} fmt --check

doc:
	${CARGO} doc --no-deps --open --features full

bench:
	${CARGO} bench --features full --verbose
