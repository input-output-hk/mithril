.PHONY: all build test check debug run help doc

CARGO = cargo

all: test build

build:
	# We use 'portable' feature to avoid SIGILL crashes
	${CARGO} build --release --features portable

test:
	${CARGO} test

check:
	${CARGO} check --release --all-features --all-targets
	${CARGO} clippy --release --all-features --all-targets
	${CARGO} fmt --check

doc:
	${CARGO} doc --no-deps --open