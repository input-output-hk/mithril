%:
    @:

.PHONY: all build test check clean doc

args = `arg="$(filter-out $@,$(MAKECMDGOALS))" && echo $${arg:-${1}}`

CARGO = cargo

all: test build

build:
	wasm-pack build --release --out-name index

test:
	#wasm-pack test --headless --firefox --chrome --node --release
	wasm-pack test --headless --firefox --release

check:
	${CARGO} check --release --all-features --all-targets
	${CARGO} clippy --release --all-features --all-targets
	${CARGO} fmt --check

clean:
	${CARGO} clean

doc:
	${CARGO} doc --no-deps --open --features full

www-install:
	npm --prefix www install

www-serve:
	npm --prefix www run start
