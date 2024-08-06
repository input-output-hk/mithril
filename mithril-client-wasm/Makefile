%:
    @:

.PHONY: all build test check clean doc

args = `arg="$(filter-out $@,$(MAKECMDGOALS))" && echo $${arg:-${1}}`

CARGO = cargo

all: test build

build:
	wasm-pack build --target web --release --out-name index

test:
	pkill -f "mithril-aggregator-fake" || true
	${CARGO} run -p mithril-aggregator-fake -- -p 8000 &
	if wasm-pack test --headless --firefox --chrome --node --release; then \
		pkill -f "mithril-aggregator-fake" || true; \
	else \
		pkill -f "mithril-aggregator-fake" || true; \
		exit 1; \
	fi

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

www-test-install:
	npm --prefix www-test install

www-test-serve:
	npm --prefix www-test run start

upgrade-www-deps: build
	make -C www upgrade
	make -C www-test upgrade

lint:
	make -C www lint
	make -C www-test lint

format:
	make -C www format
	make -C www-test format
