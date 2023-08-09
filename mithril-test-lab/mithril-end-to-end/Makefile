%:
    @:

.PHONY: all build test check clean doc monitor

args = `arg="$(filter-out $@,$(MAKECMDGOALS))" && echo $${arg:-${1}}`

CARGO = cargo

all: test build

build:
	${CARGO} build --release
	cp ../../target/release/mithril-end-to-end .
	cp ../../target/release/load-aggregator .

test:
	${CARGO} test

check:
	${CARGO} check --release --all-features --all-targets
	${CARGO} clippy --release --all-features --all-targets
	${CARGO} fmt --check

clean:
	${CARGO} clean

doc:
	${CARGO} doc --no-deps --open

monitor:
	CURRENT_UID=$(id -u) docker compose -f monitor/docker-compose.yaml --profile monitoring up --remove-orphans --force-recreate
