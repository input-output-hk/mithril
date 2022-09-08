%:
    @:

.PHONY: all build test check debug run clean help doc

args = `arg="$(filter-out $@,$(MAKECMDGOALS))" && echo $${arg:-${1}}`

CARGO = cargo

all: test build

build:
	${CARGO} build --release
	cp ../target/release/mithril-client .

run: build
	@./mithril-client $(call args,defaultstring)

debug:
	@${CARGO} run -- -vvv $(call args,defaultstring)

test:
	${CARGO} test

check:
	${CARGO} check --release --all-features --all-targets
	${CARGO} clippy --release --all-features --all-targets
	${CARGO} fmt --check

clean:
	${CARGO} clean

help:
	@${CARGO} run -- -h

doc:
	${CARGO} doc --no-deps --open

docker-build:
	cd ../ && docker build -t mithril/mithril-client -f mithril-client/Dockerfile .

docker-run:
	docker run --rm --name='mithril-client' mithril/mithril-client
