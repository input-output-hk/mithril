.PHONY: all build test check debug run help doc

CARGO = cargo

all: test build

build:
	${CARGO} build --release
	cp ../target/release/mithril-signer .

run: build
	./mithril-signer

debug:
	${CARGO} run -- -vvv

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

docker-build:
	cd ../ && docker build -t mithril/mithril-signer -f mithril-signer/Dockerfile .

docker-run:
	docker run --rm --name='mithril-signer' mithril/mithril-signer