.PHONY: all build test check debug run help doc

CARGO = cargo

all: test build

build:
	${CARGO} build --release
	cp ../target/release/mithril-aggregator .

run: build
	./mithril-aggregator

debug:
	${CARGO} run -- -vvv

test:
	${CARGO} test

check:
	${CARGO} check --release --all-features --all-targets
	${CARGO} clippy --release --all-features

help:
	@${CARGO} run -- -h

doc:
	${CARGO} doc --no-deps --open

docker-build:
	cd ../ && docker build -t mithril/mithril-aggregator -f mithril-aggregator/Dockerfile .

docker-run:
	docker run --rm -p 8080:8080 --name='mithril-aggregator' mithril/mithril-aggregator
