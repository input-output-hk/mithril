.PHONY: all build test check debug run help doc

CARGO = cargo

all: test build

build:
	${CARGO} build --release
	cp ../target/release/mithril-relay .
	
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
	cd ../ && docker build -t mithril/mithril-relay -f mithril-relay/Dockerfile .

docker-build-ci: build
	cd ../ && docker build -t mithril/mithril-relay -f mithril-relay/Dockerfile.ci --build-arg DOCKER_IMAGE_FROM .

docker-run:
	docker run --rm --name='mithril-relay' mithril/mithril-relay