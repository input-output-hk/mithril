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
	# Check that we are on Linux, then build
	if [ "$$(uname -s)" != "Linux" ]; then \
		echo "Error: this target only supports Linux"; \
		exit 1; \
	fi; \
	RAW_ARCH=$$(uname -m); \
	if [ "$$RAW_ARCH" = "x86_64" ]; then \
		ARCH="amd64"; \
	elif [ "$$RAW_ARCH" = "aarch64" ]; then \
		ARCH="arm64"; \
	else \
		echo "Error: unsupported architecture $$RAW_ARCH"; \
		exit 1; \
	fi; \
	mkdir -p ./bin-linux-$${ARCH}; \
	cp ../target/release/mithril-relay ./bin-linux-$${ARCH}/mithril-relay
	cd ../ && docker build -t mithril/mithril-relay -f mithril-relay/Dockerfile.ci --build-arg DOCKER_IMAGE_FROM .

docker-run:
	docker run --rm --name='mithril-relay' mithril/mithril-relay