%:
    @:

.PHONY: all build test check debug run clean help doc

args = `arg="$(filter-out $@,$(MAKECMDGOALS))" && echo $${arg:-${1}}`

CARGO = cargo

all: test build

build:
	${CARGO} build --release
	cp ../target/release/mithril-client .

build-ci:
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
	${CARGO} build --release; \
	mkdir -p ./bin-linux-$${ARCH}; \
	cp ../target/release/mithril-client ./bin-linux-$${ARCH}/mithril-client

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
	cd ../ && docker build -t mithril/mithril-client -f mithril-client-cli/Dockerfile .

docker-build-ci: build-ci
	cd ../ && docker build -t mithril/mithril-client -f mithril-client-cli/Dockerfile.ci --build-arg DOCKER_IMAGE_FROM .

docker-run:
	docker run --rm --name='mithril-client' mithril/mithril-client
