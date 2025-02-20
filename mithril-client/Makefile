%:
    @:

.PHONY: all build test check clean doc

args = `arg="$(filter-out $@,$(MAKECMDGOALS))" && echo $${arg:-${1}}`

CARGO = cargo
FEATURES := fs unstable

all: test build

build:
	${CARGO} build --release --features full,unstable

test:
	${CARGO} test --features full,unstable

check:
	${CARGO} check --release --all-features --all-targets
	${CARGO} clippy --release --all-features --all-targets
	${CARGO} fmt --check

clean:
	${CARGO} clean

doc:
	${CARGO} doc --no-deps --open --features full

# Compute the powerset of all the given features and save it to a file
.feature-sets:
	powerset() { [ $$# -eq 0 ] && echo || (shift; powerset "$$@") | while read r ; do echo "$$1 $$r"; echo "$$r"; done };\
	powerset $$(echo "$(FEATURES)") > .features-sets

check-all-features-set: .feature-sets
	# Read the file to run cargo clippy on all those features sets
	cat .features-sets | while read features_set; do \
		echo "Clippy client with feature '$$features_set''"; \
		${CARGO} clippy -p mithril-client --features "$$features_set"; \
	done
	echo "Clippy client without features"; \
	${CARGO} clippy -p mithril-client

	rm .features-sets
