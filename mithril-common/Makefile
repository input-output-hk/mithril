.PHONY: all build test check doc check-all-features-set

CARGO = cargo
# All the crates features excluding the two that have little to no impact to the code
FEATURES := $(shell ${CARGO} metadata --quiet --no-deps \
	| jq -r '.packages[] | select(.name=="mithril-common") | .features \
	  | del(.allow_skip_signer_certification) | del(.portable) \
	  | keys | join(" ")')

all: test build

build:
	${CARGO} build --release --features full

test:
	${CARGO} test --features full

check:
	${CARGO} check --release --all-features --all-targets
	${CARGO} clippy --release --all-features --all-targets
	${CARGO} fmt --check

doc:
	${CARGO} doc --no-deps --open --features full

bench:
	${CARGO} bench --features full --verbose

# Compute the powerset of all the given features and save it to a file
.feature-sets:
	powerset() { [ $$# -eq 0 ] && echo || (shift; powerset "$$@") | while read r ; do echo "$$1 $$r"; echo "$$r"; done };\
	powerset $$(echo "$(FEATURES)") > .features-sets

check-all-features-set: .feature-sets
	# Read the file to run cargo clippy on all those features sets
	cat .features-sets | while read features_set; do \
		echo "Clippy common with feature '$$features_set''"; \
		${CARGO} clippy -p mithril-common --features "$$features_set"; \
	done
	echo "Clippy common without features"; \
	${CARGO} clippy -p mithril-common

	rm .features-sets
