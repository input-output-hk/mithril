COMPONENTS = mithril-common mithril-stm mithril-aggregator mithril-client mithril-client-cli mithril-signer \
			internal/mithril-persistence internal/mithril-doc-derive internal/mithril-doc internal/mithril-build-script \
			demo/protocol-demo mithril-test-lab/mithril-end-to-end

LINT_COMPONENTS = mithril-explorer mithril-client-wasm docs/website

GOALS := $(filter-out lint,$(or $(MAKECMDGOALS),all))

.PHONY: $(GOALS) $(COMPONENTS) lint $(LINT_COMPONENTS)

$(GOALS): $(COMPONENTS)

# Default rule to handle general targets
$(COMPONENTS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

# Special rule for lint
lint: $(LINT_COMPONENTS)

$(LINT_COMPONENTS):
	$(MAKE) -C $@ lint
