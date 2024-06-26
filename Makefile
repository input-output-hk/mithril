COMPONENTS = mithril-common mithril-stm mithril-aggregator mithril-client mithril-client-cli mithril-signer \
			internal/mithril-persistence internal/mithril-doc-derive internal/mithril-doc internal/mithril-build-script \
			demo/protocol-demo mithril-test-lab/mithril-end-to-end
GOALS := $(or $(MAKECMDGOALS),all)
NON_COMPONENT_GOALS := lint format

.PHONY: $(GOALS) $(COMPONENTS) $(NON_COMPONENT_GOALS)

$(filter-out $(NON_COMPONENT_GOALS),$(GOALS)): $(COMPONENTS)

$(COMPONENTS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

lint:
	prettier --check .

format:
	prettier --write .
