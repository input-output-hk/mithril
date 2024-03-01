COMPONENTS = mithril-common mithril-stm mithril-aggregator mithril-client mithril-client-cli mithril-signer \
			internal/mithril-persistence internal/mithril-doc-derive internal/mithril-doc internal/mithril-build-script \
			demo/protocol-demo mithril-test-lab/mithril-end-to-end
GOALS := $(or $(MAKECMDGOALS),all)

.PHONY: $(GOALS) $(COMPONENTS)

$(GOALS): $(COMPONENTS)

$(COMPONENTS):
	$(MAKE) -C $@ $(MAKECMDGOALS)
