COMPONENTS = mithril-common mithril-stm mithril-aggregator mithril-client mithril-signer demo/protocol-demo mithril-test-lab/mithril-end-to-end
GOALS := $(or $(MAKECMDGOALS),all)

.PHONY: $(GOALS) $(COMPONENTS)

$(GOALS): $(COMPONENTS)

$(COMPONENTS):
	$(MAKE) -C $@ $(MAKECMDGOALS)
