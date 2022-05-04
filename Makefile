COMPONENTS = mithril-core mithril-aggregator mithril-client mithril-signer demo/protocol-demo mithril-test-lab
GOALS := $(or $(MAKECMDGOALS),all)

.PHONY: $(GOALS) $(COMPONENTS)

$(GOALS): $(COMPONENTS)

$(COMPONENTS):
	$(MAKE) -C $@ $(MAKECMDGOALS)
