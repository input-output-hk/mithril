COMPONENTS = mithril-common mithril-stm mithril-aggregator mithril-client mithril-client-cli mithril-signer \
			internal/mithril-persistence internal/mithril-doc-derive internal/mithril-doc internal/mithril-build-script \
			demo/protocol-demo mithril-test-lab/mithril-end-to-end
GOALS := $(or $(MAKECMDGOALS),all)
NON_COMPONENT_GOALS := check-format format

.PHONY: $(GOALS) $(COMPONENTS) $(NON_COMPONENT_GOALS)

$(filter-out $(NON_COMPONENT_GOALS),$(GOALS)): $(COMPONENTS)

$(COMPONENTS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

check-format:
	@echo 'Note: Rust is not checked by this recipe, use `make check`'
	@which prettier >/dev/null || echo 'It seems 'prettier' is not installed or not in the path. see https://prettier.io/docs/en/install';
	prettier --check .

format:
	@which prettier >/dev/null || echo 'It seems 'prettier' is not installed or not in the path. see https://prettier.io/docs/en/install';
	prettier --write .
