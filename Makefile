%:
    @:

.PHONY: build

args = `arg="$(filter-out $@,$(MAKECMDGOALS))" && echo $${arg:-${1}}`

CARGO = cargo

build:
	$(MAKE) -C mithril-aggregator build
	$(MAKE) -C mithril-client build
	$(MAKE) -C mithril-signer build

build-debug:
	$(MAKE) -C mithril-aggregator debug
	$(MAKE) -C mithril-client debug
	$(MAKE) -C mithril-signer debug
