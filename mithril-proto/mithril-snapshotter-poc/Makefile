.PHONY: snapshot-full

CARGO = cargo

all: snapshot-full

snapshot-full:
	@sh mithril-snapshot.sh full $(src) $(dest)

snapshot-light:
	@sh mithril-snapshot.sh light $(src) $(dest)