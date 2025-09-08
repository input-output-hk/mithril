FROM ghcr.io/intersectmbo/cardano-node:10.5.1

# Copy static build of the Mithril client

COPY ./target/x86_64-unknown-linux-musl/release/mithril-client /usr/local/bin/mithril-client
RUN chmod +x /usr/local/bin/mithril-client

# Copy static build of the Mithril signer
COPY ./target/x86_64-unknown-linux-musl/release/mithril-signer /usr/local/bin/mithril-signer
RUN chmod +x /usr/local/bin/mithril-signer