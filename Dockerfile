FROM ghcr.io/intersectmbo/cardano-node:10.5.1

# Copy static build of the Mithril client
COPY ./target/x86_64-unknown-linux-musl/release/mithril-client /usr/local/bin/mithril-client
RUN chmod +x /usr/local/bin/mithril-client

# Copy static build of the Mithril signer
COPY ./target/x86_64-unknown-linux-musl/release/mithril-signer /usr/local/bin/mithril-signer
RUN chmod +x /usr/local/bin/mithril-signer

# Copy configuration files for the Mithril signer
COPY ./mithril-infra/configuration/ /usr/local/bin/mithril-configuration/

# Update entrypoints of bundle
COPY ./static-docker/bundle-entrypoint /usr/local/bin/bundle-entrypoint 
RUN chmod +x /usr/local/bin/bundle-entrypoint
COPY ./static-docker/mithril-entrypoint /usr/local/bin/mithril-entrypoint 
RUN chmod +x /usr/local/bin/mithril-entrypoint

ENTRYPOINT ["/usr/local/bin/bundle-entrypoint"]