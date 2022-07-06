###############################
# STEP 1: build rust executable
###############################
FROM rust:buster AS rustbuilder

# Upgrade and install build base
RUN apt-get update && apt-get install -y libssl-dev

# Create appuser
RUN adduser --no-create-home --disabled-password appuser

WORKDIR /app

# Copy local dependencies
COPY mithril-core/ /mithril-core/
COPY Cargo.lock /mithril-core/
COPY mithril-common/ /mithril-common/
COPY Cargo.lock /mithril-common/

# Build the app using a dummy main in order to cache dependencies
COPY mithril-signer/Cargo.toml /app
COPY Cargo.lock /app/
RUN mkdir -p /app/src/ && echo "fn  main () {}" > /app/src/main.rs
RUN cargo build --release --manifest-path /app/Cargo.toml

# Copy the rest of the files into the container
COPY ./mithril-signer /app/

# Build the binary
RUN cargo build --release

###############################
# STEP 2: build a small image
###############################
FROM debian:buster-slim

# Upgrade
RUN apt-get update && apt-get install -y libssl1.1 ca-certificates && rm -rf /var/lib/apt/lists/*

# Import the user and group files from the builder
COPY --from=rustbuilder /etc/passwd /etc/passwd

# Copy the executable
COPY --from=rustbuilder /app/target/release/mithril-signer /app/bin/mithril-signer

# Copy the config files
COPY --from=rustbuilder /app/config /app/config

#Workdir
WORKDIR /app/

# Install cardano-cli
RUN wget -nv -O cardano-bin.tar.gz https://hydra.iohk.io/build/13065769/download/1/cardano-node-1.35.0-linux.tar.gz
RUN tar xzf cardano-bin.tar.gz ./cardano-cli && mv cardano-cli /app/bin
RUN rm -f cardano-bin.tar.gz

# Use an unprivileged user
USER appuser

# Run the executable
ENTRYPOINT ["/app/bin/mithril-signer"]