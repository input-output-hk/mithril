###############################
# STEP 1: build rust executable
###############################
FROM rust:bullseye AS rustbuilder

# Create appuser
RUN adduser --no-create-home --disabled-password appuser

# Set workdir
WORKDIR /app

# Copy local dependencies
COPY . .

# Build the app using a dummy main in order to cache dependencies
RUN mv /app/mithril-aggregator /app/mithril-aggregator.1 \
    && mkdir -p /app/mithril-aggregator/src \
    && mkdir -p /app/mithril-aggregator/benches
COPY mithril-aggregator/Cargo.toml /app/mithril-aggregator/
COPY mithril-aggregator/benches/* /app/mithril-aggregator/benches/
RUN echo "fn  main () {}" > /app/mithril-aggregator/src/main.rs
RUN cargo build --release --bin mithril-aggregator --manifest-path /app/mithril-aggregator/Cargo.toml

# Rollback the rest of the files into the container
RUN rm -rf /app/mithril-aggregator && mv /app/mithril-aggregator.1 /app/mithril-aggregator
COPY ./mithril-aggregator/src/main.rs /app/mithril-aggregator/src/

# Build the binary
RUN cargo build --release --bin mithril-aggregator
RUN /app/target/release/mithril-aggregator --version

###############################
# STEP 2: build a small image
###############################
FROM debian:11-slim

# Args
ARG CARDANO_NODE_VERSION=8.9.0
ARG CARDANO_BIN_URL=https://github.com/input-output-hk/cardano-node/releases/download/$CARDANO_NODE_VERSION/cardano-node-$CARDANO_NODE_VERSION-linux.tar.gz
ARG EMBED-CARDANO-CLI=0

# Upgrade
RUN apt-get update -y && apt-get install -y libssl-dev ca-certificates wget sqlite3 && rm -rf /var/lib/apt/lists/*

# Import the user and group files from the builder
COPY --from=rustbuilder /etc/passwd /etc/passwd

# Copy the executable
COPY --from=rustbuilder /app/target/release/mithril-aggregator /app/bin/mithril-aggregator

# Copy the config files
COPY --from=rustbuilder /app/mithril-aggregator/config /app/config

# Install cardano-cli
RUN if [ "$EMBED-CARDANO-CLI" = 1 ] ; then \
    wget -nv -O cardano-bin.tar.gz $CARDANO_BIN_URL \
    && mkdir -p /app/bin \
    && (tar xzf cardano-bin.tar.gz ./bin/cardano-cli && mv /bin/cardano-cli /app/bin) || (tar xzf cardano-bin.tar.gz ./cardano-cli && mv cardano-cli /app/bin) \ 
    && /app/bin/cardano-cli --version \
    && rm -f cardano-bin.tar.gz \
    && chmod a+x /app/bin/cardano-cli; \
    fi

#Workdir
WORKDIR /app/
RUN chown -R appuser /app/

# Use an unprivileged user
USER appuser

# Run the executable
ENTRYPOINT ["/app/bin/mithril-aggregator","-vvv"]
