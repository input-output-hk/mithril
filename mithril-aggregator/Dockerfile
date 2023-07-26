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
RUN mv /app/mithril-aggregator /app/mithril-aggregator.1 && mkdir -p /app/mithril-aggregator/src
COPY mithril-aggregator/Cargo.toml /app/mithril-aggregator/
RUN echo "fn  main () {}" > /app/mithril-aggregator/src/main.rs
RUN cargo build --release -p mithril-aggregator --manifest-path /app/mithril-aggregator/Cargo.toml

# Rollback the rest of the files into the container
RUN rm -rf /app/mithril-aggregator && mv /app/mithril-aggregator.1 /app/mithril-aggregator
COPY ./mithril-aggregator/src/main.rs /app/mithril-aggregator/src/

# Build the binary
RUN cargo build --release -p mithril-aggregator
RUN /app/target/release/mithril-aggregator --version

###############################
# STEP 2: build a small image
###############################
FROM debian:11-slim

# Args
ARG CARDANO_BIN_URL=https://github.com/input-output-hk/cardano-node/releases/download/8.1.2/cardano-node-8.1.2-linux.tar.gz

# Upgrade
RUN apt-get update -y && apt-get install -y libssl-dev ca-certificates wget sqlite3 && rm -rf /var/lib/apt/lists/*

# Import the user and group files from the builder
COPY --from=rustbuilder /etc/passwd /etc/passwd

# Copy the executable
COPY --from=rustbuilder /app/target/release/mithril-aggregator /app/bin/mithril-aggregator

# Copy the config files
COPY --from=rustbuilder /app/mithril-aggregator/config /app/config

#Workdir
WORKDIR /app/
RUN chown -R appuser /app/

# Install cardano-cli
RUN wget -nv -O cardano-bin.tar.gz $CARDANO_BIN_URL
RUN tar xzf cardano-bin.tar.gz ./cardano-cli && mv cardano-cli /app/bin
RUN /app/bin/cardano-cli --version
RUN rm -f cardano-bin.tar.gz

# Use an unprivileged user
USER appuser

# Run the executable
ENTRYPOINT ["/app/bin/mithril-aggregator","-vvv"]
