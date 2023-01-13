###############################
# STEP 1: build rust executable
###############################
FROM ubuntu:22.04 AS rustbuilder

# Upgrade and install build base
RUN apt-get update && apt-get install -y libssl-dev curl wget build-essential pkg-config make m4

# Install Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo --version

# Create appuser
RUN adduser --no-create-home --disabled-password appuser

# Set workdir
WORKDIR /app

# Copy local dependencies
COPY mithril-stm/ /mithril-stm/
COPY Cargo.lock /mithril-stm/
COPY mithril-common/ /mithril-common/
COPY Cargo.lock /mithril-common/

# Build the app using a dummy main in order to cache dependencies
COPY mithril-aggregator/Cargo.toml /app
COPY Cargo.lock /app/
RUN mkdir -p /app/src/ && echo "fn  main () {}" > /app/src/main.rs
RUN cargo build --release --manifest-path /app/Cargo.toml

# Copy the rest of the files into the container
COPY ./mithril-aggregator /app/

# Build the binary
RUN cargo build --release

###############################
# STEP 2: build a small image
###############################
FROM ubuntu:22.04

# Upgrade
RUN apt-get update -y && apt-get install -y libssl-dev ca-certificates wget sqlite3 && rm -rf /var/lib/apt/lists/*

# Import the user and group files from the builder
COPY --from=rustbuilder /etc/passwd /etc/passwd

# Copy the executable
COPY --from=rustbuilder /app/target/release/mithril-aggregator /app/bin/mithril-aggregator

# Copy the config files
COPY --from=rustbuilder /app/config /app/config

#Workdir
WORKDIR /app/
RUN chown -R appuser /app/

# Install cardano-cli
RUN wget -nv -O cardano-bin.tar.gz https://update-cardano-mainnet.iohk.io/cardano-node-releases/cardano-node-1.35.4-linux.tar.gz
RUN tar xzf cardano-bin.tar.gz ./cardano-cli && mv cardano-cli /app/bin
RUN /app/bin/cardano-cli --version
RUN rm -f cardano-bin.tar.gz

# Use an unprivileged user
USER appuser

# Run the executable
ENTRYPOINT ["/app/bin/mithril-aggregator","-vvv"]
