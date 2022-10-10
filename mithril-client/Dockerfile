###############################
# STEP 1: build rust executable
###############################
FROM ubuntu:latest AS rustbuilder

# Upgrade and install build base
RUN apt-get update && apt-get install -y libssl-dev curl wget build-essential pkg-config make m4

# Install Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo --version

# Create appuser
RUN adduser --no-create-home --disabled-password appuser

WORKDIR /app

# Copy local dependencies
COPY mithril-core/ /mithril-core/
COPY Cargo.lock /mithril-core/
COPY mithril-common/ /mithril-common/
COPY Cargo.lock /mithril-common/

# Build the app using a dummy main in order to cache dependencies
COPY mithril-client/Cargo.toml /app
COPY Cargo.lock /app/
RUN mkdir -p /app/src/ && echo "fn  main () {}" > /app/src/main.rs
RUN cargo build --release --manifest-path /app/Cargo.toml

# Copy the rest of the files into the container
COPY ./mithril-client /app/

# Build the binary
RUN cargo build --release

###############################
# STEP 2: build a small image
###############################
FROM ubuntu:latest

# Upgrade
RUN apt-get update -y && apt-get install -y libssl-dev ca-certificates wget && rm -rf /var/lib/apt/lists/*

# Import the user and group files from the builder
COPY --from=rustbuilder /etc/passwd /etc/passwd

# Copy the executable
COPY --from=rustbuilder /app/target/release/mithril-client /app/bin/mithril-client

# Copy the config files
COPY --from=rustbuilder /app/config /app/config

#Workdir
WORKDIR /app/

# Use an unprivileged user
USER appuser

# Run the executable
ENTRYPOINT ["/app/bin/mithril-client"]