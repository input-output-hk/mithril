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
FROM debian:buster-slim

# Upgrade
RUN apt-get update && apt-get install -y libssl1.1 ca-certificates && rm -rf /var/lib/apt/lists/*

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