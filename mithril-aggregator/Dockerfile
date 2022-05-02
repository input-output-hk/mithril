###############################
# STEP 1: build rust executable
###############################
FROM rust:1.60 AS rustbuilder

# Upgrade and install build base
RUN apt-get update && apt-get install -y libssl-dev

# Create appuser
RUN adduser --no-create-home --disabled-password appuser

# Copy files
WORKDIR /app
RUN echo "fn  main () {}" > /app/dummy.rs

# If this changed likely the Cargo.toml changed so lets trigger the
# recopying of it anyways
COPY mithril-aggregator/Cargo.lock /app/

COPY mithril-aggregator/Cargo.toml /app/

# We'll get to what this substitution is for but replace main.rs with
# lib.rs if this is a library
RUN sed -i 's/src\/lib.rs/dummy.rs/' Cargo.toml
RUN cargo build --release
# Now return the file back to normal
RUN sed -i 's/dummy.rs/src\/lib.rs/' Cargo.toml
# Copy the rest of the files into the container
COPY ./mithril-aggregator /app

# Build the binary
RUN make build

###############################
# STEP 2: build a small image
###############################
FROM debian:buster-slim

# Upgrade
RUN apt-get update && apt-get install -y libssl1.1 ca-certificates && rm -rf /var/lib/apt/lists/*

# Import the user and group files from the builder
COPY --from=rustbuilder /etc/passwd /etc/passwd

# Copy the executable
COPY --from=rustbuilder /app/target/release/mithril-aggregator /app/bin/mithril-aggregator

# Copy the config files
COPY --from=rustbuilder /app/config /app/config

#Workdir
WORKDIR /app/
RUN chown -R appuser /app/

# Use an unprivileged user
USER appuser

# Run the executable
ENTRYPOINT ["/app/bin/mithril-aggregator","-vvv"]
