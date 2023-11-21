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
RUN mv /app/mithril-relay /app/mithril-relay.1 && mkdir -p /app/mithril-relay/src
COPY mithril-relay/Cargo.toml /app/mithril-relay/
RUN echo "fn  main () {}" > /app/mithril-relay/src/main.rs
RUN cargo build --release -p mithril-relay --manifest-path /app/mithril-relay/Cargo.toml

# Rollback the rest of the files into the container
RUN rm -rf /app/mithril-relay && mv /app/mithril-relay.1 /app/mithril-relay
COPY ./mithril-relay/src/main.rs /app/mithril-relay/src/


# Build the binary
RUN cargo build --release -p mithril-relay
RUN /app/target/release/mithril-relay --version

###############################
# STEP 2: build a small image
###############################
FROM debian:11-slim

# Upgrade
RUN apt-get update -y && apt-get install -y libssl-dev ca-certificates wget sqlite3 && rm -rf /var/lib/apt/lists/*

# Import the user and group files from the builder
COPY --from=rustbuilder /etc/passwd /etc/passwd

# Copy the executable
COPY --from=rustbuilder /app/target/release/mithril-relay /app/bin/mithril-relay

# Copy the config files
COPY --from=rustbuilder /app/mithril-relay/config /app/config

#Workdir
WORKDIR /app/
RUN chown -R appuser /app/

# Use an unprivileged user
USER appuser

# Run the executable
ENTRYPOINT ["/app/bin/mithril-relay", "-vvv"]
