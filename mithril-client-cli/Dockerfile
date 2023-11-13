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
RUN mv /app/mithril-client-cli /app/mithril-client-cli.1 && mkdir -p /app/mithril-client-cli/src
COPY mithril-client-cli/Cargo.toml /app/mithril-client-cli/
RUN echo "fn  main () {}" > /app/mithril-client-cli/src/main.rs
RUN touch /app/mithril-client-cli/src/lib.rs
RUN cargo build --release --bin mithril-client --manifest-path /app/mithril-client-cli/Cargo.toml

# Rollback the rest of the files into the container
RUN rm -rf /app/mithril-client-cli && mv /app/mithril-client-cli.1 /app/mithril-client-cli
COPY ./mithril-client-cli/src/main.rs /app/mithril-client-cli/src/

# Build the binary
RUN cargo build --release --bin mithril-client
RUN /app/target/release/mithril-client --version

###############################
# STEP 2: build a small image
###############################
FROM debian:11-slim

# Upgrade
RUN apt-get update -y && apt-get install -y libssl-dev ca-certificates wget sqlite3 && rm -rf /var/lib/apt/lists/*

# Import the user and group files from the builder
COPY --from=rustbuilder /etc/passwd /etc/passwd

# Copy the executable
COPY --from=rustbuilder /app/target/release/mithril-client /app/bin/mithril-client

# Copy the config files
COPY --from=rustbuilder /app/mithril-client-cli/config /app/config

#Workdir
WORKDIR /app/
RUN chown -R appuser /app/

# Use an unprivileged user
USER appuser

# Run the executable
ENTRYPOINT ["/app/bin/mithril-client", "-vvv"]
