###############################
# STEP 1: build rust executable
###############################
FROM rust:alpine AS rustbuilder

# Upgrade and install build base
RUN apk update && apk add build-base && apk add pkgconfig && apk add openssl-dev

# Create appuser
RUN adduser -D -g '' appuser

# Copy files
WORKDIR /app
COPY . .

# Build the binary
WORKDIR /app/mithril-network/mithril-aggregator/
RUN make build

###############################
# STEP 2: build a small image
###############################
FROM alpine:latest

# Upgrade
RUN apk update

# Import the user and group files from the builder
COPY --from=rustbuilder /etc/passwd /etc/passwd

# Copy the executable
COPY --from=rustbuilder /app/mithril-network/mithril-aggregator/target/release/mithril-aggregator /app/bin/mithril-aggregator

#Workdir
WORKDIR /app/

# Use an unprivileged user
USER appuser

# Run the executable
ENTRYPOINT ["/app/bin/mithril-aggregator","-vvv"]