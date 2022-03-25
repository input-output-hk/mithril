FROM golang:1.17.1-alpine3.13 as builder

ENV PATH=/root/.cargo/bin:/go/bin:/usr/local/go/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN apk --update --no-cache add \
    ca-certificates wget xz openssl \
    git bash alpine-sdk upx unzip libgcc nano musl-dev m4 \
    && update-ca-certificates

RUN curl https://sh.rustup.rs -sSf | sh -s -- -q -y  \
    && rustup target add x86_64-unknown-linux-musl

WORKDIR /app
COPY . .

WORKDIR /app/mithril-proto/mithril-node-poc/
RUN make mlib
RUN go build -a -installsuffix cgo -o ./mithril-node-poc cmd/node/main.go

# remove some debug-related stuff from the binary
RUN strip --strip-unneeded ./mithril-node-poc

# compress the binary
RUN upx ./mithril-node-poc

FROM alpine:3.13.5

ENV MITHRIL_PARTY_ID=1
ENV POSTGRES_DSN="host=postgres port=5432 user=postgres password=mysecretpassword dbname=postgres sslmode=disable"
ENV HTTP_LISTEN_ADDR="0.0.0.0:8000"

WORKDIR  /app

RUN apk --update add libgcc nano bash

COPY --from=builder /app/mithril-proto/mithril-node-poc/mithril-node-poc /usr/local/bin/mithril-node-poc
COPY --from=builder /app/mithril-proto/mithril-node-poc/configs /app/configs
COPY --from=builder /app/mithril-proto/mithril-node-poc/migrations /app/migrations

EXPOSE 8000

ENTRYPOINT ["mithril-node-poc"]
