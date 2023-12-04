# Mithril client library example: Mithril stake distribution

## Description

This example shows how to implement a Mithril client and use the features related to the `Mithril stake distribution` type.

In this example, the client interacts with a real aggregator on the network `testing-preview` to:
- list the available Mithril stake distributions
- get a single Mithril stake distribution
- verify a certificate chain
- compute a message for a Mithril stake distribution
- verify that the certificate signs the computed message

The crates [`slog`](https://docs.rs/slog/latest/slog/) and [`slog_term`](https://docs.rs/slog-term/latest/slog_term/) are used to report the progress to the console.

## Build and run the example

```bash
# Build from the crate directory
cargo build

# Run from the crate directory
cargo run
```

## Links
- **Developer documentation**: https://docs.rs/mithril-client/latest/mithril_client/
- **Crates.io**: https://crates.io/crates/mithril-client