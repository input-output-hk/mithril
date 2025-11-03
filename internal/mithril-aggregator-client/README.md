# Mithril-aggregator-client [![CI workflow](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml/badge.svg)](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml) [![License](https://img.shields.io/badge/license-Apache%202.0-blue?style=flat-square)](https://github.com/input-output-hk/mithril/blob/main/LICENSE) [![Discord](https://img.shields.io/discord/500028886025895936.svg?logo=discord&style=flat-square)](https://discord.gg/5kaErDKDRq)

This crate provides a client to request data from a Mithril Aggregator over http.

## Configuration of http features

Reqwest is used as the backend for the http client. Its default features are disabled to let the user control
which [features to enable](https://docs.rs/reqwest/latest/reqwest/#optional-features).

To enable a reqwest feature, add the following to your `Cargo.toml`:

```toml
reqwest = { version = "x.yy", features = ["feature_a", "feature_b"] }
```

for example, if reqwest is a workspace dependency, and you want to enable the `default` feature and the compression features:

```toml
reqwest = { workspace = true, features = [
    "default",
    "gzip",
    "zstd",
    "deflate",
    "brotli"
] }
```

### Unused dependency warning

You should add the `reqwest` dependency even if you don't use it directly in your code.
If you are using [`cargo machete`](https://github.com/bnjbvr/cargo-machete) to track unused dependencies, it will raise a warning.

To avoid this warning, add the following to your `Cargo.toml`:

```toml
[package.metadata.cargo-machete]
# reqwest: for features configuration, indirect dependency via `mithril-aggregator-client`
ignored = ["reqwest"]
```
