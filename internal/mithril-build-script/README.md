# Mithril-build-script [![CI workflow](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml/badge.svg)](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml) [![crates.io](https://img.shields.io/crates/v/mithril-build-script.svg)](https://crates.io/crates/mithril-build-script) [![License](https://img.shields.io/badge/license-Apache%202.0-blue?style=flat-square)](https://github.com/input-output-hk/mithril/blob/main/LICENSE) [![Discord](https://img.shields.io/discord/500028886025895936.svg?logo=discord&style=flat-square)](https://discord.gg/5kaErDKDRq)

A toolbox for Mithril crates that need a [build scripts phase](https://doc.rust-lang.org/cargo/reference/build-scripts.html).

Moreover, this crate allows to test the build scripts, which is not supported directly in `build.rs` (see: [rust-lang/cargo#1581](https://github.com/rust-lang/cargo/issues/1581)).
