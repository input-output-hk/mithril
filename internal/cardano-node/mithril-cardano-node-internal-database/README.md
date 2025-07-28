# Mithril-cardano-node-internal-database [![CI workflow](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml/badge.svg)](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml) [![crates.io](https://img.shields.io/crates/v/mithril-cardano-node-internal-database.svg)](https://crates.io/crates/mithril-cardano-node-internal-database) [![License](https://img.shields.io/badge/license-Apache%202.0-blue?style=flat-square)](https://github.com/input-output-hk/mithril/blob/main/LICENSE) [![Discord](https://img.shields.io/discord/500028886025895936.svg?logo=discord&style=flat-square)](https://discord.gg/5kaErDKDRq)

This crate provides components to read the files of a Cardano node internal database, such as immutable files
and ledger state snapshots, and compute digests from them.
It includes observers, digesters and various helpers for file system operations.
