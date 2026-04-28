# Mithril-merkle-tree [![CI workflow](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml/badge.svg)](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml) [![crates.io](https://img.shields.io/crates/v/mithril-merkle-tree.svg)](https://crates.io/crates/mithril-merkle-tree) [![License](https://img.shields.io/badge/license-Apache%202.0-blue?style=flat-square)](https://github.com/input-output-hk/mithril/blob/main/LICENSE)

This crate provides Merkle tree and merkelized map primitives used by Mithril nodes.

It exposes:

- `MKTree`: a Merkle tree backed by a Merkle Mountain Range and a pluggable storer.
- `MKProof`: a proof of membership for a set of leaves in a Merkle tree.
- `MKMap`: a merkelized map whose keys and values are provable and can be nested (recursive proofs).
- `MKMapProof`: a proof of membership for a leaf in a merkelized map (including nested maps).
