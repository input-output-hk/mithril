#![warn(missing_docs)]
#![cfg_attr(docsrs, feature(doc_cfg))]

//! Merkle tree and merkelized map primitives used by Mithril nodes.
//!
//! This crate provides:
//! - [`MKTree`]: a Merkle tree backed by a Merkle Mountain Range and a pluggable storer.
//! - [`MKProof`]: a proof of membership for a set of leaves in a Merkle tree.
//! - [`MKMap`]: a merkelized map whose keys and values are provable and can be nested (recursive proofs).
//! - [`MKMapProof`]: a proof of membership for a leaf in a merkelized map (including nested maps).

mod merkle_map;
mod merkle_tree;
pub mod test;

pub use merkle_map::{MKMap, MKMapKey, MKMapNode, MKMapProof, MKMapValue};
pub use merkle_tree::{
    Bytes, MKProof, MKTree, MKTreeLeafIndexer, MKTreeLeafPosition, MKTreeNode, MKTreeStoreInMemory,
    MKTreeStorer,
};

/// Generic error type re-exported for convenience.
pub type StdError = anyhow::Error;

/// Generic result type re-exported for convenience.
pub type StdResult<T> = anyhow::Result<T, StdError>;
