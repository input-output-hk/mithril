#![warn(missing_docs)]

//! Define everything necessary to list, download, and validate snapshots from a
//! [Mithril Aggregator](https://mithril.network/rust-doc/mithril_aggregator/index.html).
//!
//! To query an aggregator for snapshots & certificate use the [AggregatorHTTPClient] that implement
//! the [AggregatorHandler] trait.
//!
//! To list, download, and validate snapshots use the [Runtime].
//! You must initialize it by giving it a CertificateVerifier, a ProtocolGenesisVerifier and a [AggregatorHandler], and a
//! [Digester](mithril_common::digesters::ImmutableDigester)
//! implementations using the `with_xxx` methods.

pub mod aggregator_client;
pub mod commands;
mod entities;
mod message_adapters;
pub mod services;

pub use entities::*;
pub use message_adapters::{
    FromCertificateMessageAdapter, FromSnapshotListMessageAdapter, FromSnapshotMessageAdapter,
};
