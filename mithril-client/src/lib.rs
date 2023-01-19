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

mod aggregator;
pub mod commands;
mod entities;
mod message_adapters;
mod runtime;

pub use aggregator::{AggregatorHTTPClient, AggregatorHandler, AggregatorHandlerError};
pub use entities::Config;
pub use message_adapters::{FromCertificateMessageAdapter, FromSnapshotMessageAdapter};
pub use runtime::{Runtime, RuntimeError};

pub use runtime::convert_to_field_items;
