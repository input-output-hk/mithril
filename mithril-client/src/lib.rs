#![warn(missing_docs)]

//! Define everything necessary to list, download, and validate snapshots from a
//! [Mithril Aggregator](https://mithril.network/mithril-aggregator/doc/mithril_aggregator/index.html).
//!
//! To query an aggregator for snapshots & certificate use the [AggregatorHTTPClient] that implement
//! the [AggregatorHandler] trait.
//!
//! To verify a multi-signature use the [VerifierImpl] that implement the [Verifier] trait.
//!
//! To list, download, and validate snapshots use the [Runtime].
//! You must initialize it by giving it a [Verifier], a [AggregatorHandler], and a
//! [Digester](https://mithril.network/mithril-common/doc/mithril_common/digesters/trait.Digester.html)
//! implementations using the `with_xxx` methods.

mod aggregator;
mod entities;
mod runtime;

pub use crate::aggregator::{AggregatorHTTPClient, AggregatorHandler, AggregatorHandlerError};
pub use crate::entities::Config;
pub use crate::runtime::{Runtime, RuntimeError};
