#![warn(missing_docs)]

//! Define everything necessary to list, download, and validate snapshots from a
//! [Mithril Aggregator](https://mithril.network/rust-doc/mithril_aggregator/index.html).
//!
//! To query an aggregator for snapshots & certificate use the [services::SnapshotService].
//!
//! Currently, [client::Client] exposes the following features:
//!  - [client::Client::show_snapshot]: call the snapshot service to get a snapshot message from a digest
//!  - [client::Client::list_snapshots]: call the snapshot service to get the list of available snapshots
//!  - [client::Client::list_mithril_stake_distributions]: call the mithril stake distribution service for the list of available mithril stake distributions
//!
//! Below are some examples describing the use of the library's functions from your own project:
//!
//! todo: redo examples, look at the previous lib for inspiration

pub mod aggregator_client;
// mod message_adapters;
// pub mod services;
mod certificate_client;
pub mod client;
mod message_adapters;
mod mithril_stake_distribution_client;
mod snapshot_client;
mod utils;

// pub use message_adapters::{FromCertificateMessageAdapter, FromSnapshotMessageAdapter};
pub type MithrilResult<T> = anyhow::Result<T>;
pub type MithrilError = anyhow::Error;

pub type Snapshot = mithril_common::messages::SnapshotMessage;
pub type SnapshotListItem = mithril_common::messages::SnapshotListItemMessage;

pub type MithrilStakeDistribution = mithril_common::messages::MithrilStakeDistributionMessage;
pub type MithrilStakeDistributionListItem =
    mithril_common::messages::MithrilStakeDistributionListItemMessage;

pub type MithrilCertificate = mithril_common::entities::Certificate;
pub type MithrilCertificateListItem = mithril_common::messages::CertificateListItemMessage;

/// `mithril-common` re-exports
pub mod common {
    pub use mithril_common::{
        certificate_chain::CertificateVerifier,
        crypto_helper::{ProtocolGenesisVerificationKey, ProtocolGenesisVerifier},
        entities::{Beacon, CompressionAlgorithm, Epoch},
        messages::{
            MithrilStakeDistributionListMessage, SnapshotListItemMessage, SnapshotListMessage,
            SnapshotMessage,
        },
    };
}

#[cfg(test)]
pub(crate) mod test_utils {
    use slog::Drain;
    use std::sync::Arc;

    pub fn test_logger() -> slog::Logger {
        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        slog::Logger::root(Arc::new(drain), slog::o!())
    }
}
