#![warn(missing_docs)]

//! Define everything necessary to manipulate mithril types from a
//! [Mithril Aggregator](https://mithril.network/rust-doc/mithril_aggregator/index.html).
//!
//! It handles the different types that can be queried to a mithril aggregator:
//!
//! - [Snapshot][snapshot_client] list, get and download tarball.
//! - [Mithril stake distribution][mithril_stake_distribution_client] list and get.
//! - [Certificates][certificate_client] list, get, and chain validation.
//!
//! The [Client][client::Client] aggregates the queries of all of those types.
//!
//! # Example
//!
//! Below is a example describing the use of the most of library's functions together:
//!
//! ```no_run
//! # use mithril_client::client::ClientBuilder;
//! # use mithril_client::message::MessageBuilder;
//! # use mithril_client::MithrilResult;
//! # use std::path::Path;
//! #
//! # #[tokio::main]
//! # async fn main() -> MithrilResult<()> {
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//!
//! let snapshots = client.snapshot().list().await?;
//!
//! let last_digest = snapshots.first().unwrap().digest.as_ref();
//! let snapshot = client.snapshot().get(last_digest).await?.unwrap();
//!
//! // note: the directoy must already exists
//! let target_directory = Path::new("/home/user/download/");
//! client
//!     .snapshot()
//!     .download_unpack(&snapshot, &target_directory)
//!     .await?;
//!
//! let certificate = client
//!     .certificate()
//!     .verify_chain(&snapshot.certificate_hash)
//!     .await?;
//!
//! let message = MessageBuilder::new()
//!     .compute_snapshot_message(&certificate, &target_directory)
//!     .await?;
//!
//! assert!(certificate.match_message(&message));
//! #    Ok(())
//! # }
//! ```

pub mod aggregator_client;
pub mod certificate_client;
pub mod client;
pub mod feedback;
pub mod message;
pub mod mithril_stake_distribution_client;
pub mod snapshot_client;
pub mod snapshot_downloader;
mod utils;

/// Mithril result type, an alias of [anyhow::Result]
pub type MithrilResult<T> = anyhow::Result<T>;

/// Mithril error type, an alias of [anyhow::Error]
pub type MithrilError = anyhow::Error;

/// A Mithril snapshot of a Cardano Node database.
pub type Snapshot = mithril_common::messages::SnapshotMessage;

/// List item of Mithril snapshots
///
/// A data structure dedicated to snapshots listing.
pub type SnapshotListItem = mithril_common::messages::SnapshotListItemMessage;

/// A Mithril stake distribution.
pub type MithrilStakeDistribution = mithril_common::messages::MithrilStakeDistributionMessage;

/// List item of Mithril stake distribution.
///
/// A data structure dedicated to mithril stake distributions listing.
pub type MithrilStakeDistributionListItem =
    mithril_common::messages::MithrilStakeDistributionListItemMessage;

/// A Mithril certificate.
pub type MithrilCertificate = mithril_common::entities::Certificate;

/// List item of certificates
///
/// A data structure dedicated to certificates listing.
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
