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
//! - [client::Client::show_snapshot]
//! ```no_run
//! use mithril_client::client::Client;
//! use mithril_client::common::StdResult;
//!
//! #[tokio::main]
//! async fn main() -> StdResult<()> {
//!     let client = Client::new("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").await?;
//!     let snapshot = client.show_snapshot("5a1288f7164bec049f34e46002e939f4c609a0ddf86636fdc4180ea22342cab7").await?;
//!
//!     println!("Snapshot id={}, size={}", snapshot.digest, snapshot.size);
//!
//!     Ok(())
//! }
//! ```
//!
//! - [client::Client::list_snapshots]
//! ```no_run
//! use mithril_client::client::Client;
//! use mithril_client::common::StdResult;
//!
//! #[tokio::main]
//! async fn main() -> StdResult<()> {
//!     let client = Client::new("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").await?;
//!     let snapshots = client.list_snapshots().await?;
//!
//!     for snapshot in snapshots {
//!         println!("Snapshot id={}, size={}", snapshot.digest, snapshot.size);
//!     }
//!
//!     Ok(())
//! }
//! ```
//!
//! - [client::Client::list_mithril_stake_distributions]
//! ```no_run
//! use mithril_client::client::Client;
//! use mithril_client::common::StdResult;
//!
//! #[tokio::main]
//! async fn main() -> StdResult<()> {
//!     let client = Client::new("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").await?;
//!     let stake_distributions = client.list_mithril_stake_distributions().await?;
//!
//!     for stake_distribution in stake_distributions {
//!         println!("Mithril Stake Distribution hash={}", stake_distribution.hash);
//!     }
//!
//!     Ok(())
//! }
//! ```

pub mod aggregator_client;
pub mod client;
pub mod dependencies;
mod message_adapters;
pub mod services;
pub mod utils;

pub use message_adapters::{FromCertificateMessageAdapter, FromSnapshotMessageAdapter};

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
        StdError, StdResult,
    };
}
