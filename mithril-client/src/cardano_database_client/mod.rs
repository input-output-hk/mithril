//! A client to retrieve Cardano databases data from an Aggregator.
//!
//! In order to do so it defines a [CardanoDatabaseClient] which exposes the following features:
//!  - [get][CardanoDatabaseClient::get]: get a Cardano database data from its hash
//!  - [list][CardanoDatabaseClient::list]: get the list of available Cardano database
//!  - [download_unpack][CardanoDatabaseClient::download_unpack]: download and unpack a Cardano database snapshot for a given immutable files range
//!  - [compute_merkle_proof][CardanoDatabaseClient::compute_merkle_proof]: compute a Merkle proof for a given Cardano database snapshot and a given immutable files range
//!
//! # Get a Cardano database
//!
//! To get a Cardano database using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_database = client.cardano_database_v2().get("CARDANO_DATABASE_HASH").await?.unwrap();
//!
//! println!(
//!     "Cardano database hash={}, merkle_root={}, immutable_file_number={:?}",
//!     cardano_database.hash,
//!     cardano_database.merkle_root,
//!     cardano_database.beacon.immutable_file_number
//! );
//! #    Ok(())
//! # }
//! ```
//!
//! # List available Cardano databases
//!
//! To list available Cardano databases using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_databases = client.cardano_database_v2().list().await?;
//!
//! for cardano_database in cardano_databases {
//!     println!("Cardano database hash={}, immutable_file_number={}", cardano_database.hash, cardano_database.beacon.immutable_file_number);
//! }
//! #    Ok(())
//! # }
//! ```
//!
//! # Download a Cardano database snapshot
//! **Note:** _Available on crate feature_ **fs** _only._
//!
//! To download a partial or a full Cardano database folder the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # #[cfg(feature = "fs")]
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::{ClientBuilder, cardano_database_client::{ImmutableFileRange, DownloadUnpackOptions}};
//! use std::path::Path;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_database_snapshot = client.cardano_database_v2().get("CARDANO_DATABASE_HASH").await?.unwrap();
//!
//! // Note: the directory must already exist, and the user running the binary must have read/write access to it.
//! let target_directory = Path::new("/home/user/download/");
//! let immutable_file_range = ImmutableFileRange::Range(3, 6);
//! let download_unpack_options = DownloadUnpackOptions {
//!     allow_override: true,
//!     include_ancillary: true,
//!     ..DownloadUnpackOptions::default()
//! };
//! client
//!     .cardano_database_v2()
//!     .download_unpack(
//!         &cardano_database_snapshot,
//!         &immutable_file_range,
//!         &target_directory,
//!         download_unpack_options,
//!     )
//!     .await?;
//! #
//! #    Ok(())
//! # }
//! ```
//! # Compute a Merkle proof for a Cardano database snapshot
//! **Note:** _Available on crate feature_ **fs** _only._
//!
//! To compute proof of membership of downloaded immutable files in a Cardano database folder the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # #[cfg(feature = "fs")]
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::{ClientBuilder, cardano_database_client::{ImmutableFileRange, DownloadUnpackOptions}};
//! use std::path::Path;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_database_snapshot = client.cardano_database_v2().get("CARDANO_DATABASE_HASH").await?.unwrap();
//! let certificate = client.certificate().verify_chain(&cardano_database_snapshot.certificate_hash).await?;
//!
//! // Note: the directory must already exist, and the user running the binary must have read/write access to it.
//! let target_directory = Path::new("/home/user/download/");
//! let immutable_file_range = ImmutableFileRange::Full;
//! let download_unpack_options = DownloadUnpackOptions {
//!     allow_override: true,
//!     include_ancillary: true,
//!     ..DownloadUnpackOptions::default()
//! };
//! client
//!     .cardano_database_v2()
//!     .download_unpack(
//!         &cardano_database_snapshot,
//!         &immutable_file_range,
//!         &target_directory,
//!         download_unpack_options,
//!     )
//!     .await?;
//!
//! let merkle_proof = client
//!     .cardano_database_v2()
//!     .compute_merkle_proof(&certificate, &cardano_database_snapshot, &immutable_file_range, &target_directory)
//!     .await?;
//! #
//! #    Ok(())
//! # }
//! ```
mod api;
mod fetch;
mod statistics;

#[cfg(test)]
pub(crate) use api::test_dependency_injector::CardanoDatabaseClientDependencyInjector;
pub use api::CardanoDatabaseClient;

cfg_fs! {
    mod immutable_file_range;
    mod download_unpack;
    mod proving;

    pub use download_unpack::DownloadUnpackOptions;
    pub use immutable_file_range::ImmutableFileRange;
}
