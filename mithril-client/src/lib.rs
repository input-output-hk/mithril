#![warn(missing_docs)]
#![cfg_attr(docsrs, feature(doc_cfg))]
// TODO: Remove this allow once migration from deprecated AggregatorClient types is complete
#![allow(deprecated)]

//! Define all the tooling necessary to manipulate Mithril certified types from a
//! [Mithril Aggregator](https://mithril.network/rust-doc/mithril_aggregator/index.html).
//!
//! It handles the different types that can be queried to a Mithril aggregator:
//!
//! - [Cardano Database v1 (aka Snapshot)][snapshot_client]: list, get, download archive and record statistics.
//! - [Cardano Database v2][cardano_database_client] list, get, download archive and record statistics.
//! - [Cardano transactions][cardano_transaction_client] list & get snapshot, get proofs.
//! - [Cardano stake distribution][cardano_stake_distribution_client] list, get and get by epoch.
//! - [Mithril stake distribution][mithril_stake_distribution_client] list and get.
//! - [Certificates][certificate_client] list, get, and chain validation.
//! - [MithrilEraClient][era]: retrieve the current Mithril era.
//!
//! The [Client] aggregates the queries of all of those types.
//!
//! **NOTE:** Snapshot download and Certificate chain validation can take quite some time even with a fast
//! computer and network.
//! For those a feedback mechanism is available, more details on it in the [feedback] submodule.
//!
//! # Example
//!
//! Below is an example describing the usage of most of the library's functions together:
//!
//! **Note:** _Snapshot download and the compute snapshot message functions are available using crate feature_ **fs**.
//!
//! ```no_run
//! # #[cfg(feature = "fs")]
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::{ClientBuilder, MessageBuilder};
//! use mithril_client::cardano_database_client::{DownloadUnpackOptions, ImmutableFileRange};
//! use std::path::Path;
//!
//! let client =
//!     ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY")
//!         .build()?;
//! let cardano_database_snapshot = client
//!     .cardano_database_v2()
//!     .get("CARDANO_DATABASE_HASH")
//!     .await?
//!     .unwrap();
//!
//! let certificate = client
//!     .certificate()
//!     .verify_chain(&cardano_database_snapshot.certificate_hash)
//!     .await?;
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
//!
//! let verified_digests = client
//!     .cardano_database_v2()
//!     .download_and_verify_digests(&certificate, &cardano_database_snapshot)
//!     .await?;
//!
//! let full_restoration = immutable_file_range == ImmutableFileRange::Full;
//! let include_ancillary = download_unpack_options.include_ancillary;
//! let number_of_immutable_files_restored =
//!     immutable_file_range.length(cardano_database_snapshot.beacon.immutable_file_number);
//! if let Err(error) = client
//!     .cardano_database_v2()
//!     .add_statistics(
//!         full_restoration,
//!         include_ancillary,
//!         number_of_immutable_files_restored,
//!     )
//!     .await
//! {
//!     println!("Could not increment snapshot download statistics: {error:?}");
//! }
//!
//! let allow_missing_immutables_files = false;
//! let merkle_proof = client
//!     .cardano_database_v2()
//!     .verify_cardano_database(
//!         &certificate,
//!         &cardano_database_snapshot,
//!         &immutable_file_range,
//!         allow_missing_immutables_files,
//!         &target_directory,
//!         &verified_digests,
//!     )
//!     .await?;
//!
//! let message = MessageBuilder::new()
//!     .compute_cardano_database_message(&certificate, &merkle_proof)
//!     .await?;
//!
//! assert!(certificate.match_message(&message));
//! #    Ok(())
//! # }
//! ```
//!
//! ## Optional Features
//!
//! The following are a list of [Cargo features](https://doc.rust-lang.org/stable/cargo/reference/manifest.html#the-features-section) that can be
//! enabled or disabled:
//!
//! - **fs**: Enables file system related functionalities.
//! - **unstable**: Enables experimental or in-development `mithril-client` features that may change.
//! - **rug-backend** *(enabled by default)*: Enables usage of `rug` numerical backend in `mithril-stm` (dependency of `mithril-common`).
//! - **num-integer-backend**: Enables usage of `num-integer` numerical backend in `mithril-stm` (dependency of `mithril-common`).
//!
//! To allow fine tuning of the http queries, the following [Reqwest](https://docs.rs/reqwest/latest/reqwest/#optional-features) features are re-exported:
//! - **native-tls** *(enabled by default)*: Enables TLS functionality provided by `native-tls`.
//! - **native-tls-vendored**: Enables the `vendored` feature of `native-tls`.
//! - **native-tls-alpn**: Enables the `alpn` feature of `native-tls`.
//! - **rustls-tls**: Enables TLS functionality provided by `rustls`.
//!   Equivalent to `rustls-tls-webpki-roots`.
//! - **rustls-tls-manual-roots**: Enables TLS functionality provided by `rustls`,
//!   without setting any root certificates. Roots have to be specified manually.
//! - **rustls-tls-webpki-roots**: Enables TLS functionality provided by `rustls`,
//!   while using root certificates from the `webpki-roots` crate.
//! - **rustls-tls-native-roots**: Enables TLS functionality provided by `rustls`,
//!   while using root certificates from the `rustls-native-certs` crate.
//! - **enable-http-compression** *(enabled by default)*: Enables compressed traffic with `reqwest`.

macro_rules! cfg_fs {
    ($($item:item)*) => {
        $(
            #[cfg(feature = "fs")]
            #[cfg_attr(docsrs, doc(cfg(feature = "fs")))]
            $item
        )*
    }
}

#[allow(unused_macros)]
macro_rules! cfg_unstable {
    ($($item:item)*) => {
        $(
            #[cfg(feature = "unstable")]
            #[cfg_attr(docsrs, doc(cfg(feature = "unstable")))]
            $item
        )*
    }
}

#[allow(unused_macros)]
macro_rules! cfg_fs_unstable {
    ($($item:item)*) => {
        $(
            #[cfg(all(feature = "unstable", feature = "fs"))]
            #[cfg_attr(docsrs, doc(cfg(all(feature = "unstable", feature = "fs"))))]
            $item
        )*
    }
}

mod aggregator_client;
pub mod cardano_database_client;
pub mod cardano_stake_distribution_client;
pub mod cardano_transaction_client;
pub mod certificate_client;
mod client;
pub mod era;
pub mod feedback;
mod message;
pub mod mithril_stake_distribution_client;
pub mod snapshot_client;
cfg_fs! {
    pub mod file_downloader;
}

mod type_alias;
mod utils;

pub use client::*;
pub use message::*;
pub use type_alias::*;

#[cfg(test)]
pub(crate) mod test_utils {
    mithril_common::define_test_logger!();
}
