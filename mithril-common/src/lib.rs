#![warn(missing_docs)]

//! Shared datatypes and traits used by Mithril rust projects
//!
//! Provide:
//! - A way to store data with the [store] types
//! - [Digester][digesters] to compute mithril digest from a Cardano database
//! - Helpers for the [Mithril STM](https://mithril.network/rust-doc/mithril_stm/index.html)
//! lib with the [crypto_helper].
//! - A [certificate chain] used to validate the Certificate Chain created by an aggregator
//! - The [entities] used by, and exchanged between, the aggregator, signers and client.
//! - useful test utilities including stubs, [fake data][fake_data] builders, a tool validate
//! conformity to an open api specification ([apispec]).

pub mod apispec;
mod beacon_provider;
pub mod certificate_chain;
pub mod chain_observer;
pub mod crypto_helper;
pub mod database;
pub mod digesters;
pub mod entities;
pub mod fake_data;
pub mod sqlite;
pub mod store;

pub use beacon_provider::{BeaconProvider, BeaconProviderError, BeaconProviderImpl};
pub use entities::{CardanoNetwork, MagicId};

// TODO: Investigate as why signers can't sign until epoch 3 (in the e2e) when set to -1
/// The epoch offset used for signers stake distribution and verification keys retrieval.
pub const SIGNER_EPOCH_RETRIEVAL_OFFSET: i64 = -1;

/// The epoch offset used to retrieve the signers stake distribution and verification keys that's
/// currently being signed so it can be used in the next epoch.
pub const NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET: i64 = 0;

/// The epoch offset used for signers stake distribution and verification keys recording.
pub const SIGNER_EPOCH_RECORDING_OFFSET: i64 = 1;

/// Mithril API protocol version
/// this is the same as the one in openapi.yml file.
/// If you want to update this version to reflect changes in the protocol,
/// please also update the entry in the openapi.yml
pub const MITHRIL_API_VERSION: &str = "0.0.1";
