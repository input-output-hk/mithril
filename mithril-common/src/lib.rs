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

#[cfg(any(test, feature = "test_only"))]
pub mod apispec;
mod beacon_provider;
pub mod certificate_chain;
pub mod chain_observer;
pub mod crypto_helper;
pub mod database;
pub mod digesters;
pub mod entities;
#[cfg(any(test, feature = "test_only"))]
pub mod fake_data;
pub mod sqlite;
pub mod store;
pub mod test_utils;

pub use beacon_provider::{BeaconProvider, BeaconProviderError, BeaconProviderImpl};
pub use entities::{CardanoNetwork, MagicId};

/// Mithril API protocol version
/// this is the same as the one in openapi.yml file.
/// If you want to update this version to reflect changes in the protocol,
/// please also update the entry in the openapi.yml
pub const MITHRIL_API_VERSION: &str = "0.1.0";
