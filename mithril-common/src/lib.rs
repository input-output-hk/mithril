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

pub mod api_version;
mod beacon_provider;
pub mod certificate_chain;
pub mod chain_observer;
pub mod crypto_helper;
pub mod database;
pub mod digesters;
pub mod entities;
#[macro_use]
pub mod era;
pub mod messages;
pub mod protocol;
pub mod signable_builder;
pub mod sqlite;
pub mod store;
pub mod test_utils;

pub use beacon_provider::{BeaconProvider, BeaconProviderImpl};
pub use entities::{CardanoNetwork, MagicId};

/// Generic error type
pub type StdError = anyhow::Error;

/// Generic result type
pub type StdResult<T> = anyhow::Result<T, StdError>;

/// Mithril API protocol version header name
pub const MITHRIL_API_VERSION_HEADER: &str = "mithril-api-version";

/// Mithril Signer node version header name
pub const MITHRIL_SIGNER_VERSION_HEADER: &str = "signer-node-version";
