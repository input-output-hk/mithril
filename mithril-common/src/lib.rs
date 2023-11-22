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

macro_rules! cfg_database {
    ($($item:item)*) => {
        $(
            #[cfg(feature = "database")]
            #[cfg_attr(docsrs, doc(cfg(feature = "database")))]
            $item
        )*
    }
}

macro_rules! cfg_fs {
    ($($item:item)*) => {
        $(
            #[cfg(feature = "fs")]
            #[cfg_attr(docsrs, doc(cfg(feature = "fs")))]
            $item
        )*
    }
}

pub mod api_version;
#[cfg(feature = "fs")]
mod beacon_provider;

pub mod certificate_chain;
pub mod chain_observer;
pub mod crypto_helper;
#[cfg(feature = "database")]
pub mod database;
#[cfg(feature = "fs")]
pub mod digesters;
pub mod entities;
#[macro_use]
pub mod era;
pub mod messages;
pub mod protocol;
pub mod signable_builder;

#[cfg(feature = "database")]
pub mod sqlite;
#[cfg(feature = "database")]
pub mod store;

pub mod test_utils;

#[cfg(feature = "fs")]
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
