#![warn(missing_docs)]
#![cfg_attr(docsrs, feature(doc_cfg))]

//! Shared datatypes and traits used by Mithril rust projects
//!
//! Provide:
//! - [Digester][digesters] to compute mithril digest from a Cardano database
//! - Helpers for the [Mithril STM](https://mithril.network/rust-doc/mithril_stm/index.html)
//! lib with the [crypto_helper].
//! - [certificate chain][certificate_chain] used to validate the Certificate Chain created by an aggregator
//! - The [entities] used by, and exchanged between, the aggregator, signers and client.

macro_rules! cfg_fs {
    ($($item:item)*) => {
        $(
            #[cfg(feature = "fs")]
            #[cfg_attr(docsrs, doc(cfg(feature = "fs")))]
            $item
        )*
    }
}

macro_rules! cfg_random {
    ($($item:item)*) => {
        $(
            #[cfg(any(test, feature = "random"))]
            #[cfg_attr(docsrs, doc(cfg(feature = "random")))]
            $item
        )*
    }
}

macro_rules! cfg_fs_random {
    ($($item:item)*) => {
        $(
            #[cfg(all(feature = "fs", feature = "random"))]
            #[cfg_attr(docsrs, doc(all(feature = "fs", feature = "random")))]
            $item
        )*
    }
}

macro_rules! cfg_test_tools {
    ($($item:item)*) => {
        $(
            #[cfg(any(test, feature = "test_tools"))]
            #[cfg_attr(docsrs, doc(cfg(feature = "test_tools")))]
            $item
        )*
    }
}

pub mod api_version;
pub mod certificate_chain;
pub mod chain_observer;
pub mod chain_reader;
pub mod crypto_helper;
pub mod entities;
#[macro_use]
pub mod era;
pub mod messages;
pub mod protocol;
pub mod signable_builder;

cfg_test_tools! {
    pub mod test_utils;
}

cfg_fs! {
    mod time_point_provider;
    pub mod digesters;
    pub mod cardano_block_scanner;

    pub use time_point_provider::{TimePointProvider, TimePointProviderImpl};
}

pub use entities::{CardanoNetwork, MagicId};

/// Generic error type
pub type StdError = anyhow::Error;

/// Generic result type
pub type StdResult<T> = anyhow::Result<T, StdError>;

/// Mithril API protocol version header name
pub const MITHRIL_API_VERSION_HEADER: &str = "mithril-api-version";

/// Mithril Signer node version header name
pub const MITHRIL_SIGNER_VERSION_HEADER: &str = "signer-node-version";
