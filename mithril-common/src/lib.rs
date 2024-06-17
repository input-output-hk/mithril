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
pub mod crypto_helper;
pub mod entities;
#[macro_use]
pub mod era;
pub mod messages;
pub mod protocol;
pub mod resource_pool;
pub mod signable_builder;
pub mod signed_entity_type_lock;

cfg_test_tools! {
    pub mod test_utils;
}

cfg_fs! {
    mod ticker_service;
    pub mod digesters;
    pub mod cardano_block_scanner;
    pub mod chain_reader;
    pub mod cardano_transactions_preloader;

    pub use ticker_service::{TickerService, MithrilTickerService};
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

#[cfg(test)]
mod tests {
    #[cfg(feature = "apispec")]
    #[test]
    fn test_openapi_examples_conformity() {
        use crate::test_utils::apispec::APISpec;
        let api_spec = APISpec::from_file(&APISpec::get_default_spec_file());

        let errors: Vec<String> = api_spec.verify_examples();

        assert!(
            errors.is_empty(),
            "Errors in examples\n{}",
            errors.join("\n")
        );
    }
}
