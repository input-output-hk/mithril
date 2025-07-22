#![warn(missing_docs)]
#![cfg_attr(docsrs, feature(doc_cfg))]

//! Shared datatypes and traits used by Mithril rust projects
//!
//! Provide:
//! - Helpers for the [Mithril STM](https://mithril.network/rust-doc/mithril_stm/index.html)
//!   lib with the [crypto_helper].
//! - [certificate chain][certificate_chain] used to validate the Certificate Chain created by an aggregator
//! - The [entities] used by, and exchanged between, the aggregator, signers and client.

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
pub mod crypto_helper;
pub mod entities;
pub mod logging;
pub mod messages;
pub mod protocol;
pub mod signable_builder;

cfg_test_tools! {
    pub mod test;
}

pub use entities::{CardanoNetwork, MagicId};

/// Generic error type
pub type StdError = anyhow::Error;

/// Generic result type
pub type StdResult<T> = anyhow::Result<T, StdError>;

/// Mithril API protocol version header name
pub const MITHRIL_API_VERSION_HEADER: &str = "mithril-api-version";

/// Mithril signer node version header name
pub const MITHRIL_SIGNER_VERSION_HEADER: &str = "signer-node-version";

/// Mithril aggregator node version header name
pub const MITHRIL_AGGREGATOR_VERSION_HEADER: &str = "aggregator-node-version";

/// Mithril origin of the request
pub const MITHRIL_ORIGIN_TAG_HEADER: &str = "mithril-origin-tag";

/// Mithril client type of the request
pub const MITHRIL_CLIENT_TYPE_HEADER: &str = "mithril-client-type";

/// Macro used to mark the code that should be cleaned up when the new era is activated
#[macro_export]
macro_rules! era_deprecate {
    ( $comment:literal ) => {};
}
