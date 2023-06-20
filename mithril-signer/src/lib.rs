#![warn(missing_docs)]
//! Mithril Signer crate documentation
//!
//! This crate is used by Cardano nodes to participate to Mithril signatures.
//! It proposes tools to communicate with Mithril aggregators and to issue Single Signatures.
//! See the [Mithril documentation](https://mithril.network/doc/manual/developer-docs/nodes/mithril-signer)
//! for more information on how it works.

mod aggregator_client;
mod configuration;
mod message_adapters;
mod protocol_initializer_store;
mod runtime;
mod single_signer;

#[cfg(test)]
pub use aggregator_client::dumb::DumbAggregatorClient;
pub use aggregator_client::*;
pub use configuration::{Configuration, DefaultConfiguration};
pub use message_adapters::{
    FromEpochSettingsAdapter, FromPendingCertificateMessageAdapter, ToRegisterSignerMessageAdapter,
};
pub use protocol_initializer_store::{ProtocolInitializerStore, ProtocolInitializerStorer};
pub use runtime::*;
pub use single_signer::*;
