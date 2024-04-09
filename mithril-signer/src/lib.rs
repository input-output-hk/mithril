#![warn(missing_docs)]
//! Mithril Signer crate documentation
//!
//! This crate is used by Cardano nodes to participate to Mithril signatures.
//! It proposes tools to communicate with Mithril aggregators and to issue Single Signatures.
//! See the [Mithril documentation](https://mithril.network/doc/manual/developer-docs/nodes/mithril-signer)
//! for more information on how it works.

mod aggregator_client;
mod cardano_transactions_importer;
mod configuration;
pub mod database;
mod message_adapters;
pub mod metrics;
mod protocol_initializer_store;
mod runtime;
mod single_signer;

#[cfg(test)]
pub use aggregator_client::dumb::DumbAggregatorClient;
pub use aggregator_client::*;
pub use cardano_transactions_importer::*;
pub use configuration::{Configuration, DefaultConfiguration};
pub use message_adapters::{
    FromEpochSettingsAdapter, FromPendingCertificateMessageAdapter, ToRegisterSignerMessageAdapter,
};
pub use metrics::*;
pub use protocol_initializer_store::{ProtocolInitializerStore, ProtocolInitializerStorer};
pub use runtime::*;
pub use single_signer::*;

/// HTTP request timeout duration in milliseconds
const HTTP_REQUEST_TIMEOUT_DURATION: u64 = 30000;

/// SQLite file names
const SQLITE_FILE: &str = "signer.sqlite3";
const SQLITE_FILE_CARDANO_TRANSACTION: &str = "cardano-transaction.sqlite3";

#[cfg(test)]
pub mod test_tools {
    use slog::Drain;
    use std::sync::Arc;

    pub fn logger_for_tests() -> slog::Logger {
        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        slog::Logger::root(Arc::new(drain), slog::o!())
    }
}
