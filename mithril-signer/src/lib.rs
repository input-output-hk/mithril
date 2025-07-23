#![warn(missing_docs)]
//! Mithril Signer crate documentation
//!
//! This crate is used by Cardano nodes to participate to Mithril signatures.
//! It proposes tools to communicate with Mithril aggregators and to issue Single Signatures.
//! See the [Mithril documentation](https://mithril.network/doc/manual/developer-docs/nodes/mithril-signer)
//! for more information on how it works.

mod commands;
mod configuration;
pub mod database;
pub mod dependency_injection;
pub mod entities;
mod message_adapters;
pub mod metrics;
mod runtime;
pub mod services;
pub mod store;

pub use commands::*;
pub use configuration::{Configuration, DefaultConfiguration};
pub use entities::SignerEpochSettings;
pub use message_adapters::{FromEpochSettingsAdapter, ToRegisterSignerMessageAdapter};
pub use metrics::*;
pub use runtime::*;

/// HTTP request timeout duration in milliseconds
const HTTP_REQUEST_TIMEOUT_DURATION: u64 = 30000;

/// SQLite file names
const SQLITE_FILE: &str = "signer.sqlite3";
const SQLITE_FILE_CARDANO_TRANSACTION: &str = "cardano-transaction.sqlite3";

// Memory allocator (to handle properly memory fragmentation)
#[cfg(all(not(target_env = "msvc"), feature = "jemallocator"))]
use tikv_jemallocator::Jemalloc;

#[cfg(all(not(target_env = "msvc"), feature = "jemallocator"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[cfg(test)]
pub(crate) mod test_tools {
    mithril_common::define_test_logger!();
}
