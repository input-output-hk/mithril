#![warn(missing_docs)]

//! A command line interface that uses the [Mithril Client Library](https://mithril.network/doc/manual/developer-docs/nodes/mithril-client-library)
//! to manipulate Mithril certified types from a Mithril Aggregator:
//! - Cardano Database v1 (aka Snapshot): list, show, download archive
//! - Cardano Database v2: list, show, download archive
//! - Cardano transactions: list & show snapshot, certify a list of transactions
//! - Cardano stake distribution: list and download
//! - Mithril stake distribution: list and download
//!
//!   You can find more information on how it works reading the [documentation website](https://mithril.network/doc/mithril/mithril-network/client).

mod command_context;
pub mod commands;
mod configuration;
mod utils;

pub use command_context::*;
pub use configuration::*;
/// Error Clap
pub type ClapError = clap::error::Error;
