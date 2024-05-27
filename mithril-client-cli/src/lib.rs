#![warn(missing_docs)]

//! A command line interface that uses the [Mithril Client Library](https://mithril.network/doc/manual/developer-docs/nodes/mithril-client-library)
//! to manipulate Mithril certified types from a Mithril Aggregator:
//! * Cardano DB: List, Show, download and verify
//! * Mithril Stake Distribution: List, download and verify
//
//! You can find more information on how it works reading the [documentation website](https://mithril.network/doc/mithril/mithril-network/client).

pub mod commands;
mod configuration;
mod utils;

/// Error Clap
pub type ClapError = clap::error::Error;
