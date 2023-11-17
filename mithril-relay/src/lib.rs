#![warn(missing_docs)]

//! Mithril relay modules

mod commands;
mod p2p;
mod relay;

pub use commands::RelayCommands;
pub use p2p::*;
pub use relay::AggregatorRelay;
pub use relay::PassiveRelay;
pub use relay::SignerRelay;

/// The topic name where signatures are published
pub const MITHRIL_SIGNATURES_TOPIC_NAME: &str = "mithril/signatures";
