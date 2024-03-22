#![warn(missing_docs)]
#![doc = include_str!("../README.md")]

mod commands;
/// Peer to peer module
pub mod p2p;
mod relay;

pub use commands::Args;
pub use commands::RelayCommands;
pub use relay::AggregatorRelay;
pub use relay::PassiveRelay;
pub use relay::SignerRelay;

/// The topic name where signer registrations are published
pub const MITHRIL_SIGNERS_TOPIC_NAME: &str = "mithril/signers";

/// The topic name where signatures are published
pub const MITHRIL_SIGNATURES_TOPIC_NAME: &str = "mithril/signatures";
