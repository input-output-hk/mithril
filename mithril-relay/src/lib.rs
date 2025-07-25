#![warn(missing_docs)]
#![doc = include_str!("../README.md")]

mod commands;
/// Peer to peer module
pub mod p2p;
mod relay;
mod repeater;

pub use commands::Args;
pub use commands::RelayCommands;
pub use relay::AggregatorRelay;
pub use relay::PassiveRelay;
pub use relay::SignerRelay;
pub use relay::SignerRelayMode;

/// The P2P topic names used by Mithril
pub mod mithril_p2p_topic {
    /// The topic name where signer registrations are published
    pub const SIGNERS: &str = "mithril/signers";

    /// The topic name where signatures are published
    pub const SIGNATURES: &str = "mithril/signatures";
}

#[cfg(test)]
pub(crate) mod test_tools {
    mithril_common::define_test_logger!();
}
