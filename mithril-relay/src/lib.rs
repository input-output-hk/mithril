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
    /// The topic name where HTTP signer registrations are published
    pub const SIGNERS_HTTP: &str = "mithril/signers/http";

    /// The topic name where HTTP signatures are published
    pub const SIGNATURES_HTTP: &str = "mithril/signatures/http";

    /// The topic name where DMQ signatures are published
    pub const SIGNATURES_DMQ: &str = "mithril/signatures/dmq";
}

#[cfg(test)]
pub(crate) mod test_tools {
    mithril_common::define_test_logger!();
}
