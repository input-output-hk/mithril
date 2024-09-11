//! Protocol module
//!
//! This module contains types that standardize and make easier mithril protocol operations
//! such as issuing single signatures, aggregating them as multi-signatures or computing
//! aggregate verification keys.

mod multi_signer;
mod signer_builder;
mod single_signer;

pub use multi_signer::MultiSigner;
pub use signer_builder::{SignerBuilder, SignerBuilderError};
pub use single_signer::SingleSigner;

/// Trait to convert a type to a message that can be signed or verified by the Mithril protocol.
pub trait AsMessage: Sync + Send {
    /// Return a String representation of the message.
    fn message_string(&self) -> String;
}

impl AsMessage for String {
    fn message_string(&self) -> String {
        self.clone()
    }
}

impl AsMessage for &str {
    fn message_string(&self) -> String {
        self.to_string()
    }
}
