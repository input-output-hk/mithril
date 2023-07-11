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
