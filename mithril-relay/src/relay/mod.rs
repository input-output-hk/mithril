mod aggregator;
mod passive;
mod signer;

pub use aggregator::AggregatorRelay;
pub use passive::PassiveRelay;
pub use signer::{SignerRelay, SignerRelayConfiguration, SignerRelayMode};

/// Shared maximum content length for payloads of 256kb
const MAX_CONTENT_LENGTH: u64 = 1024 * 256;
