mod aggregator;
mod client;
mod infrastructure;
mod signer;

pub use aggregator::Aggregator;
pub use client::{Client, ClientCommand, SnapshotCommand};
pub use infrastructure::MithrilInfrastructure;
pub use signer::Signer;

pub const DEVNET_MAGIC_ID: mithril_common::MagicId = 42;
