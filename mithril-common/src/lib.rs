pub mod apispec;
pub mod chain_observer;
pub mod crypto_helper;
pub mod digesters;
pub mod entities;
pub mod fake_data;
pub mod store;

pub use entities::{CardanoNetwork, MagicId};

/// The epoch offset used for signers stake distribution and verification keys retrieval
/// TODO: Investigate as why signers can't sign until epoch 3 (in the e2e) when set to -1
pub const SIGNER_EPOCH_RETRIEVAL_OFFSET: i64 = -1;
/// The epoch offset used for signers stake distribution and verification keys recording
pub const SIGNER_EPOCH_RECORDING_OFFSET: i64 = 1;
