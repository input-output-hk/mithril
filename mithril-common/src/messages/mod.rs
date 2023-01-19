//! Messages module
//! This module aims at providing shared structures for API communications.
mod epoch_settings;
mod register_signature;
mod register_signer;
mod snapshot;

pub use epoch_settings::EpochSettingsMessage;
pub use register_signature::RegisterSignatureMessage;
pub use register_signer::RegisterSignerMessage;
pub use snapshot::SnapshotMessage;
