use std::error::Error;

use mithril_common::crypto_helper::ProtocolAggregateVerificationKey;

/// Export AVK of the genesis stake distribution to a payload file
pub fn export_payload_to_sign(
    genesis_avk: ProtocolAggregateVerificationKey,
) -> Result<(), Box<dyn Error>> {
    todo!();
}

/// Import signature of the AVK of the genesis stake distribution fom a file
pub fn import_payload_signature() -> Result<(), Box<dyn Error>> {
    todo!();
}

/// Automatic bootstrap of the genesis certificate (test only)
pub fn bootstrap_test_genesis_certificate() -> Result<(), Box<dyn Error>> {
    todo!();
}

/// Create test genesis keys and save them to file (test only)
pub fn create_test_genesis_keys() -> Result<(), Box<dyn Error>> {
    todo!();
}

#[cfg(test)]
mod tests {
    use super::*;
}
