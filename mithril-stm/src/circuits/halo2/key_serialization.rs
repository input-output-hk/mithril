//! [`CircuitKeySerialization`] impls for the certificate circuit's keys — the self-describing
//! Midnight `MidnightVK` / `MidnightPK` wrappers. Production keys use [`SerdeFormat::RawBytes`].

use anyhow::Context;
use midnight_proofs::utils::SerdeFormat;
use midnight_zk_stdlib::{MidnightPK, MidnightVK};

use crate::StmResult;
use crate::circuits::key_serialization::CircuitKeySerialization;

use super::circuit::StmCertificateCircuit;

/// Serde format used for the on-disk / in-cache production keys.
const KEY_SERDE_FORMAT: SerdeFormat = SerdeFormat::RawBytes;

// Certificate circuit verifying key. `MidnightVK` is self-describing, so reading needs only the
// serde format (no circuit type), unlike the recursive raw PLONK keys.
impl CircuitKeySerialization for MidnightVK {
    fn serialize_key(&self) -> StmResult<Vec<u8>> {
        let mut bytes = Vec::new();
        self.write(&mut bytes, KEY_SERDE_FORMAT)
            .with_context(|| "Failed to serialize the certificate verifying key")?;
        Ok(bytes)
    }

    fn deserialize_key(bytes: &[u8]) -> StmResult<Self> {
        let mut reader = bytes;
        MidnightVK::read(&mut reader, KEY_SERDE_FORMAT)
            .with_context(|| "Failed to deserialize the certificate verifying key")
    }
}

// Certificate circuit proving key.
impl CircuitKeySerialization for MidnightPK<StmCertificateCircuit> {
    fn serialize_key(&self) -> StmResult<Vec<u8>> {
        let mut bytes = Vec::new();
        self.write(&mut bytes, KEY_SERDE_FORMAT)
            .with_context(|| "Failed to serialize the certificate proving key")?;
        Ok(bytes)
    }

    fn deserialize_key(bytes: &[u8]) -> StmResult<Self> {
        let mut reader = bytes;
        MidnightPK::<StmCertificateCircuit>::read(&mut reader, KEY_SERDE_FORMAT)
            .with_context(|| "Failed to deserialize the certificate proving key")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::circuits::halo2::NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;

    #[test]
    fn verifying_key_round_trips_through_bytes() {
        let verifying_key =
            MidnightVK::deserialize_key(NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION)
                .expect("production verifying key bytes should deserialize");
        let first = verifying_key.serialize_key().expect("serialize should succeed");
        let restored = MidnightVK::deserialize_key(&first).expect("re-deserialize should succeed");
        let second = restored.serialize_key().expect("re-serialize should succeed");
        assert_eq!(
            first, second,
            "certificate verifying key bytes must be stable across a round trip"
        );
    }
}
