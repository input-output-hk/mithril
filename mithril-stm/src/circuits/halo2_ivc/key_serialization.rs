//! [`CircuitKeySerialization`] impls for the recursive circuit's raw PLONK keys.
//!
//! The raw PLONK `read` is generic over the circuit type and takes its `Params`, so these impls
//! pin [`IvcCircuitData`] and its `()` params. Production keys use [`SerdeFormat::RawBytes`].
//!
//! Note: `PlonkVerifyingKey` / `PlonkProvingKey` are the shared raw PLONK key types, but only the
//! IVC circuit's keys are ever deserialized raw here (the certificate keys round-trip as the
//! high-level `MidnightVK` / `MidnightPK`), so pinning `IvcCircuitData` is correct.

use anyhow::Context;
use midnight_proofs::utils::SerdeFormat;

use crate::StmResult;
use crate::circuits::key_serialization::CircuitKeySerialization;

use super::{
    E, F, KZGCommitmentScheme, PlonkProvingKey, PlonkVerifyingKey, ProvingKey, VerifyingKey,
    circuit::IvcCircuitData,
};

/// Serde format used for the on-disk / in-cache production keys.
const KEY_SERDE_FORMAT: SerdeFormat = SerdeFormat::RawBytes;

// Recursive (IVC) circuit verifying key. The raw PLONK `read` is generic over the circuit and
// takes its `Params`, so it is pinned to `IvcCircuitData` with its `()` params below.
impl CircuitKeySerialization for PlonkVerifyingKey {
    fn serialize_key(&self) -> StmResult<Vec<u8>> {
        let mut bytes = Vec::new();
        self.write(&mut bytes, KEY_SERDE_FORMAT)
            .with_context(|| "Failed to serialize the recursive PLONK verifying key")?;
        Ok(bytes)
    }

    fn deserialize_key(bytes: &[u8]) -> StmResult<Self> {
        let mut reader = bytes;
        VerifyingKey::<F, KZGCommitmentScheme<E>>::read::<_, IvcCircuitData>(
            &mut reader,
            KEY_SERDE_FORMAT,
            (),
        )
        .with_context(|| "Failed to deserialize the recursive PLONK verifying key")
    }
}

// Recursive (IVC) circuit proving key.
impl CircuitKeySerialization for PlonkProvingKey {
    fn serialize_key(&self) -> StmResult<Vec<u8>> {
        let mut bytes = Vec::new();
        self.write(&mut bytes, KEY_SERDE_FORMAT)
            .with_context(|| "Failed to serialize the recursive PLONK proving key")?;
        Ok(bytes)
    }

    fn deserialize_key(bytes: &[u8]) -> StmResult<Self> {
        let mut reader = bytes;
        ProvingKey::<F, KZGCommitmentScheme<E>>::read::<_, IvcCircuitData>(
            &mut reader,
            KEY_SERDE_FORMAT,
            (),
        )
        .with_context(|| "Failed to deserialize the recursive PLONK proving key")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::circuits::halo2_ivc::RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;

    #[test]
    fn verifying_key_round_trips_through_bytes() {
        let verifying_key =
            PlonkVerifyingKey::deserialize_key(RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION)
                .expect("production recursive verifying key bytes should deserialize");
        let first = verifying_key.serialize_key().expect("serialize should succeed");
        let restored =
            PlonkVerifyingKey::deserialize_key(&first).expect("re-deserialize should succeed");
        let second = restored.serialize_key().expect("re-serialize should succeed");
        assert_eq!(
            first, second,
            "recursive verifying key bytes must be stable across a round trip"
        );
    }
}
