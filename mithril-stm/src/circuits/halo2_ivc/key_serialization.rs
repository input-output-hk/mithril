//! [`TryToBytes`] / [`TryFromBytes`] impls for the recursive circuit's raw PLONK keys.
//!
//! The raw PLONK `read` is generic over the circuit type and takes its `Params`, so these impls
//! pin [`IvcCircuitData`] and its `()` params. Production keys use [`SerdeFormat::RawBytes`].
//!
//! Note: only the IVC circuit's keys are ever deserialized raw here (the certificate keys
//! round-trip as the high-level `MidnightVK` / `MidnightPK`), so pinning `IvcCircuitData` is
//! correct.

use anyhow::Context;
use midnight_proofs::utils::SerdeFormat;

use crate::StmResult;
use crate::codec::{TryFromBytes, TryToBytes};

use super::{E, F, KZGCommitmentScheme, ProvingKey, VerifyingKey, circuit::IvcCircuitData};

/// Serde format used for the on-disk / in-cache production keys.
const KEY_SERDE_FORMAT: SerdeFormat = SerdeFormat::RawBytes;

// Recursive (IVC) circuit verifying key. The raw PLONK `read` is generic over the circuit and
// takes its `Params`, so it is pinned to `IvcCircuitData` with its `()` params below.
impl TryToBytes for VerifyingKey<F, KZGCommitmentScheme<E>> {
    fn to_bytes_vec(&self) -> StmResult<Vec<u8>> {
        let mut bytes = Vec::new();
        self.write(&mut bytes, KEY_SERDE_FORMAT)
            .with_context(|| "Failed to serialize the recursive PLONK verifying key")?;
        Ok(bytes)
    }
}

impl TryFromBytes for VerifyingKey<F, KZGCommitmentScheme<E>> {
    fn try_from_bytes(bytes: &[u8]) -> StmResult<Self> {
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
impl TryToBytes for ProvingKey<F, KZGCommitmentScheme<E>> {
    fn to_bytes_vec(&self) -> StmResult<Vec<u8>> {
        let mut bytes = Vec::new();
        self.write(&mut bytes, KEY_SERDE_FORMAT)
            .with_context(|| "Failed to serialize the recursive PLONK proving key")?;
        Ok(bytes)
    }
}

impl TryFromBytes for ProvingKey<F, KZGCommitmentScheme<E>> {
    fn try_from_bytes(bytes: &[u8]) -> StmResult<Self> {
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
        let verifying_key = VerifyingKey::<F, KZGCommitmentScheme<E>>::try_from_bytes(
            RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        )
        .expect("production recursive verifying key bytes should deserialize");
        let first = verifying_key.to_bytes_vec().expect("serialize should succeed");
        let restored = VerifyingKey::<F, KZGCommitmentScheme<E>>::try_from_bytes(&first)
            .expect("re-deserialize should succeed");
        let second = restored.to_bytes_vec().expect("re-serialize should succeed");
        assert_eq!(
            first, second,
            "recursive verifying key bytes must be stable across a round trip"
        );
    }

    #[test]
    fn production_verifying_key_serializes_to_the_embedded_bytes() {
        let verifying_key = VerifyingKey::<F, KZGCommitmentScheme<E>>::try_from_bytes(
            RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        )
        .expect("production recursive verifying key bytes should deserialize");
        assert_eq!(
            verifying_key.to_bytes_vec().expect("serialize should succeed"),
            RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
            "the embedded production recursive verifying key must be its own canonical serialization"
        );
    }
}
