// Per-circuit key newtypes for the recursive (IVC) circuit, and the circuit's implementation of
// [`CircuitKeyGenerator`]. The newtypes wrap the raw PLONK keys and delegate their byte
// (de)serialization to the impls in `key_serialization`; the raw keys stay internal to
// `circuits::halo2_ivc`, where Halo2's keygen APIs require them.
use midnight_curves::Bls12;
use midnight_proofs::plonk::{keygen_pk, keygen_vk_with_k};
use midnight_proofs::poly::commitment::Params;
use midnight_proofs::poly::kzg::params::ParamsKZG;
use serde::{Deserialize, Serialize};

use crate::StmResult;
use crate::circuits::circuit_key_generator::CircuitKeyGenerator;
use crate::codec::{TryFromBytes, TryToBytes};

use super::{
    PlonkProvingKey, PlonkVerifyingKey, RECURSIVE_CIRCUIT_DEGREE, circuit::IvcCircuitData,
};

/// Verifying key of the recursive (IVC) circuit.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct RecursiveCircuitVerifyingKey(
    #[serde(with = "recursive_verifying_key_serde")] PlonkVerifyingKey,
);

/// Proving key of the recursive (IVC) circuit.
pub(crate) struct RecursiveCircuitProvingKey(PlonkProvingKey);

impl RecursiveCircuitVerifyingKey {
    /// Wraps a raw recursive verifying key.
    pub(crate) fn new(verifying_key: PlonkVerifyingKey) -> Self {
        Self(verifying_key)
    }

    /// Borrows the wrapped raw verifying key, for the prover/verifier and fixed-base construction.
    pub(crate) fn verifying_key(&self) -> &PlonkVerifyingKey {
        &self.0
    }
}

impl RecursiveCircuitProvingKey {
    /// Wraps a raw recursive proving key.
    pub(crate) fn new(proving_key: PlonkProvingKey) -> Self {
        Self(proving_key)
    }

    /// Borrows the wrapped raw proving key, for proof generation.
    pub(crate) fn proving_key(&self) -> &PlonkProvingKey {
        &self.0
    }
}

impl TryToBytes for RecursiveCircuitVerifyingKey {
    fn to_bytes_vec(&self) -> StmResult<Vec<u8>> {
        self.0.to_bytes_vec()
    }
}

impl TryFromBytes for RecursiveCircuitVerifyingKey {
    fn try_from_bytes(bytes: &[u8]) -> StmResult<Self> {
        Ok(Self(PlonkVerifyingKey::try_from_bytes(bytes)?))
    }
}

impl TryToBytes for RecursiveCircuitProvingKey {
    fn to_bytes_vec(&self) -> StmResult<Vec<u8>> {
        self.0.to_bytes_vec()
    }
}

impl TryFromBytes for RecursiveCircuitProvingKey {
    fn try_from_bytes(bytes: &[u8]) -> StmResult<Self> {
        Ok(Self(PlonkProvingKey::try_from_bytes(bytes)?))
    }
}

/// Serde for the wrapped raw recursive verifying key: the raw-bytes encoding pinned to the IVC
/// circuit (via the key's byte codec), matching the embedded verifier-data wire format.
mod recursive_verifying_key_serde {
    use serde::{Deserializer, Serializer};

    use super::PlonkVerifyingKey;
    use crate::codec::{TryFromBytes, TryToBytes};

    pub(super) fn serialize<S: Serializer>(
        verifying_key: &PlonkVerifyingKey,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        let bytes = verifying_key.to_bytes_vec().map_err(serde::ser::Error::custom)?;
        serializer.serialize_bytes(&bytes)
    }

    pub(super) fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<PlonkVerifyingKey, D::Error> {
        let bytes: Vec<u8> = serde::Deserialize::deserialize(deserializer)?;
        PlonkVerifyingKey::try_from_bytes(&bytes).map_err(serde::de::Error::custom)
    }
}

impl CircuitKeyGenerator for IvcCircuitData {
    type VerifyingKey = RecursiveCircuitVerifyingKey;
    type ProvingKey = RecursiveCircuitProvingKey;

    fn generate_key_pair(
        &self,
        srs: &ParamsKZG<Bls12>,
    ) -> StmResult<(Self::VerifyingKey, Self::ProvingKey)> {
        // Keygen needs the SRS at the IVC circuit's degree. The SRS must be at least that large: when
        // it is exactly RECURSIVE_CIRCUIT_DEGREE it is used directly, and when it is larger it is
        // downsized on a clone so the caller's SRS (which may be reused at a different degree) is left
        // untouched.
        debug_assert!(
            srs.max_k() >= RECURSIVE_CIRCUIT_DEGREE,
            "the SRS must be at least the recursive circuit degree"
        );
        let verifying_key = if srs.max_k() == RECURSIVE_CIRCUIT_DEGREE {
            keygen_vk_with_k(srs, self, RECURSIVE_CIRCUIT_DEGREE)?
        } else {
            let mut recursive_srs = srs.clone();
            recursive_srs.downsize(RECURSIVE_CIRCUIT_DEGREE);
            keygen_vk_with_k(&recursive_srs, self, RECURSIVE_CIRCUIT_DEGREE)?
        };
        let proving_key = keygen_pk(verifying_key.clone(), self)?;
        Ok((
            RecursiveCircuitVerifyingKey(verifying_key),
            RecursiveCircuitProvingKey(proving_key),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::circuits::halo2_ivc::RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;

    #[test]
    fn recursive_verifying_key_newtype_round_trips_through_bytes() {
        let verifying_key = RecursiveCircuitVerifyingKey::try_from_bytes(
            RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        )
        .expect("production recursive verifying key bytes should deserialize");
        assert_eq!(
            verifying_key.to_bytes_vec().expect("serialize should succeed"),
            RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
            "the recursive verifying key newtype must round-trip to the embedded production bytes"
        );
    }
}
