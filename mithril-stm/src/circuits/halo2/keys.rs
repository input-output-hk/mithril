// Per-circuit key newtypes for the non-recursive (certificate) circuit, and the circuit's
// implementation of [`CircuitKeyGenerator`]. The newtypes wrap Midnight's self-describing
// `MidnightVK` / `MidnightPK` and delegate their byte (de)serialization to the impls in
// `key_serialization`.
use midnight_curves::Bls12;
use midnight_proofs::poly::kzg::params::ParamsKZG;
use midnight_zk_stdlib::{self as zk, MidnightPK, MidnightVK};
use serde::{Deserialize, Serialize};

use crate::circuits::circuit_key_generator::CircuitKeyGenerator;
use crate::circuits::circuit_verification_key_provider::CircuitVerificationKeyProvider;
use crate::codec::{TryFromBytes, TryToBytes};
use crate::{Parameters, StmResult};

use super::circuit::StmCertificateCircuit;

/// Verifying key of the non-recursive certificate circuit.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct NonRecursiveCircuitVerifyingKey(
    #[serde(with = "midnight_verifying_key_serde")] MidnightVK,
);

/// Proving key of the non-recursive certificate circuit.
pub(crate) struct NonRecursiveCircuitProvingKey(MidnightPK<StmCertificateCircuit>);

impl NonRecursiveCircuitVerifyingKey {
    /// Wraps a Midnight verifying key.
    pub(crate) fn new(midnight_vk: MidnightVK) -> Self {
        Self(midnight_vk)
    }

    /// Borrows the wrapped Midnight verifying key.
    pub(crate) fn midnight_vk(&self) -> &MidnightVK {
        &self.0
    }
}

/// Serde for the wrapped Midnight verifying key: the raw-bytes encoding, matching the embedded
/// SNARK-proof wire format.
mod midnight_verifying_key_serde {
    use midnight_proofs::utils::SerdeFormat;
    use midnight_zk_stdlib::MidnightVK;
    use serde::{Deserializer, Serializer};

    pub(super) fn serialize<S: Serializer>(
        verifying_key: &MidnightVK,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        let mut buffer = Vec::new();
        verifying_key
            .write(&mut buffer, SerdeFormat::RawBytes)
            .map_err(serde::ser::Error::custom)?;
        serializer.serialize_bytes(&buffer)
    }

    pub(super) fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<MidnightVK, D::Error> {
        let bytes: Vec<u8> = serde::Deserialize::deserialize(deserializer)?;
        MidnightVK::read(&mut bytes.as_slice(), SerdeFormat::RawBytes)
            .map_err(serde::de::Error::custom)
    }
}

impl NonRecursiveCircuitProvingKey {
    /// Borrows the wrapped Midnight proving key, for proof generation.
    pub(crate) fn midnight_pk(&self) -> &MidnightPK<StmCertificateCircuit> {
        &self.0
    }
}

impl CircuitVerificationKeyProvider<StmCertificateCircuit> {
    /// Production certificate-circuit provider: builds the circuit from `parameters`, roots the
    /// cache at the temporary directory, and validates against the embedded production verifying key.
    pub(crate) fn for_non_recursive_circuit(
        parameters: &Parameters,
        merkle_tree_depth: u32,
    ) -> StmResult<Self> {
        let circuit = StmCertificateCircuit::try_new(parameters, merkle_tree_depth)?;
        Ok(Self::new(
            std::env::temp_dir(),
            "non-recursive-keys",
            super::NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
            circuit,
        ))
    }
}

impl TryToBytes for NonRecursiveCircuitVerifyingKey {
    fn to_bytes_vec(&self) -> StmResult<Vec<u8>> {
        self.0.to_bytes_vec()
    }
}

impl TryFromBytes for NonRecursiveCircuitVerifyingKey {
    fn try_from_bytes(bytes: &[u8]) -> StmResult<Self> {
        Ok(Self(MidnightVK::try_from_bytes(bytes)?))
    }
}

impl TryToBytes for NonRecursiveCircuitProvingKey {
    fn to_bytes_vec(&self) -> StmResult<Vec<u8>> {
        self.0.to_bytes_vec()
    }
}

impl TryFromBytes for NonRecursiveCircuitProvingKey {
    fn try_from_bytes(bytes: &[u8]) -> StmResult<Self> {
        Ok(Self(MidnightPK::<StmCertificateCircuit>::try_from_bytes(
            bytes,
        )?))
    }
}

impl CircuitKeyGenerator for StmCertificateCircuit {
    type VerifyingKey = NonRecursiveCircuitVerifyingKey;
    type ProvingKey = NonRecursiveCircuitProvingKey;

    fn generate_key_pair(
        &self,
        srs: &ParamsKZG<Bls12>,
    ) -> StmResult<(Self::VerifyingKey, Self::ProvingKey)> {
        let mut certificate_srs = srs.clone();
        zk::downsize_srs_for_relation(&mut certificate_srs, self);
        let verifying_key = zk::setup_vk(&certificate_srs, self);
        let proving_key = zk::setup_pk(self, &verifying_key);
        Ok((
            NonRecursiveCircuitVerifyingKey(verifying_key),
            NonRecursiveCircuitProvingKey(proving_key),
        ))
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use midnight_proofs::poly::commitment::Params;
    use midnight_proofs::poly::kzg::params::ParamsKZG;
    use midnight_zk_stdlib::MidnightCircuit;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use super::{NonRecursiveCircuitProvingKey, NonRecursiveCircuitVerifyingKey};
    use crate::Parameters;
    use crate::circuits::circuit_key_generator::CircuitKeyGenerator;
    use crate::circuits::circuit_verification_key_provider::CircuitVerificationKeyProvider;
    use crate::circuits::halo2::circuit::StmCertificateCircuit;
    use crate::codec::{TryFromBytes, TryToBytes};

    #[test]
    fn generate_key_pair_downsizes_a_clone_and_round_trips() {
        let parameters = Parameters {
            k: 3,
            m: 10,
            phi_f: 0.2,
        };
        let merkle_tree_depth = 4;
        let circuit = StmCertificateCircuit::try_new(&parameters, merkle_tree_depth)
            .expect("certificate circuit should build");
        // Oversized on purpose: the generator must clone and downsize to the circuit's degree
        // (keygen would otherwise fail), and the caller's SRS must be left untouched.
        let oversized_degree = MidnightCircuit::from_relation(&circuit).min_k() + 1;
        let srs = ParamsKZG::unsafe_setup(oversized_degree, ChaCha20Rng::seed_from_u64(42));

        let (verifying_key, proving_key) = circuit
            .generate_key_pair(&srs)
            .expect("key generation should succeed");

        assert_eq!(
            srs.max_k(),
            oversized_degree,
            "the generator must downsize a clone, leaving the caller's SRS untouched"
        );

        let verifying_key_bytes = verifying_key.to_bytes_vec().expect("serialize should succeed");
        let restored_verifying_key =
            NonRecursiveCircuitVerifyingKey::try_from_bytes(&verifying_key_bytes)
                .expect("deserialize should succeed");
        assert_eq!(
            verifying_key_bytes,
            restored_verifying_key.to_bytes_vec().unwrap(),
            "verifying key bytes must be stable across a round trip"
        );

        let proving_key_bytes = proving_key.to_bytes_vec().expect("serialize should succeed");
        let restored_proving_key =
            NonRecursiveCircuitProvingKey::try_from_bytes(&proving_key_bytes)
                .expect("deserialize should succeed");
        assert_eq!(
            proving_key_bytes,
            restored_proving_key.to_bytes_vec().unwrap(),
            "proving key bytes must be stable across a round trip"
        );
    }

    #[test]
    fn provider_returns_cached_verifying_key_on_hit() {
        let parameters = Parameters {
            k: 3,
            m: 10,
            phi_f: 0.2,
        };
        let merkle_tree_depth = 4;
        let circuit = StmCertificateCircuit::try_new(&parameters, merkle_tree_depth)
            .expect("certificate circuit should build");
        let circuit_degree = MidnightCircuit::from_relation(&circuit).min_k();
        let srs = ParamsKZG::unsafe_setup(circuit_degree, ChaCha20Rng::seed_from_u64(42));

        // Derive the real key pair and use its verifying-key bytes as the cache golden, so a
        // pre-populated cache is a fresh hit rather than stale.
        let (verifying_key, proving_key) = circuit
            .generate_key_pair(&srs)
            .expect("key generation should succeed");
        let verifying_key_bytes = verifying_key.to_bytes_vec().unwrap();
        let proving_key_bytes = proving_key.to_bytes_vec().unwrap();

        let base_dir = std::env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let provider = CircuitVerificationKeyProvider::new(
            base_dir.clone(),
            "non-recursive",
            &verifying_key_bytes,
            circuit,
        );
        fs::create_dir_all(provider.verification_key_path().parent().unwrap()).unwrap();
        fs::write(provider.verification_key_path(), &verifying_key_bytes).unwrap();
        fs::write(provider.proving_key_path(), &proving_key_bytes).unwrap();

        let (hit_verifying_key, _) = provider.key_pair(&srs).expect("cache hit should succeed");

        assert_eq!(
            hit_verifying_key.to_bytes_vec().unwrap(),
            verifying_key_bytes,
            "the provider must return the real verifying key read from the populated cache"
        );
        fs::remove_dir_all(&base_dir).ok();
    }
}
