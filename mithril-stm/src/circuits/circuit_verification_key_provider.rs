// Compute-on-miss verification key provider: wraps the on-disk [`CircuitKeyCache`] with a
// [`CircuitKeyGenerator`] circuit, returning the cached key pair when fresh and otherwise deriving
// it from the SRS and storing it.
use std::path::PathBuf;

use midnight_curves::Bls12;
use midnight_proofs::poly::kzg::params::ParamsKZG;

use crate::StmResult;
use crate::codec::TryToBytes;

use super::circuit_key_generator::CircuitKeyGenerator;
use super::key_cache::CircuitKeyCache;

/// Provides a circuit's verifying and proving keys, backed by an on-disk [`CircuitKeyCache`] and
/// computing them from the SRS on a miss.
#[allow(dead_code)] // consumed by the certificate and recursive setups in the following steps
pub(crate) struct CircuitVerificationKeyProvider<C: CircuitKeyGenerator> {
    cache: CircuitKeyCache,
    circuit: C,
}

#[allow(dead_code)] // consumed by the certificate and recursive setups in the following steps
impl<C: CircuitKeyGenerator> CircuitVerificationKeyProvider<C> {
    /// Builds a provider rooted at `base_dir`. The cached verifying key is compared against
    /// `expected_verification_key_bytes`; empty bytes skip the comparison and trust the cache
    /// directory (used by content-keyed test caches). Keys are computed from `circuit` on a miss.
    pub(crate) fn new(
        base_dir: PathBuf,
        circuit_name: &str,
        expected_verification_key_bytes: &[u8],
        circuit: C,
    ) -> Self {
        Self {
            cache: CircuitKeyCache::new(base_dir, circuit_name, expected_verification_key_bytes),
            circuit,
        }
    }

    /// The circuit this provider derives keys for.
    pub(crate) fn circuit(&self) -> &C {
        &self.circuit
    }

    /// Returns the verifying key, computing and caching the key pair from `srs` on a miss.
    pub(crate) fn verification_key(&self, srs: &ParamsKZG<Bls12>) -> StmResult<C::VerifyingKey> {
        if let Some(verification_key) = self.cache.get_verification_key::<C::VerifyingKey>()? {
            return Ok(verification_key);
        }
        let (verification_key, _proving_key) = self.compute_and_store(srs)?;
        Ok(verification_key)
    }

    /// Returns the verifying and proving key pair, computing and caching it from `srs` on a miss.
    pub(crate) fn key_pair(
        &self,
        srs: &ParamsKZG<Bls12>,
    ) -> StmResult<(C::VerifyingKey, C::ProvingKey)> {
        // The proving key is read only once the verifying key is present, so an orphan proving key
        // left by an interrupted store is recomputed instead of surfacing a deserialization error.
        if let Some(verification_key) = self.cache.get_verification_key::<C::VerifyingKey>()?
            && let Some(proving_key) = self.cache.get_proving_key::<C::ProvingKey>()?
        {
            return Ok((verification_key, proving_key));
        }
        self.compute_and_store(srs)
    }

    /// Derives the key pair from `srs` via the circuit and stores it in the cache.
    fn compute_and_store(
        &self,
        srs: &ParamsKZG<Bls12>,
    ) -> StmResult<(C::VerifyingKey, C::ProvingKey)> {
        let (verification_key, proving_key) = self.circuit.generate_key_pair(srs)?;
        self.cache.store_key_pair(
            &verification_key.to_bytes_vec()?,
            &proving_key.to_bytes_vec()?,
        )?;
        Ok((verification_key, proving_key))
    }
}

#[cfg(test)]
impl<C: CircuitKeyGenerator> CircuitVerificationKeyProvider<C> {
    fn verification_key_path(&self) -> &std::path::Path {
        self.cache.verification_key_path()
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;
    use std::{env, fs};

    use midnight_curves::Bls12;
    use midnight_proofs::poly::kzg::params::ParamsKZG;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use super::{CircuitKeyGenerator, CircuitVerificationKeyProvider};
    use crate::StmResult;
    use crate::codec::{TryFromBytes, TryToBytes};

    /// Key backed by raw bytes, so the provider mechanics can be tested without real keygen.
    #[derive(Clone, Debug, PartialEq)]
    struct ByteKey(Vec<u8>);

    impl TryToBytes for ByteKey {
        fn to_bytes_vec(&self) -> StmResult<Vec<u8>> {
            Ok(self.0.clone())
        }
    }

    impl TryFromBytes for ByteKey {
        fn try_from_bytes(bytes: &[u8]) -> StmResult<Self> {
            Ok(Self(bytes.to_vec()))
        }
    }

    /// Generator returning fixed key bytes and counting how often it is invoked.
    struct CountingGenerator {
        verification_key: Vec<u8>,
        proving_key: Vec<u8>,
        calls: Cell<u32>,
    }

    impl CountingGenerator {
        fn new(verification_key: &[u8], proving_key: &[u8]) -> Self {
            Self {
                verification_key: verification_key.to_vec(),
                proving_key: proving_key.to_vec(),
                calls: Cell::new(0),
            }
        }
    }

    impl CircuitKeyGenerator for CountingGenerator {
        type VerifyingKey = ByteKey;
        type ProvingKey = ByteKey;

        fn generate_key_pair(&self, _srs: &ParamsKZG<Bls12>) -> StmResult<(ByteKey, ByteKey)> {
            self.calls.set(self.calls.get() + 1);
            Ok((
                ByteKey(self.verification_key.clone()),
                ByteKey(self.proving_key.clone()),
            ))
        }
    }

    // The generator ignores the SRS, so the smallest constructible parameters are enough.
    fn negligible_srs() -> ParamsKZG<Bls12> {
        ParamsKZG::unsafe_setup(1, ChaCha20Rng::seed_from_u64(0))
    }

    #[test]
    fn cold_miss_generates_stores_and_returns_the_pair() {
        let base_dir = env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let provider = CircuitVerificationKeyProvider::new(
            base_dir.clone(),
            "counting",
            b"vk",
            CountingGenerator::new(b"vk", b"pk"),
        );

        let (verification_key, proving_key) = provider.key_pair(&negligible_srs()).unwrap();

        assert_eq!(verification_key, ByteKey(b"vk".to_vec()));
        assert_eq!(proving_key, ByteKey(b"pk".to_vec()));
        assert_eq!(
            provider.circuit().calls.get(),
            1,
            "a cold miss must generate exactly once"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn warm_hit_reads_from_cache_without_regenerating() {
        let base_dir = env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let provider = CircuitVerificationKeyProvider::new(
            base_dir.clone(),
            "counting",
            b"vk",
            CountingGenerator::new(b"vk", b"pk"),
        );

        provider.key_pair(&negligible_srs()).unwrap();
        let (verification_key, proving_key) = provider.key_pair(&negligible_srs()).unwrap();

        assert_eq!(verification_key, ByteKey(b"vk".to_vec()));
        assert_eq!(proving_key, ByteKey(b"pk".to_vec()));
        assert_eq!(
            provider.circuit().calls.get(),
            1,
            "the second call must hit the cache"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn missing_verifying_key_recomputes_the_pair() {
        let base_dir = env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let provider = CircuitVerificationKeyProvider::new(
            base_dir.clone(),
            "counting",
            b"vk",
            CountingGenerator::new(b"vk", b"pk"),
        );
        provider.key_pair(&negligible_srs()).unwrap();

        // Removing the verifying key leaves an orphan proving key; the next read must recompute
        // rather than trust the orphan.
        fs::remove_file(provider.verification_key_path()).unwrap();
        provider.key_pair(&negligible_srs()).unwrap();

        assert_eq!(
            provider.circuit().calls.get(),
            2,
            "an orphan proving key must trigger a recompute"
        );
        fs::remove_dir_all(&base_dir).ok();
    }
}
