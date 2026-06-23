//! SNARK setup for the STM certificate circuit.
//!
//! Bundles circuit compilation and key derivation into [`SnarkProverSetup`], backed by the on-disk
//! [`CircuitKeyCache`] so derived keys are reused across process restarts.
use anyhow::Context;
use midnight_curves::Bls12;
use midnight_proofs::{
    poly::kzg::params::{ParamsKZG, ParamsVerifierKZG},
    utils::SerdeFormat,
};
use midnight_zk_stdlib::{self as zk, MidnightPK, MidnightVK};

use crate::{
    Parameters, StmResult,
    circuits::{
        halo2::circuit::StmCertificateCircuit, key_cache::CircuitKeyCache,
        trusted_setup::TrustedSetupProvider,
    },
    codec::TryToBytes,
    proof_system::KZG_VERIFIER_PARAMS,
};

/// Bundles the one-time setup artifacts needed to prove and verify SNARK proofs.
///
/// This includes the Structured Reference String (SRS), the compiled circuit, and the
/// proving and verification keys derived from them.
pub struct SnarkProverSetup {
    /// KZG Structured Reference String.
    pub(crate) srs: ParamsKZG<Bls12>,
    /// Compiled STM circuit.
    pub(crate) circuit: StmCertificateCircuit,
    /// Verification key for the SNARK proof.
    pub(crate) verification_key: MidnightVK,
    /// Proving key for the SNARK proof.
    pub(crate) proving_key: MidnightPK<StmCertificateCircuit>,
}

impl SnarkProverSetup {
    /// Loads the trusted SRS via [`TrustedSetupProvider`] and delegates to
    /// [`Self::try_new_with_srs`].
    pub(crate) fn try_new(params: &Parameters, merkle_tree_depth: u32) -> StmResult<Self> {
        let srs = TrustedSetupProvider::default().get_trusted_setup_parameters()?;
        Self::try_new_with_srs(
            params,
            merkle_tree_depth,
            srs,
            &CircuitKeyCache::for_non_recursive_circuit(),
        )
    }

    /// Build a new `SnarkProverSetup` from protocol parameters, Merkle tree depth, a caller-supplied
    /// SRS, and the key cache to consult. The SRS must have `max_k >= circuit.min_k()`; it is
    /// downsized to the exact circuit degree before key derivation (required by `keygen_vk`).
    pub(crate) fn try_new_with_srs(
        params: &Parameters,
        merkle_tree_depth: u32,
        mut srs: ParamsKZG<Bls12>,
        key_cache: &CircuitKeyCache,
    ) -> StmResult<Self> {
        let circuit = StmCertificateCircuit::try_new(params, merkle_tree_depth)?;
        zk::downsize_srs_for_relation(&mut srs, &circuit);

        let (verification_key, proving_key) =
            get_or_build_snark_keys_with_disk_cache(&circuit, &srs, key_cache)?;

        Ok(Self {
            srs,
            circuit,
            verification_key,
            proving_key,
        })
    }
}

/// Bundles the minimal setup artifacts needed to verify SNARK proofs.
pub(crate) struct SnarkVerifierSetup {
    /// KZG verifier parameters derived from `s_g2`.
    pub(crate) verifier_params: ParamsVerifierKZG<Bls12>,
}

impl SnarkVerifierSetup {
    /// Build the verifier setup from the embedded constant verifier params bytes.
    pub(crate) fn try_new() -> StmResult<Self> {
        let verifier_params = ParamsVerifierKZG::<Bls12>::read(
            &mut &KZG_VERIFIER_PARAMS[..],
            SerdeFormat::RawBytesUnchecked,
        )
        .with_context(|| "Failed to read embedded SNARK verifier params bytes")?;

        Ok(Self { verifier_params })
    }
}

/// Returns the certificate VK/PK pair from the on-disk [`CircuitKeyCache`], deriving it from the
/// SRS and storing it back on a cache miss.
fn get_or_build_snark_keys_with_disk_cache(
    circuit: &StmCertificateCircuit,
    srs: &ParamsKZG<Bls12>,
    disk_cache: &CircuitKeyCache,
) -> StmResult<(MidnightVK, MidnightPK<StmCertificateCircuit>)> {
    // The proving key is only read once the verifying key is present, so an orphan proving key left
    // by a failed store (which removes the verifying key) is recomputed rather than surfaced as a
    // deserialization error.
    if let Some(verification_key) = disk_cache.get_verification_key::<MidnightVK>()?
        && let Some(proving_key) =
            disk_cache.get_proving_key::<MidnightPK<StmCertificateCircuit>>()?
    {
        return Ok((verification_key, proving_key));
    }

    let verification_key = zk::setup_vk(srs, circuit);
    let proving_key = zk::setup_pk(circuit, &verification_key);
    disk_cache.store_key_pair(
        &verification_key.to_bytes_vec()?,
        &proving_key.to_bytes_vec()?,
    )?;
    Ok((verification_key, proving_key))
}

#[cfg(test)]
mod test {
    use std::fs;

    use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};
    use midnight_zk_stdlib::{self as zk, MidnightCircuit};
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        Parameters, circuits::halo2::circuit::StmCertificateCircuit,
        circuits::key_cache::CircuitKeyCache, proof_system::halo2_snark::SnarkProverSetup,
    };

    use super::get_or_build_snark_keys_with_disk_cache;

    fn default_params() -> Parameters {
        Parameters {
            k: 3,
            m: 10,
            phi_f: 0.2,
        }
    }

    #[test]
    fn try_new_with_srs_succeeds_with_valid_parameters() {
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 4).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let srs = ParamsKZG::unsafe_setup(degree, ChaCha20Rng::seed_from_u64(42));
        let base_dir = std::env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let cache = CircuitKeyCache::new(base_dir.clone(), "non-recursive", b"test-vk");
        let result = SnarkProverSetup::try_new_with_srs(&params, 4, srs, &cache);
        assert!(result.is_ok());
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn try_new_with_srs_returns_same_verification_key_for_same_parameters() {
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 4).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let make_srs = || ParamsKZG::unsafe_setup(degree, ChaCha20Rng::seed_from_u64(42));
        let base_dir = std::env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let cache = CircuitKeyCache::new(base_dir.clone(), "non-recursive", b"test-vk");
        let setup1 = SnarkProverSetup::try_new_with_srs(&params, 4, make_srs(), &cache).unwrap();
        let setup2 = SnarkProverSetup::try_new_with_srs(&params, 4, make_srs(), &cache).unwrap();

        let mut vk_bytes1 = vec![];
        setup1
            .verification_key
            .write(&mut vk_bytes1, SerdeFormat::RawBytes)
            .unwrap();
        let mut vk_bytes2 = vec![];
        setup2
            .verification_key
            .write(&mut vk_bytes2, SerdeFormat::RawBytes)
            .unwrap();

        assert_eq!(
            vk_bytes1, vk_bytes2,
            "same parameters must produce the same verification key"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    mod verifier_setup {
        use super::*;
        use crate::circuits::trusted_setup::TrustedSetupProvider;
        use crate::proof_system::halo2_snark::SnarkVerifierSetup;

        #[test]
        #[ignore = "requires SRS download from the internet"]
        fn verifier_setup_matches_trusted_srs() {
            let setup = SnarkVerifierSetup::try_new().unwrap();
            let srs = TrustedSetupProvider::default()
                .get_trusted_setup_parameters()
                .unwrap();
            let expected = srs.verifier_params();
            let mut expected_bytes = vec![];
            expected
                .write(&mut expected_bytes, SerdeFormat::RawBytesUnchecked)
                .unwrap();
            let mut actual_bytes = vec![];
            setup
                .verifier_params
                .write(&mut actual_bytes, SerdeFormat::RawBytesUnchecked)
                .unwrap();
            assert_eq!(
                expected_bytes, actual_bytes,
                "verifier params must match the Midnight trusted SRS"
            );
        }
    }

    #[test]
    fn disk_cache_empty_writes_keys_to_disk() {
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 100).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let srs = ParamsKZG::unsafe_setup(degree, ChaCha20Rng::seed_from_u64(42));
        let base_dir = std::env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let disk_cache = CircuitKeyCache::new(base_dir.clone(), "non-recursive", b"placeholder");

        assert!(!disk_cache.verification_key_path().exists());
        get_or_build_snark_keys_with_disk_cache(&circuit, &srs, &disk_cache).unwrap();
        assert!(
            disk_cache.verification_key_path().exists(),
            "VK should be written to disk on cache miss"
        );
        assert!(
            disk_cache.proving_key_path().exists(),
            "PK should be written to disk on cache miss"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn disk_cache_valid_returns_expected_vk_bytes() {
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 101).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let srs = ParamsKZG::unsafe_setup(degree, ChaCha20Rng::seed_from_u64(42));
        let vk = zk::setup_vk(&srs, &circuit);
        let pk = zk::setup_pk(&circuit, &vk);
        let mut vk_bytes = vec![];
        vk.write(&mut vk_bytes, SerdeFormat::RawBytes).unwrap();
        let mut pk_bytes = vec![];
        pk.write(&mut pk_bytes, SerdeFormat::RawBytes).unwrap();

        let base_dir = std::env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let disk_cache = CircuitKeyCache::new(base_dir.clone(), "non-recursive", &vk_bytes);

        fs::create_dir_all(disk_cache.verification_key_path().parent().unwrap()).unwrap();
        fs::write(disk_cache.verification_key_path(), &vk_bytes).unwrap();
        fs::write(disk_cache.proving_key_path(), &pk_bytes).unwrap();

        let loaded = get_or_build_snark_keys_with_disk_cache(&circuit, &srs, &disk_cache).unwrap();

        let mut loaded_vk_bytes = vec![];
        loaded.0.write(&mut loaded_vk_bytes, SerdeFormat::RawBytes).unwrap();
        assert_eq!(
            loaded_vk_bytes, vk_bytes,
            "loaded VK must match what was written to disk"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn orphan_corrupt_proving_key_is_recomputed_not_errored() {
        // A failed store_key_pair removes the verifying key but can leave a partial proving key, so
        // an orphan corrupt proving key with no verifying key must be treated as a cache miss and
        // recomputed, not surfaced as a deserialization error.
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 4).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let srs = ParamsKZG::unsafe_setup(degree, ChaCha20Rng::seed_from_u64(42));

        let base_dir = std::env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let disk_cache = CircuitKeyCache::new(base_dir.clone(), "non-recursive", b"placeholder");
        fs::create_dir_all(disk_cache.proving_key_path().parent().unwrap()).unwrap();
        fs::write(disk_cache.proving_key_path(), b"not-a-valid-proving-key").unwrap();
        assert!(!disk_cache.verification_key_path().exists());

        let result = get_or_build_snark_keys_with_disk_cache(&circuit, &srs, &disk_cache);
        assert!(
            result.is_ok(),
            "orphan corrupt proving key must be recomputed, not error"
        );
        assert!(
            disk_cache.verification_key_path().exists(),
            "the verifying key should be recomputed and written to disk"
        );
        fs::remove_dir_all(&base_dir).ok();
    }
}
