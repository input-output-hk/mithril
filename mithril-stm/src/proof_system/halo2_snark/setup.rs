//! SNARK setup for the STM certificate circuit.
//!
//! Bundles circuit compilation and key derivation into [`SnarkProverSetup`], backed by the on-disk
//! [`KeyProvider`] so derived keys are reused across process restarts.
use anyhow::Context;
use midnight_curves::Bls12;
use midnight_proofs::{
    poly::kzg::params::{ParamsKZG, ParamsVerifierKZG},
    utils::SerdeFormat,
};
use midnight_zk_stdlib as zk;

use crate::{
    Parameters, StmResult,
    circuits::{
        halo2::{
            circuit::StmCertificateCircuit,
            keys::{NonRecursiveCircuitProvingKey, NonRecursiveCircuitVerifyingKey},
        },
        key_provider::KeyProvider,
        trusted_setup::TrustedSetupProvider,
    },
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
    pub(crate) verification_key: NonRecursiveCircuitVerifyingKey,
    /// Proving key for the SNARK proof.
    pub(crate) proving_key: NonRecursiveCircuitProvingKey,
}

impl SnarkProverSetup {
    /// Builds the production trusted setup provider and certificate key provider, then delegates to
    /// [`Self::load`].
    pub(crate) fn try_new(params: &Parameters, merkle_tree_depth: u32) -> StmResult<Self> {
        let trusted_setup_provider = TrustedSetupProvider::default();
        let provider = KeyProvider::for_non_recursive_circuit(params, merkle_tree_depth)?;
        Self::load(&trusted_setup_provider, &provider)
    }

    /// Derives the certificate setup from a trusted setup provider and a certificate key provider,
    /// mirroring `IvcSnarkProverSetup::load` on the recursive side.
    ///
    /// Loads the SRS from the trusted setup provider, takes the circuit from the key provider (single
    /// source), downsizes the SRS to that circuit's degree and stores it for proving, and reads or
    /// derives the key pair through the key provider.
    pub(crate) fn load(
        trusted_setup_provider: &TrustedSetupProvider,
        provider: &KeyProvider<StmCertificateCircuit>,
    ) -> StmResult<Self> {
        let mut srs = trusted_setup_provider.get_trusted_setup_parameters()?;
        let circuit = provider.circuit().clone();
        zk::downsize_srs_for_relation(&mut srs, &circuit);
        let (verification_key, proving_key) = provider.key_pair(&srs)?;

        Ok(Self {
            srs,
            circuit,
            verification_key,
            proving_key,
        })
    }
}

/// Builds a [`SnarkProverSetup`] from a deterministic, oversized unsafe SRS, exercising the real `load`
/// path without the production SRS. Mirrors `build_unsafe_ivc_setup` on the recursive side. Shared by
/// the slow certificate tests through a content-keyed cache keyed by the protocol parameters,
/// Merkle-tree depth, the unsafe SRS seed, and the production verifying key as a circuit-version salt,
/// so the certificate key is computed once and reused across tests and runs.
#[cfg(test)]
pub(crate) fn build_unsafe_certificate_setup(
    parameters: &Parameters,
    merkle_tree_depth: u32,
) -> StmResult<SnarkProverSetup> {
    use crate::circuits::{
        halo2::NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        halo2_ivc::RECURSIVE_CIRCUIT_DEGREE,
        test_utils::{file_mutex::FileMutex, key_cache::shared_cache_directory},
        trusted_setup::UNSAFE_SRS_SEED,
    };

    let parameters_bytes = parameters.to_bytes()?;
    let depth_bytes = merkle_tree_depth.to_le_bytes();
    let seed_bytes = UNSAFE_SRS_SEED.to_le_bytes();
    let cache_directory = shared_cache_directory(
        "non-recursive",
        &[
            NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
            &parameters_bytes,
            &depth_bytes,
            &seed_bytes,
        ],
    );
    // Serialize cold-start keygen across the parallel slow-test processes.
    let _key_cache_lock = FileMutex::new(cache_directory.join(".lock")).lock()?;

    let trusted_setup_provider =
        TrustedSetupProvider::with_unsafe_srs(&cache_directory, RECURSIVE_CIRCUIT_DEGREE);
    let circuit = StmCertificateCircuit::try_new(parameters, merkle_tree_depth)?;
    let provider = KeyProvider::new(cache_directory, "non-recursive", &[], circuit);
    SnarkProverSetup::load(&trusted_setup_provider, &provider)
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

#[cfg(test)]
mod test {
    use std::fs;

    use midnight_proofs::utils::SerdeFormat;
    use midnight_zk_stdlib::MidnightCircuit;

    use crate::{
        Parameters,
        circuits::{
            halo2::circuit::StmCertificateCircuit, key_provider::KeyProvider,
            trusted_setup::TrustedSetupProvider,
        },
        codec::TryToBytes,
        proof_system::halo2_snark::SnarkProverSetup,
    };

    fn default_params() -> Parameters {
        Parameters {
            k: 3,
            m: 10,
            phi_f: 0.2,
        }
    }

    #[test]
    fn load_succeeds_with_valid_parameters() {
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 4).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let base_dir = std::env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let trusted_setup_provider = TrustedSetupProvider::with_unsafe_srs(&base_dir, degree);
        let provider = KeyProvider::new(base_dir.clone(), "non-recursive", b"test-vk", circuit);
        let result = SnarkProverSetup::load(&trusted_setup_provider, &provider);
        assert!(result.is_ok());
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn load_returns_same_verification_key_for_same_parameters() {
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 4).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let base_dir = std::env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let trusted_setup_provider = TrustedSetupProvider::with_unsafe_srs(&base_dir, degree);
        let provider = KeyProvider::new(base_dir.clone(), "non-recursive", b"test-vk", circuit);
        let setup1 = SnarkProverSetup::load(&trusted_setup_provider, &provider).unwrap();
        let setup2 = SnarkProverSetup::load(&trusted_setup_provider, &provider).unwrap();

        let vk_bytes1 = setup1.verification_key.to_bytes_vec().unwrap();
        let vk_bytes2 = setup2.verification_key.to_bytes_vec().unwrap();

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
}
