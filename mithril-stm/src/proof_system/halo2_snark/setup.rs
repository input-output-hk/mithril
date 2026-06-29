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
    /// Loads the trusted SRS via [`TrustedSetupProvider`], builds the production certificate
    /// verification key provider, and delegates to [`Self::try_new_with_srs`].
    pub(crate) fn try_new(params: &Parameters, merkle_tree_depth: u32) -> StmResult<Self> {
        let srs = TrustedSetupProvider::default().get_trusted_setup_parameters()?;
        let provider = KeyProvider::for_non_recursive_circuit(params, merkle_tree_depth)?;
        Self::try_new_with_srs(srs, &provider)
    }

    /// Builds a `SnarkProverSetup` from a caller-supplied SRS and certificate verification key
    /// provider. The circuit is taken from the provider (single source); the SRS must have
    /// `max_k >= circuit.min_k()` and is downsized to that circuit's degree and stored for proving;
    /// the key pair is read from, or derived through, the provider.
    pub(crate) fn try_new_with_srs(
        mut srs: ParamsKZG<Bls12>,
        provider: &KeyProvider<StmCertificateCircuit>,
    ) -> StmResult<Self> {
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

    use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};
    use midnight_zk_stdlib::MidnightCircuit;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        Parameters, circuits::halo2::circuit::StmCertificateCircuit,
        circuits::key_provider::KeyProvider, codec::TryToBytes,
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
    fn try_new_with_srs_succeeds_with_valid_parameters() {
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 4).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let srs = ParamsKZG::unsafe_setup(degree, ChaCha20Rng::seed_from_u64(42));
        let base_dir = std::env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let provider = KeyProvider::new(base_dir.clone(), "non-recursive", b"test-vk", circuit);
        let result = SnarkProverSetup::try_new_with_srs(srs, &provider);
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
        let provider = KeyProvider::new(base_dir.clone(), "non-recursive", b"test-vk", circuit);
        let setup1 = SnarkProverSetup::try_new_with_srs(make_srs(), &provider).unwrap();
        let setup2 = SnarkProverSetup::try_new_with_srs(make_srs(), &provider).unwrap();

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
