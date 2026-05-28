//! Wrapped genesis verifier pairing the ed25519 Ed25519 verifier with an optional
//! SNARK-friendly Schnorr verifier.

use std::path::Path;

#[cfg(feature = "future_snark")]
use anyhow::anyhow;

use crate::StdResult;
use crate::crypto_helper::{
    GenesisEd25519Signature, GenesisEd25519VerificationKey, GenesisEd25519Verifier, GenesisSigner,
    GenesisVerificationKeyBundle,
};
#[cfg(feature = "future_snark")]
use crate::crypto_helper::{GenesisSchnorrSignature, GenesisSchnorrVerifier};
use crate::entities::SupportedEra;

/// Wraps the two genesis verifiers and the bundle plumbing required to verify a genesis
/// certificate across legacy and dual-signature eras.
#[derive(Debug, Clone)]
pub struct GenesisVerifier {
    /// Ed25519-genesis verifier (legacy Ed25519).
    pub ed25519: GenesisEd25519Verifier,

    /// Schnorr-genesis verifier (Schnorr over Jubjub). `None` when the operator loaded a legacy
    /// single-Ed25519 verification key.
    #[cfg(feature = "future_snark")]
    pub schnorr: Option<GenesisSchnorrVerifier>,
}

impl GenesisVerifier {
    /// Build a verifier wrapper from the ed25519 half only.
    pub fn from_ed25519(verification_key: GenesisEd25519VerificationKey) -> Self {
        Self {
            ed25519: GenesisEd25519Verifier::from_verification_key(verification_key),
            #[cfg(feature = "future_snark")]
            schnorr: None,
        }
    }

    /// Build a fixed-seed deterministic verifier wrapper for non-production use (devnet, tests).
    ///
    /// Mirrors [`GenesisSigner::create_deterministic_signer`] and reproduces that signer's
    /// verification keys, so a deterministically signed genesis certificate verifies. Never use it
    /// for a long-lived genesis key.
    pub fn create_deterministic_verifier() -> Self {
        Self::from_bundle(GenesisSigner::create_deterministic_signer().verification_key_bundle())
    }

    /// Build a verifier wrapper from a dual verification-key bundle.
    pub fn from_bundle(bundle: GenesisVerificationKeyBundle) -> Self {
        Self {
            ed25519: GenesisEd25519Verifier::from_verification_key(bundle.ed25519),
            #[cfg(feature = "future_snark")]
            schnorr: bundle.schnorr.map(GenesisSchnorrVerifier::from_verification_key),
        }
    }

    /// Parse a verification-key hex string, auto-detecting bundle vs legacy via the first hex
    /// character (delegates to [`GenesisVerificationKeyBundle::try_from_hex_or_legacy`]).
    pub fn try_from_hex(raw: &str) -> StdResult<Self> {
        let bundle = GenesisVerificationKeyBundle::try_from_hex_or_legacy(raw)?;
        Ok(Self::from_bundle(bundle))
    }

    /// Read [Self::try_from_hex] from disk.
    pub fn read_from_file(path: &Path) -> StdResult<Self> {
        let raw = std::fs::read_to_string(path)?;
        Self::try_from_hex(&raw)
    }

    /// Reject verifier/era combinations the chain cannot satisfy. Delegates to
    /// [`GenesisVerificationKeyBundle::ensure_supports_era`].
    pub fn ensure_supports_era(&self, era: SupportedEra) -> StdResult<()> {
        self.verification_key_bundle().ensure_supports_era(era)
    }

    /// Verify the ed25519 (Ed25519) signature carried by a genesis certificate.
    pub fn verify_ed25519(
        &self,
        message: &[u8],
        signature: &GenesisEd25519Signature,
    ) -> StdResult<()> {
        self.ed25519.verify(message, signature)
    }

    /// Verify the SNARK-friendly (Schnorr) signature carried by a dual genesis certificate against
    /// the SHA-256 digest of the protocol-message rigid preimage.
    ///
    /// Errors when the verifier holds no SNARK verification key (legacy single-Ed25519 input).
    #[cfg(feature = "future_snark")]
    pub fn verify_schnorr(
        &self,
        sha256_digest: &[u8],
        signature: &GenesisSchnorrSignature,
    ) -> StdResult<()> {
        self.schnorr
            .as_ref()
            .ok_or_else(|| anyhow!("the genesis verifier holds no SNARK verification key"))?
            .verify(sha256_digest, signature)
    }

    /// Return the ed25519 (Ed25519) verification key.
    pub fn to_ed25519_verification_key(&self) -> GenesisEd25519VerificationKey {
        self.ed25519.to_verification_key()
    }

    /// Derive the matching verification-key bundle, suitable for serialisation to disk by the
    /// genesis import and `upgrade-key-to-dual` aggregator subcommands.
    pub fn verification_key_bundle(&self) -> GenesisVerificationKeyBundle {
        GenesisVerificationKeyBundle {
            ed25519: self.ed25519.to_verification_key(),
            #[cfg(feature = "future_snark")]
            schnorr: self.schnorr.as_ref().map(|v| v.to_verification_key()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::crypto_helper::GenesisEd25519Signer;
    #[cfg(feature = "future_snark")]
    use crate::crypto_helper::GenesisSchnorrSigner;
    use crate::temp_dir_create;

    fn deterministic_signer() -> GenesisEd25519Signer {
        GenesisEd25519Signer::create_deterministic_signer()
    }

    #[cfg(feature = "future_snark")]
    #[test]
    fn verify_schnorr_errors_when_verifier_holds_no_schnorr_key() {
        let signer = deterministic_signer();
        let verifier = GenesisVerifier::from_ed25519(signer.verification_key());
        let schnorr_signature = GenesisSchnorrSigner::create_non_deterministic_signer()
            .sign_non_deterministic(&[0u8; 32])
            .unwrap();

        verifier
            .verify_schnorr(&[0u8; 32], &schnorr_signature)
            .expect_err("verify_schnorr must error when the verifier holds no SNARK key");
    }

    #[test]
    fn from_ed25519_yields_no_schnorr_half() {
        let signer = deterministic_signer();
        let verifier = GenesisVerifier::from_ed25519(signer.verification_key());

        assert_eq!(
            verifier.ed25519.to_verification_key().as_bytes(),
            signer.verification_key().as_bytes()
        );
        #[cfg(feature = "future_snark")]
        assert!(verifier.schnorr.is_none());
    }

    #[test]
    fn try_from_hex_accepts_legacy_single_ed25519() {
        let signer = deterministic_signer();
        let legacy_hex = signer.verification_key().to_json_hex().unwrap();

        let verifier = GenesisVerifier::try_from_hex(&legacy_hex).unwrap();

        assert_eq!(
            verifier.ed25519.to_verification_key().as_bytes(),
            signer.verification_key().as_bytes()
        );
        #[cfg(feature = "future_snark")]
        assert!(verifier.schnorr.is_none());
    }

    #[test]
    fn try_from_hex_rejects_empty_input() {
        GenesisVerifier::try_from_hex("   ").expect_err("empty input must be rejected");
    }

    #[test]
    fn read_from_file_round_trips_legacy() {
        let temp = temp_dir_create!();
        let signer = deterministic_signer();
        let path = temp.join("genesis.vk");
        signer.verification_key().write_json_hex_to_file(&path).unwrap();

        let verifier = GenesisVerifier::read_from_file(&path).unwrap();

        assert_eq!(
            verifier.ed25519.to_verification_key().as_bytes(),
            signer.verification_key().as_bytes()
        );
    }

    #[test]
    fn verify_ed25519_accepts_valid_signature() {
        let signer = deterministic_signer();
        let message = b"genesis-message";
        let signature = signer.sign(message);
        let verifier = GenesisVerifier::from_ed25519(signer.verification_key());

        verifier
            .verify_ed25519(message, &signature)
            .expect("valid signature must verify");
    }

    #[test]
    fn verify_ed25519_rejects_tampered_message() {
        let signer = deterministic_signer();
        let signature = signer.sign(b"genesis-message");
        let verifier = GenesisVerifier::from_ed25519(signer.verification_key());

        verifier
            .verify_ed25519(b"tampered-message", &signature)
            .expect_err("tampered message must be rejected");
    }

    #[test]
    fn ensure_supports_era_pythagoras_accepts_ed25519_only() {
        let verifier = GenesisVerifier::from_ed25519(deterministic_signer().verification_key());

        verifier
            .ensure_supports_era(SupportedEra::Pythagoras)
            .expect("Pythagoras must accept a ed25519-only verifier");
    }

    #[test]
    fn verification_key_bundle_mirrors_ed25519_only() {
        let signer = deterministic_signer();
        let verifier = GenesisVerifier::from_ed25519(signer.verification_key());

        let bundle = verifier.verification_key_bundle();

        assert_eq!(
            bundle.ed25519.as_bytes(),
            signer.verification_key().as_bytes()
        );
        #[cfg(feature = "future_snark")]
        assert!(bundle.schnorr.is_none());
    }

    #[cfg(feature = "future_snark")]
    mod schnorr {
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use super::*;
        use crate::crypto_helper::{GenesisBundleError, GenesisSchnorrSigner};

        fn build_bundle() -> GenesisVerificationKeyBundle {
            let ed25519 = deterministic_signer().verification_key();
            let mut rng = ChaCha20Rng::from_seed([7u8; 32]);
            let schnorr = GenesisSchnorrSigner::generate(&mut rng).verification_key();
            GenesisVerificationKeyBundle::new(ed25519, schnorr)
        }

        #[test]
        fn from_bundle_pairs_both_halves() {
            let bundle = build_bundle();
            let expected_ed25519 = bundle.ed25519.as_bytes().to_vec();
            let expected_schnorr = bundle.schnorr.as_ref().unwrap().to_bytes();

            let verifier = GenesisVerifier::from_bundle(bundle);

            assert_eq!(
                verifier.ed25519.to_verification_key().as_bytes(),
                expected_ed25519.as_slice()
            );
            assert_eq!(
                verifier.schnorr.as_ref().unwrap().to_verification_key().to_bytes(),
                expected_schnorr
            );
        }

        #[test]
        fn verify_schnorr_accepts_valid_signature() {
            let ed25519 = deterministic_signer().verification_key();
            let schnorr_signer = GenesisSchnorrSigner::create_deterministic_signer();
            let bundle =
                GenesisVerificationKeyBundle::new(ed25519, schnorr_signer.verification_key());
            let verifier = GenesisVerifier::from_bundle(bundle);
            let digest = [123u8; 32];
            let signature = schnorr_signer.sign_non_deterministic(&digest).unwrap();

            verifier
                .verify_schnorr(&digest, &signature)
                .expect("a valid SNARK signature must verify through the wrapper");
        }

        #[test]
        fn ensure_supports_era_lagrange_rejects_ed25519_only() {
            let verifier = GenesisVerifier::from_ed25519(deterministic_signer().verification_key());

            let error = verifier
                .ensure_supports_era(SupportedEra::Lagrange)
                .expect_err("Lagrange must reject a ed25519-only verifier");

            assert!(matches!(
                error.downcast_ref::<GenesisBundleError>(),
                Some(GenesisBundleError::SchnorrVerificationKeyRequired)
            ));
        }

        #[test]
        fn ensure_supports_era_lagrange_accepts_dual_verifier() {
            let verifier = GenesisVerifier::from_bundle(build_bundle());

            verifier
                .ensure_supports_era(SupportedEra::Lagrange)
                .expect("Lagrange must accept a dual verifier");
        }

        #[test]
        fn verification_key_bundle_mirrors_verifier_halves() {
            let verifier = GenesisVerifier::from_bundle(build_bundle());
            let expected_schnorr =
                verifier.schnorr.as_ref().unwrap().to_verification_key().to_bytes();

            let bundle = verifier.verification_key_bundle();

            assert_eq!(
                bundle.ed25519.as_bytes(),
                verifier.ed25519.to_verification_key().as_bytes()
            );
            assert_eq!(bundle.schnorr.unwrap().to_bytes(), expected_schnorr);
        }
    }
}
