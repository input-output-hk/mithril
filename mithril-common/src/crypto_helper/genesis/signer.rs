//! Wrapped genesis signer pairing the ed25519 Ed25519 signer with an optional
//! SNARK-friendly Schnorr signer.

use std::path::Path;

use anyhow::anyhow;

use crate::StdResult;
use crate::crypto_helper::GenesisEd25519SecretKey;
use crate::crypto_helper::GenesisEd25519Signer;
use crate::crypto_helper::GenesisVerificationKeyBundle;
#[cfg(feature = "future_snark")]
use crate::crypto_helper::{
    BUNDLE_FIRST_HEX_CHAR, GenesisBundleError, GenesisSchnorrSigner, GenesisSigningKeyBundle,
    LEGACY_FIRST_HEX_CHAR, ProtocolKey,
};
use crate::entities::SupportedEra;

/// Wraps the two genesis signers and the bundle plumbing required by the offline ceremony.
#[derive(Debug, Clone)]
pub struct GenesisSigner {
    /// Ed25519-genesis signer (legacy Ed25519).
    pub ed25519: GenesisEd25519Signer,

    /// Schnorr-genesis signer (Schnorr over Jubjub). `None` when the operator loaded a legacy
    /// single-Ed25519 file.
    #[cfg(feature = "future_snark")]
    pub schnorr: Option<GenesisSchnorrSigner>,
}

impl GenesisSigner {
    /// Build a signer wrapper from the ed25519 half only.
    pub fn from_ed25519(ed25519: GenesisEd25519Signer) -> Self {
        Self {
            ed25519,
            #[cfg(feature = "future_snark")]
            schnorr: None,
        }
    }

    /// Build a fixed-seed deterministic signer wrapper for non-production use (devnet, tests).
    ///
    /// Both halves use their deterministic constructor so independently built signers and
    /// verifiers reproduce the same keys. Never use it for a long-lived genesis key.
    pub fn create_deterministic_signer() -> Self {
        Self {
            ed25519: GenesisEd25519Signer::create_deterministic_signer(),
            #[cfg(feature = "future_snark")]
            schnorr: Some(GenesisSchnorrSigner::create_deterministic_signer()),
        }
    }

    /// Build a signer wrapper from a dual signing-key bundle.
    #[cfg(feature = "future_snark")]
    pub fn from_bundle(bundle: GenesisSigningKeyBundle) -> Self {
        Self {
            ed25519: GenesisEd25519Signer::from_secret_key(bundle.ed25519),
            schnorr: Some(GenesisSchnorrSigner::from_secret_key(bundle.schnorr)),
        }
    }

    /// Parse a hex blob, preferring the dual signing-key bundle and falling back to the legacy
    /// single-Ed25519 JSON-hex format.
    pub fn try_from_hex(raw: &str) -> StdResult<Self> {
        let trimmed = raw.trim();
        if trimmed.is_empty() {
            return Err(anyhow!("genesis signer input is empty"));
        }
        #[cfg(feature = "future_snark")]
        {
            match trimmed.as_bytes().first() {
                Some(&BUNDLE_FIRST_HEX_CHAR) => {
                    let bundle = GenesisSigningKeyBundle::try_from_hex(trimmed)?;
                    Ok(Self::from_bundle(bundle))
                }
                Some(&LEGACY_FIRST_HEX_CHAR) => {
                    let secret_key = GenesisEd25519SecretKey::from_json_hex(trimmed)?;
                    Ok(Self::from_ed25519(GenesisEd25519Signer::from_secret_key(
                        secret_key,
                    )))
                }
                _ => Err(anyhow!(
                    "unrecognised genesis signing key format: expected dual bundle or legacy Ed25519 hex"
                )),
            }
        }
        #[cfg(not(feature = "future_snark"))]
        {
            let secret_key = GenesisEd25519SecretKey::from_json_hex(trimmed)?;
            Ok(Self::from_ed25519(GenesisEd25519Signer::from_secret_key(
                secret_key,
            )))
        }
    }

    /// Read [Self::try_from_hex] from disk.
    pub fn read_from_file(path: &Path) -> StdResult<Self> {
        let raw = std::fs::read_to_string(path)?;
        Self::try_from_hex(&raw)
    }

    /// Reject signer/era combinations the producer cannot satisfy.
    pub fn ensure_supports_era(
        &self,
        #[cfg_attr(not(feature = "future_snark"), allow(unused_variables))] era: SupportedEra,
    ) -> StdResult<()> {
        #[cfg(feature = "future_snark")]
        if matches!(era, SupportedEra::Lagrange) && self.schnorr.is_none() {
            return Err(GenesisBundleError::LegacySigningKey.into());
        }
        Ok(())
    }

    /// Derive the matching verification-key bundle.
    #[cfg(feature = "future_snark")]
    pub fn verification_key_bundle(&self) -> GenesisVerificationKeyBundle {
        GenesisVerificationKeyBundle {
            ed25519: self.ed25519.verification_key(),
            schnorr: self.schnorr.as_ref().map(|s| s.verification_key()),
        }
    }

    /// Derive the matching verification-key bundle.
    #[cfg(not(feature = "future_snark"))]
    pub fn verification_key_bundle(&self) -> GenesisVerificationKeyBundle {
        GenesisVerificationKeyBundle::from_ed25519(self.ed25519.verification_key())
    }

    /// Write the wrapped signer to disk in its native hex form (bundle bytes-hex when the SNARK
    /// half is present, legacy JSON-hex otherwise).
    pub fn write_to_file(&self, path: &Path) -> StdResult<()> {
        #[cfg(feature = "future_snark")]
        {
            if let Some(schnorr) = self.schnorr.as_ref() {
                let bundle =
                    GenesisSigningKeyBundle::new(self.ed25519.secret_key(), schnorr.secret_key());
                let hex_string = ProtocolKey::new(bundle).to_bytes_hex()?;
                std::fs::write(path, hex_string)?;
                return Ok(());
            }
        }
        self.ed25519.secret_key().write_json_hex_to_file(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::temp_dir_create;

    #[test]
    fn try_from_hex_accepts_legacy_single_ed25519() {
        let signer = GenesisEd25519Signer::create_deterministic_signer();
        let legacy_hex = signer.secret_key().to_json_hex().unwrap();

        let parsed = GenesisSigner::try_from_hex(&legacy_hex).unwrap();

        assert_eq!(
            parsed.ed25519.secret_key().to_bytes(),
            signer.secret_key().to_bytes()
        );
        #[cfg(feature = "future_snark")]
        assert!(parsed.schnorr.is_none());
    }

    #[test]
    fn create_deterministic_signer_is_reproducible() {
        let first = GenesisSigner::create_deterministic_signer();
        let second = GenesisSigner::create_deterministic_signer();

        assert_eq!(
            first.ed25519.secret_key().to_bytes(),
            second.ed25519.secret_key().to_bytes()
        );
    }

    #[test]
    fn ensure_supports_era_pythagoras_accepts_ed25519_only() {
        let signer =
            GenesisSigner::from_ed25519(GenesisEd25519Signer::create_deterministic_signer());

        signer
            .ensure_supports_era(SupportedEra::Pythagoras)
            .expect("Pythagoras must accept a ed25519-only signer");
    }

    #[test]
    fn try_from_hex_rejects_empty_input() {
        GenesisSigner::try_from_hex("   ").expect_err("empty input must be rejected");
    }

    #[test]
    fn read_from_file_round_trips_legacy() {
        let temp = temp_dir_create!();
        let signer = GenesisEd25519Signer::create_deterministic_signer();
        let path = temp.join("genesis.sk");
        signer.secret_key().write_json_hex_to_file(&path).unwrap();

        let restored = GenesisSigner::read_from_file(&path).unwrap();

        assert_eq!(
            restored.ed25519.secret_key().to_bytes(),
            signer.secret_key().to_bytes()
        );
    }

    #[cfg(feature = "future_snark")]
    mod schnorr {
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use super::*;

        fn build_bundle() -> GenesisSigningKeyBundle {
            let ed25519 = GenesisEd25519Signer::create_deterministic_signer();
            let mut rng = ChaCha20Rng::from_seed([7u8; 32]);
            let schnorr = GenesisSchnorrSigner::generate(&mut rng);
            GenesisSigningKeyBundle::new(ed25519.secret_key(), schnorr.secret_key())
        }

        #[test]
        fn from_bundle_pairs_both_halves() {
            let bundle = build_bundle();
            let expected_ed25519 = bundle.ed25519.to_bytes();
            let expected_schnorr = bundle.schnorr.to_bytes();

            let signer = GenesisSigner::from_bundle(bundle);

            assert_eq!(signer.ed25519.secret_key().to_bytes(), expected_ed25519);
            assert_eq!(
                signer.schnorr.as_ref().unwrap().secret_key().to_bytes(),
                expected_schnorr
            );
        }

        #[test]
        fn try_from_hex_accepts_dual_bundle() {
            let bundle = build_bundle();
            let expected_schnorr = bundle.schnorr.to_bytes();
            let hex_string = ProtocolKey::new(bundle).to_bytes_hex().unwrap();

            let parsed = GenesisSigner::try_from_hex(&hex_string).unwrap();

            assert_eq!(
                parsed.schnorr.as_ref().unwrap().secret_key().to_bytes(),
                expected_schnorr
            );
        }

        #[test]
        fn ensure_supports_era_lagrange_rejects_ed25519_only() {
            let signer =
                GenesisSigner::from_ed25519(GenesisEd25519Signer::create_deterministic_signer());

            let error = signer.ensure_supports_era(SupportedEra::Lagrange).unwrap_err();

            assert!(matches!(
                error.downcast_ref::<GenesisBundleError>(),
                Some(GenesisBundleError::LegacySigningKey)
            ));
        }

        #[test]
        fn ensure_supports_era_lagrange_accepts_dual_signer() {
            let signer = GenesisSigner::from_bundle(build_bundle());

            signer
                .ensure_supports_era(SupportedEra::Lagrange)
                .expect("Lagrange must accept a dual signer");
        }

        #[test]
        fn verification_key_bundle_mirrors_signer_halves() {
            let signer = GenesisSigner::from_bundle(build_bundle());
            let expected_schnorr = signer.schnorr.as_ref().unwrap().verification_key().to_bytes();

            let bundle = signer.verification_key_bundle();

            assert_eq!(
                bundle.ed25519.as_bytes(),
                signer.ed25519.verification_key().as_bytes()
            );
            assert_eq!(bundle.schnorr.unwrap().to_bytes(), expected_schnorr);
        }

        #[test]
        fn write_to_file_round_trips_bundle() {
            let temp = temp_dir_create!();
            let signer = GenesisSigner::from_bundle(build_bundle());
            let path = temp.join("genesis.sk");

            signer.write_to_file(&path).unwrap();
            let restored = GenesisSigner::read_from_file(&path).unwrap();

            assert_eq!(
                restored.ed25519.secret_key().to_bytes(),
                signer.ed25519.secret_key().to_bytes()
            );
            assert_eq!(
                restored.schnorr.as_ref().unwrap().secret_key().to_bytes(),
                signer.schnorr.as_ref().unwrap().secret_key().to_bytes()
            );
        }
    }
}
