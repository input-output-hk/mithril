//! Versioned bytes-hex bundles that pair the legacy ed25519-genesis Ed25519 key with the
//! SNARK-friendly Schnorr genesis key.
//!
//! Version 1 is the only defined version; the parsers reject every other version byte upfront.
//!
//! Both bundle types implement [TryToBytes]/[TryFromBytes] and are wrapped in
//! [ProtocolKey] so the hex-encoding plumbing is shared with every other key type in the
//! workspace.

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::StdResult;
use crate::crypto_helper::GenesisEd25519VerificationKey;
#[cfg(feature = "future_snark")]
use crate::crypto_helper::{
    GenesisEd25519SecretKey, GenesisSchnorrSecretKey, GenesisSchnorrVerificationKey, ProtocolKey,
    ProtocolKeyCodec, TryFromBytes, TryToBytes,
};
use crate::entities::SupportedEra;

/// Current (only) supported bundle version.
#[cfg(feature = "future_snark")]
pub const GENESIS_BUNDLE_VERSION: u8 = 1;

/// Expected raw Ed25519 verification-key byte length.
#[cfg(feature = "future_snark")]
pub const ED25519_VERIFICATION_KEY_BYTES: u8 = 32;

/// Expected raw Schnorr verification-key byte length (two Jubjub base field coordinates).
#[cfg(feature = "future_snark")]
pub const SCHNORR_VERIFICATION_KEY_BYTES: u8 = 64;

/// Expected raw Ed25519 signing-key byte length.
#[cfg(feature = "future_snark")]
pub const ED25519_SIGNING_KEY_BYTES: u8 = 32;

/// Expected raw Schnorr signing-key byte length (one Jubjub scalar).
#[cfg(feature = "future_snark")]
pub const SCHNORR_SIGNING_KEY_BYTES: u8 = 32;

/// First hex character of a version-1 bundle (`0` from the leading `0x01` version byte).
#[cfg(feature = "future_snark")]
pub const BUNDLE_FIRST_HEX_CHAR: u8 = b'0';

/// First hex character of the legacy single-Ed25519 file (`5` from the JSON `[` array opener).
pub const LEGACY_FIRST_HEX_CHAR: u8 = b'5';

/// Errors raised when parsing or serialising a genesis key bundle.
#[derive(Error, Debug)]
pub enum GenesisBundleError {
    /// The bundle hex string is empty.
    #[error("genesis bundle input is empty")]
    EmptyInput,

    /// Verification-key input does not match either the legacy single-Ed25519 file format or a
    /// dual-signature bundle (no recognised sniff prefix).
    #[error(
        "unrecognised genesis verification key format: expected the legacy Ed25519-only file (first hex character `5`) or a dual-signature bundle (first hex character `0`, version 1)"
    )]
    UnrecognisedVerificationKeyFormat,

    /// Signing-key input is the legacy single-Ed25519 format. Operators must convert it offline.
    #[error(
        "the file at `--genesis-secret-key-path` looks like a legacy single-Ed25519 signing key; convert it offline with `mithril-aggregator genesis upgrade-key-to-dual`, then rerun (see docs/runbook/genesis-manually/README.md)"
    )]
    LegacySigningKey,

    /// Signing-key input does not match the v1 dual-signature bundle format.
    #[error(
        "unrecognised genesis signing key format: expected a dual-signature signing bundle (first hex character `0`, version 1)"
    )]
    UnrecognisedSigningKeyFormat,

    /// The bundle declares a version other than [GENESIS_BUNDLE_VERSION].
    #[error("unsupported genesis bundle version: {version} (only version 1 is supported)")]
    UnsupportedVersion {
        /// Version byte read from the bundle.
        version: u8,
    },

    /// A length-prefix field in the bundle does not match its expected fixed value.
    #[error("{field_kind} length prefix mismatch: expected {expected} bytes, got {actual}")]
    LengthPrefixMismatch {
        /// Field name (`Ed25519` or `Schnorr`) prefixed with key kind.
        field_kind: &'static str,
        /// Expected byte length pinned by [GENESIS_BUNDLE_VERSION].
        expected: u8,
        /// Length actually declared by the bundle.
        actual: u8,
    },

    /// The bundle hex string decoded to fewer bytes than the layout requires.
    #[error("genesis bundle truncated: missing {field}")]
    Truncated {
        /// Layout segment that ran short.
        field: &'static str,
    },

    /// The bundle hex string decoded to more bytes than the layout consumes.
    #[error("trailing bytes in genesis bundle ({extra} extra bytes)")]
    TrailingBytes {
        /// Number of unconsumed bytes after the bundle layout.
        extra: usize,
    },

    /// A verification-key bundle without a Schnorr key was passed to a writer.
    #[error("cannot serialise a verification-key bundle without a Schnorr key")]
    MissingSchnorrKey,

    /// SNARK operation requested but the bundle holds only the legacy Ed25519 key.
    #[error(
        "this operation requires the Schnorr genesis verification key; the bundle holds only the legacy Ed25519 key, fetch the dual-signature bundle from the URL listed in `docs/website/root/networks-matrix.md`"
    )]
    SchnorrVerificationKeyRequired,
}

/// Dual genesis verification-key bundle: a legacy Ed25519 key (always present) paired with an
/// optional SNARK-friendly Schnorr key.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GenesisVerificationKeyBundle {
    /// Ed25519-genesis verification key (legacy Ed25519).
    pub ed25519: GenesisEd25519VerificationKey,

    /// Schnorr-genesis verification key (Schnorr over Jubjub). `None` when the operator supplies the
    /// legacy single-key file; SNARK operations fail with an actionable error in that case.
    #[cfg(feature = "future_snark")]
    pub schnorr: Option<GenesisSchnorrVerificationKey>,
}

impl GenesisVerificationKeyBundle {
    /// Build a bundle from the ed25519 verification key alone (legacy single-Ed25519 layout).
    pub fn from_ed25519(ed25519: GenesisEd25519VerificationKey) -> Self {
        Self {
            ed25519,
            #[cfg(feature = "future_snark")]
            schnorr: None,
        }
    }

    /// Build a fresh bundle from the two raw verification keys.
    #[cfg(feature = "future_snark")]
    pub fn new(
        ed25519: GenesisEd25519VerificationKey,
        schnorr: GenesisSchnorrVerificationKey,
    ) -> Self {
        Self {
            ed25519,
            schnorr: Some(schnorr),
        }
    }

    /// Parse a verification-key hex string, auto-detecting bundle vs legacy via the first hex
    /// character. Legacy inputs yield a bundle with `schnorr: None`.
    pub fn try_from_hex_or_legacy(raw: &str) -> StdResult<Self> {
        let trimmed = raw.trim();
        match trimmed.as_bytes().first() {
            None => Err(GenesisBundleError::EmptyInput.into()),
            #[cfg(feature = "future_snark")]
            Some(&BUNDLE_FIRST_HEX_CHAR) => {
                let key = ProtocolKey::<Self>::from_bytes_hex(trimmed)?;
                Ok(key.into_inner())
            }
            Some(&LEGACY_FIRST_HEX_CHAR) => {
                let ed25519 = GenesisEd25519VerificationKey::try_from(trimmed)?;
                Ok(Self::from_ed25519(ed25519))
            }
            _ => Err(GenesisBundleError::UnrecognisedVerificationKeyFormat.into()),
        }
    }

    /// Reject verification-key/era combinations the producer cannot satisfy. Lagrange requires
    /// the SNARK half to be present; Pythagoras tolerates either layout.
    pub fn ensure_supports_era(&self, era: SupportedEra) -> StdResult<()> {
        #[cfg(feature = "future_snark")]
        if matches!(era, SupportedEra::Lagrange) && self.schnorr.is_none() {
            return Err(GenesisBundleError::SchnorrVerificationKeyRequired.into());
        }
        #[cfg(not(feature = "future_snark"))]
        let _ = era;
        Ok(())
    }
}

#[cfg(feature = "future_snark")]
impl TryToBytes for GenesisVerificationKeyBundle {
    fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
        let schnorr = self.schnorr.as_ref().ok_or(GenesisBundleError::MissingSchnorrKey)?;
        let mut bytes = Vec::with_capacity(2 + 32 + 1 + 64);
        bytes.push(GENESIS_BUNDLE_VERSION);
        bytes.push(ED25519_VERIFICATION_KEY_BYTES);
        bytes.extend_from_slice(self.ed25519.as_bytes());
        bytes.push(SCHNORR_VERIFICATION_KEY_BYTES);
        bytes.extend_from_slice(&schnorr.to_bytes());
        Ok(bytes)
    }
}

#[cfg(feature = "future_snark")]
impl TryFromBytes for GenesisVerificationKeyBundle {
    fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
        let mut reader = GenesisBundleReader::new(bytes);
        reader.check_version()?;
        let ed25519_bytes = reader.read_length_prefixed(
            "Verification-key Ed25519",
            ED25519_VERIFICATION_KEY_BYTES,
            "ed25519 verification key body",
        )?;
        let schnorr_bytes = reader.read_length_prefixed(
            "Verification-key Schnorr",
            SCHNORR_VERIFICATION_KEY_BYTES,
            "schnorr verification key body",
        )?;
        reader.check_no_trailing()?;
        let ed25519 = GenesisEd25519VerificationKey::from_bytes(ed25519_bytes)?;
        let schnorr = GenesisSchnorrVerificationKey::from_bytes(schnorr_bytes)?;
        Ok(Self {
            ed25519,
            schnorr: Some(schnorr),
        })
    }
}

#[cfg(feature = "future_snark")]
impl ProtocolKeyCodec<GenesisVerificationKeyBundle> for GenesisVerificationKeyBundle {
    fn decode_key(encoded: &str) -> StdResult<ProtocolKey<GenesisVerificationKeyBundle>> {
        ProtocolKey::from_bytes_hex(encoded)
    }

    fn encode_key(key: &GenesisVerificationKeyBundle) -> StdResult<String> {
        ProtocolKey::key_to_bytes_hex(key)
    }
}

#[cfg(feature = "future_snark")]
impl From<ProtocolKey<GenesisVerificationKeyBundle>> for GenesisVerificationKeyBundle {
    fn from(value: ProtocolKey<GenesisVerificationKeyBundle>) -> Self {
        value.into_inner()
    }
}

#[cfg(feature = "future_snark")]
impl From<GenesisVerificationKeyBundle> for ProtocolKey<GenesisVerificationKeyBundle> {
    fn from(value: GenesisVerificationKeyBundle) -> Self {
        ProtocolKey::new(value)
    }
}

/// Dual genesis signing-key bundle: legacy Ed25519 secret paired with the SNARK-friendly Schnorr
/// secret. Both are required; the era setting on the aggregator decides which one is used.
#[cfg(feature = "future_snark")]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenesisSigningKeyBundle {
    /// Ed25519-genesis signing key (legacy Ed25519).
    pub ed25519: GenesisEd25519SecretKey,

    /// Schnorr-genesis signing key (Schnorr over Jubjub).
    pub schnorr: GenesisSchnorrSecretKey,
}

#[cfg(feature = "future_snark")]
impl GenesisSigningKeyBundle {
    /// Build a fresh bundle from two existing secrets.
    pub fn new(ed25519: GenesisEd25519SecretKey, schnorr: GenesisSchnorrSecretKey) -> Self {
        Self { ed25519, schnorr }
    }

    /// Parse a signing-key hex string. Refuses legacy single-Ed25519 inputs with a precise
    /// migration message that points operators at `upgrade-key-to-dual`.
    pub fn try_from_hex(raw: &str) -> StdResult<Self> {
        let trimmed = raw.trim();
        match trimmed.as_bytes().first() {
            None => Err(GenesisBundleError::EmptyInput.into()),
            Some(&LEGACY_FIRST_HEX_CHAR) => Err(GenesisBundleError::LegacySigningKey.into()),
            Some(&BUNDLE_FIRST_HEX_CHAR) => {
                let key = ProtocolKey::<Self>::from_bytes_hex(trimmed)?;
                Ok(key.into_inner())
            }
            _ => Err(GenesisBundleError::UnrecognisedSigningKeyFormat.into()),
        }
    }
}

#[cfg(feature = "future_snark")]
impl TryToBytes for GenesisSigningKeyBundle {
    fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
        let mut bytes = Vec::with_capacity(2 + 32 + 1 + 32);
        bytes.push(GENESIS_BUNDLE_VERSION);
        bytes.push(ED25519_SIGNING_KEY_BYTES);
        bytes.extend_from_slice(&self.ed25519.to_bytes());
        bytes.push(SCHNORR_SIGNING_KEY_BYTES);
        bytes.extend_from_slice(&self.schnorr.to_bytes());
        Ok(bytes)
    }
}

#[cfg(feature = "future_snark")]
impl TryFromBytes for GenesisSigningKeyBundle {
    fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
        let mut reader = GenesisBundleReader::new(bytes);
        reader.check_version()?;
        let ed25519_bytes = reader.read_length_prefixed(
            "Signing-key Ed25519",
            ED25519_SIGNING_KEY_BYTES,
            "ed25519 signing key body",
        )?;
        let schnorr_bytes = reader.read_length_prefixed(
            "Signing-key Schnorr",
            SCHNORR_SIGNING_KEY_BYTES,
            "schnorr signing key body",
        )?;
        reader.check_no_trailing()?;
        let ed25519 = GenesisEd25519SecretKey::from_bytes(ed25519_bytes)?;
        let schnorr = GenesisSchnorrSecretKey::from_bytes(schnorr_bytes)?;
        Ok(Self { ed25519, schnorr })
    }
}

#[cfg(feature = "future_snark")]
impl ProtocolKeyCodec<GenesisSigningKeyBundle> for GenesisSigningKeyBundle {
    fn decode_key(encoded: &str) -> StdResult<ProtocolKey<GenesisSigningKeyBundle>> {
        ProtocolKey::from_bytes_hex(encoded)
    }

    fn encode_key(key: &GenesisSigningKeyBundle) -> StdResult<String> {
        ProtocolKey::key_to_bytes_hex(key)
    }
}

#[cfg(feature = "future_snark")]
impl From<ProtocolKey<GenesisSigningKeyBundle>> for GenesisSigningKeyBundle {
    fn from(value: ProtocolKey<GenesisSigningKeyBundle>) -> Self {
        value.into_inner()
    }
}

#[cfg(feature = "future_snark")]
impl From<GenesisSigningKeyBundle> for ProtocolKey<GenesisSigningKeyBundle> {
    fn from(value: GenesisSigningKeyBundle) -> Self {
        ProtocolKey::new(value)
    }
}

/// Cursor over a genesis bundle byte buffer, advancing as each segment is consumed.
#[cfg(feature = "future_snark")]
struct GenesisBundleReader<'a> {
    cursor: &'a [u8],
}

#[cfg(feature = "future_snark")]
impl<'a> GenesisBundleReader<'a> {
    /// Build a reader positioned at the start of the buffer.
    fn new(bytes: &'a [u8]) -> Self {
        Self { cursor: bytes }
    }

    /// Read and validate the leading version byte against [GENESIS_BUNDLE_VERSION].
    fn check_version(&mut self) -> StdResult<()> {
        let version = self.read_u8("version")?;
        if version != GENESIS_BUNDLE_VERSION {
            return Err(GenesisBundleError::UnsupportedVersion { version }.into());
        }
        Ok(())
    }

    /// Read a length-prefixed segment, requiring the prefix to equal `expected`.
    fn read_length_prefixed(
        &mut self,
        field_kind: &'static str,
        expected: u8,
        body_field: &'static str,
    ) -> StdResult<&'a [u8]> {
        let actual = self.read_u8("length prefix")?;
        if actual != expected {
            return Err(GenesisBundleError::LengthPrefixMismatch {
                field_kind,
                expected,
                actual,
            }
            .into());
        }
        self.read_slice(expected as usize, body_field)
    }

    /// Assert the buffer is fully consumed.
    fn check_no_trailing(&self) -> StdResult<()> {
        if self.cursor.is_empty() {
            Ok(())
        } else {
            Err(GenesisBundleError::TrailingBytes {
                extra: self.cursor.len(),
            }
            .into())
        }
    }

    /// Read a single byte, advancing the cursor.
    fn read_u8(&mut self, field: &'static str) -> StdResult<u8> {
        match self.cursor.split_first() {
            Some((byte, rest)) => {
                self.cursor = rest;
                Ok(*byte)
            }
            None => Err(GenesisBundleError::Truncated { field }.into()),
        }
    }

    /// Read `len` bytes, advancing the cursor.
    fn read_slice(&mut self, len: usize, field: &'static str) -> StdResult<&'a [u8]> {
        if self.cursor.len() < len {
            return Err(GenesisBundleError::Truncated { field }.into());
        }
        let remaining = self.cursor;
        let (head, tail) = remaining.split_at(len);
        self.cursor = tail;
        Ok(head)
    }
}

#[cfg(all(test, feature = "future_snark"))]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::crypto_helper::{GenesisEd25519Signer, GenesisSchnorrSigner};

    use super::*;

    fn downcast_bundle_error(error: &crate::StdError) -> &GenesisBundleError {
        error
            .downcast_ref::<GenesisBundleError>()
            .expect("expected a GenesisBundleError variant")
    }

    fn deterministic_bundle() -> (
        GenesisVerificationKeyBundle,
        GenesisSigningKeyBundle,
        String,
        String,
    ) {
        let mut rng = ChaCha20Rng::from_seed([42u8; 32]);
        let ed25519_signer = GenesisEd25519Signer::create_deterministic_signer();
        let schnorr_signer = GenesisSchnorrSigner::generate(&mut rng);

        let verification_bundle = GenesisVerificationKeyBundle::new(
            ed25519_signer.verification_key(),
            schnorr_signer.verification_key(),
        );
        let signing_bundle =
            GenesisSigningKeyBundle::new(ed25519_signer.secret_key(), schnorr_signer.secret_key());

        let verification_hex =
            ProtocolKey::new(verification_bundle.clone()).to_bytes_hex().unwrap();
        let signing_hex = ProtocolKey::new(signing_bundle.clone()).to_bytes_hex().unwrap();

        (
            verification_bundle,
            signing_bundle,
            verification_hex,
            signing_hex,
        )
    }

    #[test]
    fn verification_bundle_round_trips_through_protocol_key_bytes_hex() {
        let (bundle, _, hex_string, _) = deterministic_bundle();

        let restored = ProtocolKey::<GenesisVerificationKeyBundle>::from_bytes_hex(&hex_string)
            .unwrap()
            .into_inner();

        assert_eq!(bundle.ed25519.as_bytes(), restored.ed25519.as_bytes());
        assert_eq!(
            bundle.schnorr.unwrap().to_bytes(),
            restored.schnorr.unwrap().to_bytes(),
        );
    }

    #[test]
    fn signing_bundle_round_trips_through_protocol_key_bytes_hex() {
        let (_, bundle, _, hex_string) = deterministic_bundle();

        let restored = ProtocolKey::<GenesisSigningKeyBundle>::from_bytes_hex(&hex_string)
            .unwrap()
            .into_inner();

        assert_eq!(bundle.ed25519.to_bytes(), restored.ed25519.to_bytes());
        assert_eq!(bundle.schnorr.to_bytes(), restored.schnorr.to_bytes());
    }

    #[test]
    fn verification_bundle_sniffs_legacy_format_via_first_character() {
        let signer = GenesisEd25519Signer::create_deterministic_signer();
        let legacy_hex = signer.verification_key().to_json_hex().unwrap();
        assert_eq!(legacy_hex.as_bytes()[0], LEGACY_FIRST_HEX_CHAR);

        let parsed = GenesisVerificationKeyBundle::try_from_hex_or_legacy(&legacy_hex).unwrap();

        assert!(
            parsed.schnorr.is_none(),
            "legacy input must yield no Schnorr key"
        );
        assert_eq!(
            parsed.ed25519.as_bytes(),
            signer.verification_key().as_bytes()
        );
    }

    #[test]
    fn verification_bundle_rejects_empty_input() {
        let error = GenesisVerificationKeyBundle::try_from_hex_or_legacy("   ").unwrap_err();

        assert!(matches!(
            downcast_bundle_error(&error),
            GenesisBundleError::EmptyInput
        ));
    }

    #[test]
    fn verification_bundle_rejects_unknown_first_character() {
        let error = GenesisVerificationKeyBundle::try_from_hex_or_legacy("abc").unwrap_err();

        assert!(matches!(
            downcast_bundle_error(&error),
            GenesisBundleError::UnrecognisedVerificationKeyFormat
        ));
    }

    #[test]
    fn verification_bundle_rejects_unknown_version_byte() {
        let (_, _, mut hex_string, _) = deterministic_bundle();
        hex_string.replace_range(0..2, "02");

        let error =
            ProtocolKey::<GenesisVerificationKeyBundle>::from_bytes_hex(&hex_string).unwrap_err();

        assert!(matches!(
            downcast_bundle_error(&error),
            GenesisBundleError::UnsupportedVersion { version: 2 }
        ));
    }

    #[test]
    fn verification_bundle_rejects_length_prefix_mismatch() {
        let (_, _, mut hex_string, _) = deterministic_bundle();
        hex_string.replace_range(2..4, "21");

        let error =
            ProtocolKey::<GenesisVerificationKeyBundle>::from_bytes_hex(&hex_string).unwrap_err();

        assert!(matches!(
            downcast_bundle_error(&error),
            GenesisBundleError::LengthPrefixMismatch {
                field_kind: "Verification-key Ed25519",
                expected: 32,
                actual: 0x21,
            }
        ));
    }

    #[test]
    fn verification_bundle_rejects_trailing_bytes() {
        let (_, _, mut hex_string, _) = deterministic_bundle();
        hex_string.push_str("aa");

        let error =
            ProtocolKey::<GenesisVerificationKeyBundle>::from_bytes_hex(&hex_string).unwrap_err();

        assert!(matches!(
            downcast_bundle_error(&error),
            GenesisBundleError::TrailingBytes { extra: 1 }
        ));
    }

    #[test]
    fn signing_bundle_rejects_legacy_input_with_typed_variant() {
        let signer = GenesisEd25519Signer::create_deterministic_signer();
        let legacy_hex = signer.secret_key().to_json_hex().unwrap();

        let error = GenesisSigningKeyBundle::try_from_hex(&legacy_hex).unwrap_err();

        assert!(matches!(
            downcast_bundle_error(&error),
            GenesisBundleError::LegacySigningKey
        ));
    }

    #[test]
    fn signing_bundle_rejects_unknown_version_byte() {
        let (_, _, _, mut hex_string) = deterministic_bundle();
        hex_string.replace_range(0..2, "02");

        let error =
            ProtocolKey::<GenesisSigningKeyBundle>::from_bytes_hex(&hex_string).unwrap_err();

        assert!(matches!(
            downcast_bundle_error(&error),
            GenesisBundleError::UnsupportedVersion { version: 2 }
        ));
    }

    #[test]
    fn signing_bundle_rejects_trailing_bytes() {
        let (_, _, _, mut hex_string) = deterministic_bundle();
        hex_string.push_str("aa");

        let error =
            ProtocolKey::<GenesisSigningKeyBundle>::from_bytes_hex(&hex_string).unwrap_err();

        assert!(matches!(
            downcast_bundle_error(&error),
            GenesisBundleError::TrailingBytes { extra: 1 }
        ));
    }

    #[test]
    fn ensure_supports_era_lagrange_rejects_legacy_only_bundle() {
        let signer = GenesisEd25519Signer::create_deterministic_signer();
        let bundle = GenesisVerificationKeyBundle {
            ed25519: signer.verification_key(),
            schnorr: None,
        };

        let error = bundle.ensure_supports_era(SupportedEra::Lagrange).unwrap_err();

        assert!(matches!(
            downcast_bundle_error(&error),
            GenesisBundleError::SchnorrVerificationKeyRequired
        ));
    }

    #[test]
    fn ensure_supports_era_pythagoras_accepts_legacy_only_bundle() {
        let signer = GenesisEd25519Signer::create_deterministic_signer();
        let bundle = GenesisVerificationKeyBundle {
            ed25519: signer.verification_key(),
            schnorr: None,
        };

        bundle
            .ensure_supports_era(SupportedEra::Pythagoras)
            .expect("Pythagoras must accept a legacy-only verification bundle");
    }

    #[test]
    fn ensure_supports_era_lagrange_accepts_dual_bundle() {
        let (bundle, _, _, _) = deterministic_bundle();

        bundle
            .ensure_supports_era(SupportedEra::Lagrange)
            .expect("Lagrange must accept a dual verification bundle");
    }

    mod golden {
        use super::*;

        const GOLDEN_VERIFICATION_BUNDLE_HEX: &str = "012020fdbac9b10b7587bba7b5bc163bce69e796d71e4ed44c10fcb4488689f7a1444015867de7be2355322f7e1824372e15176b1a6b22b91ebda768b56d686175e225b79c86887346ea791c6e26c42b353ec4d41f08759d9479e0b854afb44d16310b";

        const GOLDEN_SIGNING_BUNDLE_HEX: &str = "012076b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc7204ad5f9042024299a39096f822693071fa948c62c904cbd2a5b83f5fe351ee404";

        fn golden_verification_bundle() -> GenesisVerificationKeyBundle {
            let mut rng = ChaCha20Rng::from_seed([7u8; 32]);
            let ed25519_signer = GenesisEd25519Signer::create_deterministic_signer();
            let schnorr_signer = GenesisSchnorrSigner::generate(&mut rng);
            GenesisVerificationKeyBundle::new(
                ed25519_signer.verification_key(),
                schnorr_signer.verification_key(),
            )
        }

        fn golden_signing_bundle() -> GenesisSigningKeyBundle {
            let mut rng = ChaCha20Rng::from_seed([7u8; 32]);
            let ed25519_signer = GenesisEd25519Signer::create_deterministic_signer();
            let schnorr_signer = GenesisSchnorrSigner::generate(&mut rng);
            GenesisSigningKeyBundle::new(ed25519_signer.secret_key(), schnorr_signer.secret_key())
        }

        #[test]
        fn verification_bundle_bytes_hex_round_trip_matches_pinned_golden() {
            let bundle = golden_verification_bundle();
            let encoded = ProtocolKey::new(bundle.clone()).to_bytes_hex().unwrap();

            let restored = ProtocolKey::<GenesisVerificationKeyBundle>::from_bytes_hex(&encoded)
                .unwrap()
                .into_inner();

            assert_eq!(encoded, GOLDEN_VERIFICATION_BUNDLE_HEX);
            assert_eq!(bundle.ed25519.as_bytes(), restored.ed25519.as_bytes());
            assert_eq!(
                bundle.schnorr.unwrap().to_bytes(),
                restored.schnorr.unwrap().to_bytes(),
            );
        }

        #[test]
        fn signing_bundle_bytes_hex_round_trip_matches_pinned_golden() {
            let bundle = golden_signing_bundle();
            let encoded = ProtocolKey::new(bundle.clone()).to_bytes_hex().unwrap();

            let restored = ProtocolKey::<GenesisSigningKeyBundle>::from_bytes_hex(&encoded)
                .unwrap()
                .into_inner();

            assert_eq!(encoded, GOLDEN_SIGNING_BUNDLE_HEX);
            assert_eq!(bundle.ed25519.to_bytes(), restored.ed25519.to_bytes());
            assert_eq!(bundle.schnorr.to_bytes(), restored.schnorr.to_bytes());
        }
    }
}
