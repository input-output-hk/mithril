//! Versioned envelope carrying the dual genesis signature (Ed25519 + Schnorr) produced by the
//! offline sign ceremony under the Lagrange era.

#![allow(dead_code)]

use mithril_common::StdResult;
use mithril_common::crypto_helper::{GenesisEd25519Signature, GenesisSchnorrSignature};
use thiserror::Error;

/// Current envelope version.
pub const GENESIS_SIGNED_PAYLOAD_VERSION: u8 = 1;

/// Expected raw Ed25519 signature byte length.
pub const ED25519_SIGNATURE_BYTES: u8 = 64;

/// Expected raw Schnorr signature byte length.
pub const SCHNORR_SIGNATURE_BYTES: u8 = 64;

/// Errors raised when parsing or serialising a dual genesis signed payload.
#[derive(Error, Debug)]
pub enum GenesisSignedPayloadError {
    /// The envelope declares a version other than [GENESIS_SIGNED_PAYLOAD_VERSION].
    #[error("unsupported genesis signed payload version: {version} (only version 1 is supported)")]
    UnsupportedVersion {
        /// Version byte read from the envelope.
        version: u8,
    },

    /// A length-prefix field in the envelope does not match its expected fixed value.
    #[error("{field_kind} length prefix mismatch: expected {expected} bytes, got {actual}")]
    LengthPrefixMismatch {
        /// Field name (`Ed25519` or `Schnorr`) prefixed with signature kind.
        field_kind: &'static str,
        /// Expected byte length pinned by [GENESIS_SIGNED_PAYLOAD_VERSION].
        expected: u8,
        /// Length actually declared by the envelope.
        actual: u8,
    },

    /// The envelope decoded to fewer bytes than the layout requires.
    #[error("genesis signed payload truncated: missing {field}")]
    Truncated {
        /// Layout segment that ran short.
        field: &'static str,
    },

    /// The envelope decoded to more bytes than the layout consumes.
    #[error("trailing bytes in genesis signed payload ({extra} extra bytes)")]
    TrailingBytes {
        /// Number of unconsumed bytes after the envelope layout.
        extra: usize,
    },
}

/// Dual genesis signed payload: a legacy Ed25519 signature paired with a SNARK-friendly Schnorr
/// signature.
#[derive(Debug, Clone)]
pub struct GenesisSignedPayload {
    /// Ed25519 signature produced by the ed25519 signer.
    pub ed25519: GenesisEd25519Signature,

    /// Schnorr signature produced by the SNARK signer.
    pub schnorr: GenesisSchnorrSignature,
}

impl GenesisSignedPayload {
    /// Build a fresh envelope from the two signatures.
    pub fn new(ed25519: GenesisEd25519Signature, schnorr: GenesisSchnorrSignature) -> Self {
        Self { ed25519, schnorr }
    }

    /// Encode the envelope as raw bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(3 + 64 + 64);
        bytes.push(GENESIS_SIGNED_PAYLOAD_VERSION);
        bytes.push(ED25519_SIGNATURE_BYTES);
        bytes.extend_from_slice(&self.ed25519.to_bytes());
        bytes.push(SCHNORR_SIGNATURE_BYTES);
        bytes.extend_from_slice(&self.schnorr.to_bytes());
        bytes
    }

    /// Decode an envelope from raw bytes.
    pub fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
        let mut cursor = bytes;
        let version = read_u8(&mut cursor, "version")?;
        if version != GENESIS_SIGNED_PAYLOAD_VERSION {
            return Err(GenesisSignedPayloadError::UnsupportedVersion { version }.into());
        }
        let ed25519_bytes = read_length_prefixed(
            &mut cursor,
            "Ed25519 signature",
            ED25519_SIGNATURE_BYTES,
            "ed25519 signature body",
        )?;
        let schnorr_bytes = read_length_prefixed(
            &mut cursor,
            "Schnorr signature",
            SCHNORR_SIGNATURE_BYTES,
            "schnorr signature body",
        )?;
        if !cursor.is_empty() {
            return Err(GenesisSignedPayloadError::TrailingBytes {
                extra: cursor.len(),
            }
            .into());
        }
        let ed25519 = GenesisEd25519Signature::from_bytes(ed25519_bytes)?;
        let schnorr = GenesisSchnorrSignature::from_bytes(schnorr_bytes)?;
        Ok(Self { ed25519, schnorr })
    }
}

fn read_u8(cursor: &mut &[u8], field: &'static str) -> StdResult<u8> {
    match cursor.split_first() {
        Some((byte, rest)) => {
            *cursor = rest;
            Ok(*byte)
        }
        None => Err(GenesisSignedPayloadError::Truncated { field }.into()),
    }
}

fn read_length_prefixed<'a>(
    cursor: &mut &'a [u8],
    field_kind: &'static str,
    expected: u8,
    body_field: &'static str,
) -> StdResult<&'a [u8]> {
    let actual = read_u8(cursor, "length prefix")?;
    if actual != expected {
        return Err(GenesisSignedPayloadError::LengthPrefixMismatch {
            field_kind,
            expected,
            actual,
        }
        .into());
    }
    if cursor.len() < expected as usize {
        return Err(GenesisSignedPayloadError::Truncated { field: body_field }.into());
    }
    let (head, tail) = cursor.split_at(expected as usize);
    *cursor = tail;
    Ok(head)
}

#[cfg(test)]
mod tests {
    use mithril_common::crypto_helper::{GenesisEd25519Signer, GenesisSchnorrSigner};

    use super::*;

    fn deterministic_payload() -> GenesisSignedPayload {
        let ed_signer = GenesisEd25519Signer::create_deterministic_signer();
        let ed25519 = ed_signer.sign(b"genesis-signed-message");

        let schnorr_signer = GenesisSchnorrSigner::create_non_deterministic_signer();
        let schnorr = schnorr_signer.sign_non_deterministic(&[0x42u8; 32]).unwrap();

        GenesisSignedPayload::new(ed25519, schnorr)
    }

    #[test]
    fn round_trips_through_bytes() {
        let payload = deterministic_payload();
        let bytes = payload.to_bytes();

        let restored = GenesisSignedPayload::try_from_bytes(&bytes).unwrap();

        assert_eq!(restored.ed25519.to_bytes(), payload.ed25519.to_bytes());
        assert_eq!(restored.schnorr.to_bytes(), payload.schnorr.to_bytes());
    }

    #[test]
    fn rejects_unsupported_version() {
        let mut bytes = deterministic_payload().to_bytes();
        bytes[0] = 2;

        let error = GenesisSignedPayload::try_from_bytes(&bytes).unwrap_err();

        assert!(matches!(
            error.downcast_ref::<GenesisSignedPayloadError>(),
            Some(GenesisSignedPayloadError::UnsupportedVersion { version: 2 })
        ));
    }

    #[test]
    fn rejects_length_prefix_mismatch() {
        let mut bytes = deterministic_payload().to_bytes();
        bytes[1] = 32;

        let error = GenesisSignedPayload::try_from_bytes(&bytes).unwrap_err();

        assert!(matches!(
            error.downcast_ref::<GenesisSignedPayloadError>(),
            Some(GenesisSignedPayloadError::LengthPrefixMismatch {
                field_kind: "Ed25519 signature",
                expected: 64,
                actual: 32
            })
        ));
    }

    #[test]
    fn rejects_trailing_bytes() {
        let mut bytes = deterministic_payload().to_bytes();
        bytes.push(0xAA);

        let error = GenesisSignedPayload::try_from_bytes(&bytes).unwrap_err();

        assert!(matches!(
            error.downcast_ref::<GenesisSignedPayloadError>(),
            Some(GenesisSignedPayloadError::TrailingBytes { extra: 1 })
        ));
    }

    #[test]
    fn rejects_truncated_envelope() {
        let bytes = deterministic_payload().to_bytes();
        let truncated = &bytes[..2];

        let error = GenesisSignedPayload::try_from_bytes(truncated).unwrap_err();

        assert!(matches!(
            error.downcast_ref::<GenesisSignedPayloadError>(),
            Some(GenesisSignedPayloadError::Truncated { .. })
        ));
    }

    mod golden {
        use super::*;

        const GOLDEN_MESSAGE: &[u8] = b"genesis-signed-message";

        const GOLDEN_ED25519_HEX: &str = "1191eb0dcad9ad33b5f7b4af635090caa35413d24ff714c828d91d616665541d108b104a594a04c59b8b097aba6a74b900eb082f527d656681fca4e708cc2d08";

        const GOLDEN_SCHNORR_HEX: &str = "35b28d59bd557a7a2d8ae3c72683d4b6e77aa1d5d978c515b3fe3bb42466a20cc4837e0dd0861abae347b0aa7b56477cd9368b3a351c3f60623698d2f4ab5e68";

        const GOLDEN_BYTES: &[u8; 297] = &[
            1, 162, 103, 101, 100, 50, 53, 53, 49, 57, 120, 128, 49, 49, 57, 49, 101, 98, 48, 100,
            99, 97, 100, 57, 97, 100, 51, 51, 98, 53, 102, 55, 98, 52, 97, 102, 54, 51, 53, 48, 57,
            48, 99, 97, 97, 51, 53, 52, 49, 51, 100, 50, 52, 102, 102, 55, 49, 52, 99, 56, 50, 56,
            100, 57, 49, 100, 54, 49, 54, 54, 54, 53, 53, 52, 49, 100, 49, 48, 56, 98, 49, 48, 52,
            97, 53, 57, 52, 97, 48, 52, 99, 53, 57, 98, 56, 98, 48, 57, 55, 97, 98, 97, 54, 97, 55,
            52, 98, 57, 48, 48, 101, 98, 48, 56, 50, 102, 53, 50, 55, 100, 54, 53, 54, 54, 56, 49,
            102, 99, 97, 52, 101, 55, 48, 56, 99, 99, 50, 100, 48, 56, 103, 115, 99, 104, 110, 111,
            114, 114, 162, 104, 114, 101, 115, 112, 111, 110, 115, 101, 152, 32, 24, 53, 24, 178,
            24, 141, 24, 89, 24, 189, 24, 85, 24, 122, 24, 122, 24, 45, 24, 138, 24, 227, 24, 199,
            24, 38, 24, 131, 24, 212, 24, 182, 24, 231, 24, 122, 24, 161, 24, 213, 24, 217, 24,
            120, 24, 197, 21, 24, 179, 24, 254, 24, 59, 24, 180, 24, 36, 24, 102, 24, 162, 12, 105,
            99, 104, 97, 108, 108, 101, 110, 103, 101, 152, 32, 24, 196, 24, 131, 24, 126, 13, 24,
            208, 24, 134, 24, 26, 24, 186, 24, 227, 24, 71, 24, 176, 24, 170, 24, 123, 24, 86, 24,
            71, 24, 124, 24, 217, 24, 54, 24, 139, 24, 58, 24, 53, 24, 28, 24, 63, 24, 96, 24, 98,
            24, 54, 24, 152, 24, 210, 24, 244, 24, 171, 24, 94, 24, 104,
        ];

        #[test]
        fn golden_bytes_decode_to_expected_signatures() {
            let payload = GenesisSignedPayload::try_from_bytes(GOLDEN_BYTES)
                .expect("Golden genesis signed payload should decode");

            assert_eq!(GOLDEN_ED25519_HEX, hex::encode(payload.ed25519.to_bytes()));
            assert_eq!(GOLDEN_SCHNORR_HEX, hex::encode(payload.schnorr.to_bytes()));
        }

        #[test]
        fn golden_encoding_is_stable() {
            let payload = GenesisSignedPayload::try_from_bytes(GOLDEN_BYTES)
                .expect("Golden genesis signed payload should decode");
            let reencoded = payload.to_bytes().expect("Golden payload should re-encode");

            assert_eq!(GOLDEN_BYTES.as_slice(), reencoded.as_slice());
        }

        #[test]
        fn golden_ed25519_signature_is_reproducible() {
            let ed25519 = GenesisEd25519Signer::create_deterministic_signer().sign(GOLDEN_MESSAGE);

            assert_eq!(GOLDEN_ED25519_HEX, hex::encode(ed25519.to_bytes()));
        }
    }
}
