//! CBOR codec utilities for evolvable binary serialization.
//!
//! This module provides versioned CBOR encoding/decoding functions that allow
//! schema evolution without introducing breaking changes. The encoding uses a
//! version byte prefix to distinguish between legacy (manual big-endian byte
//! packing) and CBOR-encoded payloads.

use anyhow::Context;
use serde::{Serialize, de::DeserializeOwned};

use crate::StmResult;

/// Version byte indicating CBOR v1 encoding.
pub const CODEC_VERSION_CBOR_V1: u8 = 1;

/// Serialize a value to CBOR bytes with a version byte prefix.
///
/// The output format is: `[CODEC_VERSION_CBOR_V1] [cbor_payload...]`
pub fn to_cbor_bytes<T: Serialize>(value: &T) -> StmResult<Vec<u8>> {
    let mut output = vec![CODEC_VERSION_CBOR_V1];
    ciborium::ser::into_writer(value, &mut output).context("Failed to serialize value to CBOR")?;
    Ok(output)
}

/// Deserialize a value from CBOR bytes (without version prefix).
///
/// This function expects raw CBOR bytes with no version prefix.
pub fn from_cbor_bytes<T: DeserializeOwned>(bytes: &[u8]) -> StmResult<T> {
    ciborium::de::from_reader(bytes).context("Failed to deserialize value from CBOR")
}

/// Check whether the given bytes start with the CBOR v1 version prefix.
pub fn has_cbor_v1_prefix(bytes: &[u8]) -> bool {
    bytes.first() == Some(&CODEC_VERSION_CBOR_V1)
}

/// Deserialize a value from versioned bytes.
///
/// If the first byte is `CODEC_VERSION_CBOR_V1`, the remaining bytes are decoded as CBOR.
/// Otherwise, the `legacy_decoder` is called to handle legacy format.
pub fn from_versioned_bytes<T: DeserializeOwned>(
    bytes: &[u8],
    legacy_decoder: impl FnOnce(&[u8]) -> StmResult<T>,
) -> StmResult<T> {
    if has_cbor_v1_prefix(bytes) {
        from_cbor_bytes(&bytes[1..])
    } else {
        legacy_decoder(bytes)
    }
}

#[cfg(test)]
mod tests {
    use serde::Deserialize;

    use super::*;

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    struct SampleStruct {
        field_a: u64,
        field_b: String,
    }

    #[test]
    fn cbor_roundtrip() {
        let value = SampleStruct {
            field_a: 42,
            field_b: "hello".to_string(),
        };

        let bytes = to_cbor_bytes(&value).expect("Serialization should not fail");
        assert_eq!(bytes[0], CODEC_VERSION_CBOR_V1);

        let decoded: SampleStruct =
            from_cbor_bytes(&bytes[1..]).expect("Deserialization should not fail");
        assert_eq!(value, decoded);
    }

    #[test]
    fn versioned_bytes_dispatches_to_cbor_for_version_1() {
        let value = SampleStruct {
            field_a: 99,
            field_b: "cbor".to_string(),
        };

        let bytes = to_cbor_bytes(&value).expect("Serialization should not fail");

        let decoded: SampleStruct =
            from_versioned_bytes(&bytes, |_| panic!("Legacy decoder should not be called"))
                .expect("Deserialization should not fail");
        assert_eq!(value, decoded);
    }

    #[test]
    fn versioned_bytes_dispatches_to_legacy_for_non_cbor_v1_prefix() {
        let legacy_bytes = vec![0, 0, 0, 0, 0, 0, 0, 42];
        let expected = SampleStruct {
            field_a: 42,
            field_b: "legacy".to_string(),
        };
        let expected_clone = expected.clone();

        let decoded: SampleStruct = from_versioned_bytes(&legacy_bytes, |_| Ok(expected_clone))
            .expect("Legacy decoding should not fail");
        assert_eq!(expected, decoded);
    }

    #[test]
    fn has_cbor_v1_prefix_detects_version_byte() {
        assert!(has_cbor_v1_prefix(&[CODEC_VERSION_CBOR_V1, 0, 0]));
        assert!(!has_cbor_v1_prefix(&[0, 0, 0]));
        assert!(!has_cbor_v1_prefix(&[2, 0, 0]));
        assert!(!has_cbor_v1_prefix(&[]));
    }

    #[test]
    fn forward_compatible_with_extra_fields() {
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        struct EvolvedSampleStruct {
            field_a: u64,
            field_b: String,
            field_c: Option<bool>,
        }

        let evolved = EvolvedSampleStruct {
            field_a: 42,
            field_b: "hello".to_string(),
            field_c: Some(true),
        };
        let bytes = to_cbor_bytes(&evolved).expect("Serialization should not fail");

        let decoded: SampleStruct =
            from_versioned_bytes(&bytes, |_| panic!("Legacy decoder should not be called"))
                .expect("Decoding should succeed ignoring extra field");
        assert_eq!(decoded.field_a, 42);
        assert_eq!(decoded.field_b, "hello");
    }

    #[test]
    fn backward_compatible_with_missing_optional_fields() {
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        struct EvolvedSampleStruct {
            field_a: u64,
            field_b: String,
            #[serde(default)]
            field_c: Option<bool>,
        }

        let original = SampleStruct {
            field_a: 42,
            field_b: "hello".to_string(),
        };
        let bytes = to_cbor_bytes(&original).expect("Serialization should not fail");

        let decoded: EvolvedSampleStruct =
            from_versioned_bytes(&bytes, |_| panic!("Legacy decoder should not be called"))
                .expect("Decoding should succeed with default for missing field");
        assert_eq!(decoded.field_a, 42);
        assert_eq!(decoded.field_b, "hello");
        assert_eq!(decoded.field_c, None);
    }

    #[test]
    fn from_cbor_bytes_fails_on_corrupted_input() {
        let corrupted = [0xFF, 0xFF, 0x00];
        from_cbor_bytes::<SampleStruct>(&corrupted)
            .expect_err("Corrupted CBOR should fail to deserialize");
    }

    #[test]
    fn from_cbor_bytes_fails_on_empty_input() {
        from_cbor_bytes::<SampleStruct>(&[]).expect_err("Empty CBOR should fail to deserialize");
    }

    #[test]
    fn versioned_bytes_dispatches_to_legacy_for_empty_input() {
        let result: SampleStruct = from_versioned_bytes(&[], |_| {
            Ok(SampleStruct {
                field_a: 0,
                field_b: "empty".to_string(),
            })
        })
        .expect("Empty input should dispatch to legacy decoder");
        assert_eq!(result.field_b, "empty");
    }
}
