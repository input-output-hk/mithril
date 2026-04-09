use serde::{Deserialize, Serialize};

use crate::codec;
use crate::{PhiFValue, StmResult};

use super::RegisterError;

/// Used to set protocol parameters.
// todo: this is the criteria to consider parameters valid:
// Let A = max assumed adversarial stake
// Let a = A / max_stake
// Let p = φ(a)  // f needs tuning, something close to 0.2 is reasonable
// Then, we're secure if SUM[from i=k to i=m] Binomial(i successes, m experiments, p chance of success) <= 2^-100 or thereabouts.
// The latter turns to 1 - BinomialCDF(k-1,m,p)
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Parameters {
    /// Security parameter, upper bound on indices.
    pub m: u64,
    /// Quorum parameter.
    pub k: u64,
    /// `f` in phi(w) = 1 - (1 - f)^w, where w is the stake of a participant..
    pub phi_f: PhiFValue,
}

impl Parameters {
    /// Convert to CBOR bytes with a version prefix.
    ///
    /// Legacy readers that encounter the version prefix byte will fail
    /// gracefully, while new readers will decode the CBOR payload.
    pub fn to_bytes(&self) -> StmResult<Vec<u8>> {
        codec::to_cbor_bytes(self)
    }

    /// Extract the `Parameters` from a byte slice.
    ///
    /// Supports both the legacy big-endian byte format and the new
    /// versioned CBOR format.
    ///
    /// # Error
    /// The function fails if the given string of bytes is not of required size.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        codec::from_versioned_bytes(bytes, Self::from_bytes_legacy)
    }

    fn from_bytes_legacy(bytes: &[u8]) -> StmResult<Self> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(bytes.get(..8).ok_or(RegisterError::SerializationError)?);
        let m = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(bytes.get(8..16).ok_or(RegisterError::SerializationError)?);
        let k = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(bytes.get(16..24).ok_or(RegisterError::SerializationError)?);
        let phi_f = f64::from_be_bytes(u64_bytes);

        Ok(Self { m, k, phi_f })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod golden {
        use super::*;

        const GOLDEN_JSON: &str = r#"
            {
                "m": 20973,
                "k": 2422,
                "phi_f": 0.2
            }
        "#;

        fn golden_value() -> Parameters {
            Parameters {
                m: 20973,
                k: 2422,
                phi_f: 0.2,
            }
        }

        #[test]
        fn golden_conversions() {
            let value = serde_json::from_str(GOLDEN_JSON)
                .expect("This JSON deserialization should not fail");
            assert_eq!(golden_value(), value);

            let serialized =
                serde_json::to_string(&value).expect("This JSON serialization should not fail");
            let golden_serialized = serde_json::to_string(&golden_value())
                .expect("This JSON serialization should not fail");
            assert_eq!(golden_serialized, serialized);
        }
    }

    mod cbor {
        use super::*;

        const LEGACY_BYTES: &[u8; 24] = &[
            0, 0, 0, 0, 0, 0, 81, 237, 0, 0, 0, 0, 0, 0, 9, 118, 63, 201, 153, 153, 153, 153, 153,
            154,
        ];

        fn test_value() -> Parameters {
            Parameters {
                m: 20973,
                k: 2422,
                phi_f: 0.2,
            }
        }

        #[test]
        fn cbor_roundtrip() {
            let value = test_value();
            let bytes = value.to_bytes().expect("CBOR serialization should not fail");
            let decoded =
                Parameters::from_bytes(&bytes).expect("CBOR deserialization should not fail");
            assert_eq!(value, decoded);
        }

        #[test]
        fn legacy_bytes_can_still_be_decoded() {
            let decoded = Parameters::from_bytes(LEGACY_BYTES)
                .expect("Legacy deserialization should not fail");
            assert_eq!(test_value(), decoded);
        }

        const GOLDEN_CBOR_BYTES: &[u8; 27] = &[
            1, 163, 97, 109, 25, 81, 237, 97, 107, 25, 9, 118, 101, 112, 104, 105, 95, 102, 251,
            63, 201, 153, 153, 153, 153, 153, 154,
        ];

        #[test]
        fn cbor_golden_bytes_can_be_decoded() {
            let decoded = Parameters::from_bytes(GOLDEN_CBOR_BYTES)
                .expect("CBOR golden bytes deserialization should not fail");
            assert_eq!(test_value(), decoded);
        }

        #[test]
        fn cbor_encoding_is_stable() {
            let bytes = test_value().to_bytes().expect("CBOR serialization should not fail");
            assert_eq!(GOLDEN_CBOR_BYTES.as_slice(), bytes.as_slice());
        }
    }
}
