use ff::PrimeField;

use crate::circuits::halo2::types::JubjubBase;

/// Convert a Jubjub base field element to 32-byte little-endian encoding.
pub(crate) fn jubjub_base_to_le_bytes(x: &JubjubBase) -> [u8; 32] {
    x.to_bytes_le()
}

/// Strictly decode a 32-byte little-endian encoding into a Jubjub base field element.
/// Returns None on non-canonical encodings.
pub(crate) fn jubjub_base_from_le_bytes_strict(bytes: &[u8; 32]) -> Option<JubjubBase> {
    JubjubBase::from_bytes_le(bytes).into_option()
}

/// Convert digest bytes to a Jubjub base field element using strict decoding.
pub(crate) fn digest_bytes_to_base(bytes: &[u8; 32]) -> Option<JubjubBase> {
    jubjub_base_from_le_bytes_strict(bytes)
}

/// Split 32 digest bytes into two Jubjub base field elements using 16-byte limbs.
pub(crate) fn digest_bytes_to_fields(bytes: &[u8; 32]) -> [JubjubBase; 2] {
    let mut lo = [0u8; 16];
    let mut hi = [0u8; 16];
    lo.copy_from_slice(&bytes[0..16]);
    hi.copy_from_slice(&bytes[16..32]);
    let lo_u128 = u128::from_le_bytes(lo);
    let hi_u128 = u128::from_le_bytes(hi);
    [JubjubBase::from_u128(lo_u128), JubjubBase::from_u128(hi_u128)]
}

#[cfg(test)]
mod tests {
    use super::*;
    use ff::Field;
    use ff::PrimeField;

    #[test]
    fn roundtrip_jubjub_base_le_bytes() {
        let values = [JubjubBase::ZERO, JubjubBase::ONE, -JubjubBase::ONE];
        for value in values {
            let bytes = jubjub_base_to_le_bytes(&value);
            let decoded = jubjub_base_from_le_bytes_strict(&bytes)
                .expect("canonical encoding should decode");
            assert_eq!(decoded, value);
        }
    }

    #[test]
    fn strict_decode_rejects_non_canonical() {
        let bytes = [0xFFu8; 32];
        assert!(
            jubjub_base_from_le_bytes_strict(&bytes).is_none(),
            "non-canonical encoding should be rejected"
        );
    }

    #[test]
    fn digest_bytes_to_fields_matches_manual_split() {
        let bytes = [
            1u8, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
            23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
        ];
        let fields = digest_bytes_to_fields(&bytes);
        let lo = u128::from_le_bytes(bytes[0..16].try_into().unwrap());
        let hi = u128::from_le_bytes(bytes[16..32].try_into().unwrap());
        assert_eq!(fields[0], JubjubBase::from_u128(lo));
        assert_eq!(fields[1], JubjubBase::from_u128(hi));
    }
}
