use crate::circuits::halo2_ivc::NativeField;

/// Interprets 32 little-endian bytes as an integer and maps it to a Jubjub
/// base-field element.
///
/// This intentionally uses `from_raw` instead of canonical decoding: SHA256
/// digests and legacy asset/test bytes may be non-canonical, and `from_raw`
/// converts them into the congruent field element modulo the field order.
pub(crate) fn jubjub_base_from_raw_le_bytes(bytes: &[u8]) -> NativeField {
    assert_eq!(bytes.len(), 32);
    NativeField::from_raw([
        u64::from_le_bytes(bytes[0..8].try_into().unwrap()),
        u64::from_le_bytes(bytes[8..16].try_into().unwrap()),
        u64::from_le_bytes(bytes[16..24].try_into().unwrap()),
        u64::from_le_bytes(bytes[24..32].try_into().unwrap()),
    ])
}
