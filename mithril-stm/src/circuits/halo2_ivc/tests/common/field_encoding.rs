use crate::circuits::halo2_ivc::F;

/// Interprets 32 little-endian bytes as a raw Jubjub base-field element.
///
/// This intentionally uses `from_raw` instead of canonical decoding: SHA256
/// digests and stored asset field bytes are reduced modulo the field order.
pub(crate) fn jubjub_base_from_raw_le_bytes(bytes: &[u8]) -> F {
    assert_eq!(bytes.len(), 32);
    F::from_raw([
        u64::from_le_bytes(bytes[0..8].try_into().unwrap()),
        u64::from_le_bytes(bytes[8..16].try_into().unwrap()),
        u64::from_le_bytes(bytes[16..24].try_into().unwrap()),
        u64::from_le_bytes(bytes[24..32].try_into().unwrap()),
    ])
}
