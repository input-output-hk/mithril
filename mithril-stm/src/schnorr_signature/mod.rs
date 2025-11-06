#![allow(dead_code)]

use midnight_curves::Fq as JubjubBase;
use sha2::{Digest, Sha256};

mod signature;
mod signing_key;
mod verification_key;

/// Convert an arbitrary array of bytes into a Jubjub scalar field element
/// First hash the message to 256 bits use Sha256 then perform the conversion
/// TODO: Handle the unwrap properly
pub fn hash_msg_to_jubjubbase(msg: &[u8]) -> JubjubBase {
    let mut hash = Sha256::new();
    hash.update(msg);
    let hmsg = hash.finalize();
    let mut output = [0u8; 32];
    output.copy_from_slice(&hmsg);
    JubjubBase::from_raw([
        u64::from_le_bytes(output[0..8].try_into().unwrap()),
        u64::from_le_bytes(output[8..16].try_into().unwrap()),
        u64::from_le_bytes(output[16..24].try_into().unwrap()),
        u64::from_le_bytes(output[24..32].try_into().unwrap()),
    ])
}

#[cfg(test)]
mod tests {

    use super::*;
    // Testing conversion from arbitrary message to base field element
    #[test]
    fn test_hash_msg_to_jubjubbase() {
        let msg = vec![0, 0, 0, 1];
        let h = hash_msg_to_jubjubbase(&msg);
        let bytes_le = [
            179, 7, 17, 168, 141, 112, 57, 117, 112, 92, 169, 56, 36, 70, 1, 217, 9, 13, 255, 42,
            100, 207, 166, 110, 188, 47, 35, 211, 35, 168, 100, 25,
        ];
        let field_elem = JubjubBase::from_bytes_le(&bytes_le).unwrap();
        assert_eq!(h, field_elem)
    }
}
