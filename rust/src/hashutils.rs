use blake2::digest::{Update, VariableOutput};
use blake2::VarBlake2b;
use std::vec::Vec;

pub fn hash_message(msg: &[u8], out_len: usize) -> Vec<u8> {
    let mut hasher = VarBlake2b::new(out_len).unwrap();
    hasher.update(msg);
    let mut out = vec![0u8; out_len];
    hasher.finalize_variable(|res| out.copy_from_slice(res));
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn size_test() {
        let message = b"testing if lengths bigger than 300 bits work";
        let hash = hash_message(message, 42);

        assert_eq!(
            hash,
            [
                210, 197, 244, 136, 108, 49, 176, 109, 118, 254, 196, 130, 213, 133, 151, 78, 137,
                125, 87, 113, 139, 149, 190, 80, 184, 115, 149, 139, 125, 186, 70, 170, 209, 237,
                78, 168, 192, 58, 120, 189, 83, 232
            ]
        )
    }
}
