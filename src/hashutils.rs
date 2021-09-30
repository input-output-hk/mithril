use digest::ExtendableOutput;
use sha3::Shake256;
use std::io::Write;
use std::vec::Vec;

pub fn hash_message(msg: &[u8], out_len: usize) -> Vec<u8> {
    let mut hasher = Shake256::default();
    hasher.write(msg).unwrap();
    let out = hasher.finalize_boxed(out_len);
    out.to_vec()
}
