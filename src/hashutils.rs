use blake2::VarBlake2b;
use digest::{Update, VariableOutput};
use std::vec::Vec;

pub fn hash_message(msg: &[u8], out_len: usize) -> Vec<u8> {
    let mut dest = vec![0; out_len];
    let mut hasher: VarBlake2b = VariableOutput::new(out_len).unwrap();

    hasher.update(msg);
    hasher.finalize_variable(|out| {
        dest.copy_from_slice(out);
    });

    dest
}
