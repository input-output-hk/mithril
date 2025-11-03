use crate::snark_friendly::*;

pub struct MerkleTree<D: Digest> {
    _marker: std::marker::PhantomData<D>,
}
