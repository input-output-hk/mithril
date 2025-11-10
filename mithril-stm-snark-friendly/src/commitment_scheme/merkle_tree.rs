use crate::*;

pub struct MerkleTree<D: Digest> {
    _marker: std::marker::PhantomData<D>,
}
