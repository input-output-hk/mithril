pub trait Digest {
    fn digest(data: &[u8]) -> Vec<u8>;
}

pub struct MerkleTree<D: Digest> {
    _marker: std::marker::PhantomData<D>,
}
