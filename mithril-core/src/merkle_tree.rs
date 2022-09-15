//! Creation and verification of Merkle Trees
use crate::error::MerkleTreeError;
use crate::multi_sig::VerificationKey;
use crate::stm::Stake;
use blake2::digest::Digest;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::convert::TryFrom;
use std::fmt::Debug;
use std::marker::PhantomData;

/// The values that are committed in the Merkle Tree.
/// Namely, a verified `VerificationKey` and its corresponding stake.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct MTLeaf(pub VerificationKey, pub Stake);

/// Path of hashes from root to leaf in a Merkle Tree.
/// Contains all hashes on the path, and the index of the leaf.
/// Used to verify that signatures come from eligible signers.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Path<D: Digest> {
    pub(crate) values: Vec<Vec<u8>>,
    pub(crate) index: usize,
    hasher: PhantomData<D>,
}

/// `MerkleTree` commitment.
/// This structure differs from `MerkleTree` in that it does not contain all elements, which are not always necessary.
/// Instead, it only contains the root of the tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleTreeCommitment<D: Digest> {
    /// Root of the merkle commitment.
    pub root: Vec<u8>,
    hasher: PhantomData<D>,
}

/// Tree of hashes, providing a commitment of data and its ordering.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MerkleTree<D: Digest> {
    /// The nodes are stored in an array heap:
    /// * `nodes[0]` is the root,
    /// * the parent of `nodes[i]` is `nodes[(i-1)/2]`
    /// * the children of `nodes[i]` are `{nodes[2i + 1], nodes[2i + 2]}`
    /// * All nodes have size `Output<D>::output_size()`, even leafs (which are hashed before committing them).
    nodes: Vec<Vec<u8>>,
    /// The leaves begin at `nodes[leaf_off]`.
    leaf_off: usize,
    /// Number of leaves cached in the merkle tree.
    n: usize,
    /// Phantom type to link the tree with its hasher
    hasher: PhantomData<D>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProofList<D: Digest> {
    pub(crate) values: Vec<Vec<u8>>,
    pub(crate) indexes: Vec<usize>,
    hasher: PhantomData<D>,
}

impl<D: Digest + Clone>  ProofList<D> {
    pub fn create (values: Vec<Vec<u8>>, indexes: Vec<usize>)-> Self {
        Self {
            values,
            indexes,
            hasher:PhantomData::default(),
        }
    }

    pub fn match_val_ind (&self, ind: &usize)-> usize {
        let offset = self
            .indexes
            .iter()
            .position(|&x| x.eq(ind))
            .unwrap();
        offset
    }

    /// Convert to bytes
    /// # Layout
    /// * Index representing the position in the Merkle Tree
    /// * Size of the Path
    /// * Path of hashes
    pub fn to_bytes(&self) -> Vec<u8> {

        let mut out = Vec::new();
        out.extend_from_slice(&u64::try_from(self.values.len()).unwrap().to_be_bytes());
        for val in &self.values {
            out.extend_from_slice(val)
        }
        for ind in &self.indexes {
            out.extend_from_slice(&ind.to_be_bytes())
        }
        out
    }

    /// Extract a `Path` from a byte slice.
    /// # Error
    /// This function fails if the bytes cannot retrieve path.
    pub fn from_bytes(bytes: &[u8]) -> Result<ProofList<D>, MerkleTreeError<D>> {

        let mut u64_bytes = [0u8; 8];

        u64_bytes.copy_from_slice(&bytes[..8]);
        let len = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;

        let mut values = Vec::with_capacity(len);
        for i in 0..len {
            values.push(
                bytes[8 + i * <D as Digest>::output_size()
                    ..8 + (i + 1) * <D as Digest>::output_size()]
                    .to_vec(),
            );
        }

        let offset = 8 + len * <D as Digest>::output_size();

        let mut indexes = Vec::with_capacity(len);
        for i in 0..len {
            u64_bytes.copy_from_slice(&bytes[offset + i * 8..offset + (i + 1) * 8]);
            indexes.push(usize::try_from(u64::from_be_bytes(u64_bytes))
                .map_err(|_| MerkleTreeError::SerializationError)?)
        }
        Ok(ProofList {
            values,
            indexes,
            hasher: Default::default(),
        })
    }
}

impl MTLeaf {
    pub(crate) fn to_bytes(self) -> [u8; 104] {
        let mut result = [0u8; 104];
        result[..96].copy_from_slice(&self.0.to_bytes());
        result[96..].copy_from_slice(&self.1.to_be_bytes());
        result
    }
}

impl PartialOrd for MTLeaf {
    /// Ordering of MT Values.
    ///
    /// First we order by stake, then by key. By having this ordering,
    /// we have the players with higher stake close together,
    /// meaning that the probability of having several signatures in the same side of the tree, is higher.
    /// This allows us to produce a more efficient batch opening of the merkle tree.
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.1.cmp(&other.1).then(self.0.cmp(&other.0)))
    }
}

impl Ord for MTLeaf {
    fn cmp(&self, other: &Self) -> Ordering {
        self.1.cmp(&other.1).then(self.0.cmp(&other.0))
    }
}

impl<D: Digest + Clone> Path<D> {
    pub fn create (values: Vec<Vec<u8>>, index: usize)-> Self {
        Self {
            values,
            index,
            hasher:PhantomData::default(),
        }
    }

    /// Convert to bytes
    /// # Layout
    /// * Index representing the position in the Merkle Tree
    /// * Size of the Path
    /// * Path of hashes
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend_from_slice(&u64::try_from(self.index).unwrap().to_be_bytes());
        output.extend_from_slice(&u64::try_from(self.values.len()).unwrap().to_be_bytes());
        for value in &self.values {
            output.extend_from_slice(value)
        }
        output
    }

    /// Extract a `Path` from a byte slice.
    /// # Error
    /// This function fails if the bytes cannot retrieve path.
    pub fn from_bytes(bytes: &[u8]) -> Result<Path<D>, MerkleTreeError<D>> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let index = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;
        u64_bytes.copy_from_slice(&bytes[8..16]);
        let len = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;
        let mut values = Vec::with_capacity(len);
        for i in 0..len {
            values.push(
                bytes[16 + i * <D as Digest>::output_size()
                    ..16 + (i + 1) * <D as Digest>::output_size()]
                    .to_vec(),
            );
        }

        Ok(Path {
            values,
            index,
            hasher: Default::default(),
        })
    }
}

impl<D: Clone + Digest> MerkleTreeCommitment<D> {
    /// Check an inclusion proof that `val` is part of the tree by traveling the whole path until the root.
    /// # Error
    /// If the merkle tree path is invalid, then the function fails.
    pub fn check(&self, val: &MTLeaf, proof: &Path<D>) -> Result<(), MerkleTreeError<D>> {
        let mut idx = proof.index;

        let mut h = D::digest(&val.to_bytes()).to_vec();
        for p in &proof.values {
            if (idx & 0b1) == 0 {
                h = D::new().chain_update(h).chain_update(p).finalize().to_vec();
            } else {
                h = D::new().chain_update(p).chain_update(h).finalize().to_vec();
            }
            idx >>= 1;
        }

        if h == self.root {
            return Ok(());
        }
        Err(MerkleTreeError::PathInvalid(proof.clone()))
    }

    /// Serializes the Merkle Tree commitment together with a message in a single vector of bytes.
    /// Outputs `msg || self` as a vector of bytes.
    pub fn concat_with_msg(&self, msg: &[u8]) -> Vec<u8>
    where
        D: Digest,
    {
        let mut msgp = msg.to_vec();
        let mut bytes = self.root.clone();
        msgp.append(&mut bytes);

        msgp
    }
}

impl<D: Digest> MerkleTree<D> {
    /// Provided a non-empty list of leaves, `create` generates its corresponding `MerkleTree`.
    pub fn create(leaves: &[MTLeaf]) -> MerkleTree<D> {
        let n = leaves.len();
        assert!(n > 0, "MerkleTree::create() called with no leaves");

        let num_nodes = n + n.next_power_of_two() - 1;

        let mut nodes = vec![vec![0u8]; num_nodes];

        for i in 0..leaves.len() {
            nodes[num_nodes - n + i] = D::digest(&leaves[i].to_bytes()).to_vec();
        }

        for i in (0..num_nodes - n).rev() {
            let z = D::digest(&[0u8]).to_vec();
            let left = if left_child(i) < num_nodes {
                &nodes[left_child(i)]
            } else {
                &z
            };
            let right = if right_child(i) < num_nodes {
                &nodes[right_child(i)]
            } else {
                &z
            };
            nodes[i] = D::new()
                .chain_update(left)
                .chain_update(right)
                .finalize()
                .to_vec();
        }

        Self {
            nodes,
            n,
            leaf_off: num_nodes - n,
            hasher: PhantomData::default(),
        }
    }

    /// Convert merkle tree to a commitment. This function simply returns the root.
    pub fn to_commitment(&self) -> MerkleTreeCommitment<D> {
        MerkleTreeCommitment {
            root: self.nodes[0].clone(),
            hasher: self.hasher,
        }
    }

    /// Get the root of the tree.
    pub fn root(&self) -> &Vec<u8> {
        &self.nodes[0]
    }

    /// Get a path (hashes of siblings of the path to the root node)
    /// for the `i`th value stored in the tree.
    /// Requires `i < self.n`
    pub fn get_path(&self, i: usize) -> Path<D> {
        assert!(
            i < self.n,
            "Proof index out of bounds: asked for {} out of {}",
            i,
            self.n
        );
        let mut idx = self.idx_of_leaf(i);
        let mut proof = Vec::new();

        while idx > 0 {
            let h = if sibling(idx) < self.nodes.len() {
                self.nodes[sibling(idx)].clone()
            } else {
                D::digest(&[0u8]).to_vec()
            };
            proof.push(h.clone());
            idx = parent(idx);
        }

        Path {
            values: proof,
            index: i,
            hasher: Default::default(),
        }
    }

    pub fn generate_proof_list(&self, i: usize) -> ProofList<D> {
        assert!(
            i < self.n,
            "Proof index out of bounds: asked for {} out of {}",
            i,
            self.n
        );
        let mut idx = self.idx_of_leaf(i);
        let mut values = Vec::new();
        let mut indexes = Vec::new();

        while idx > 0 {
            let h = if sibling(idx) < self.nodes.len() {
                self.nodes[sibling(idx)].clone()
            } else {
                D::digest(&[0u8]).to_vec()
            };

            values.push(h.clone());
            indexes.push(sibling(idx));
            idx = parent(idx);
        }

        ProofList {
            values,
            indexes,
            hasher: PhantomData::default(),
        }
    }

    pub fn get_mt_index_path(&self, i: usize) -> Vec<usize> {
        assert!(
            i < self.n,
            "Proof index out of bounds: asked for {} out of {}",
            i,
            self.n
        );
        let mut idx = self.idx_of_leaf(i);
        let mut index_path = Vec::new();
        while idx > 0 {
            index_path.push(sibling(idx));
            idx = parent(idx);
        }
        index_path
    }

    /// Return the index of the leaf.
    fn idx_of_leaf(&self, i: usize) -> usize {
        self.leaf_off + i
    }

    /// Convert a `MerkleTree` into a byte string, containing $4 + n * S$ bytes where $n$ is the
    /// number of nodes and $S$ the output size of the hash function.
    /// # Layout
    /// * Number of leaves committed in the Merkle Tree (as u64)
    /// * All nodes of the merkle tree (starting with the root)
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(8 + self.nodes.len() * <D as Digest>::output_size());
        result.extend_from_slice(&u64::try_from(self.n).unwrap().to_be_bytes());
        for node in self.nodes.iter() {
            result.extend_from_slice(node);
        }
        result
    }

    /// Try to convert a byte string into a `MerkleTree`.
    /// # Error
    /// It returns error if conversion fails.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MerkleTreeError<D>> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let n = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;
        let num_nodes = n + n.next_power_of_two() - 1;
        let mut nodes = Vec::with_capacity(num_nodes);
        for i in 0..num_nodes {
            nodes.push(
                bytes[8 + i * <D as Digest>::output_size()
                    ..8 + (i + 1) * <D as Digest>::output_size()]
                    .to_vec(),
            );
        }
        Ok(Self {
            nodes,
            leaf_off: num_nodes - n,
            n,
            hasher: PhantomData::default(),
        })
    }
}

//////////////////
// Heap Helpers //
//////////////////

fn parent(i: usize) -> usize {
    assert!(i > 0, "The root node does not have a parent");
    (i - 1) / 2
}

fn left_child(i: usize) -> usize {
    (2 * i) + 1
}

fn right_child(i: usize) -> usize {
    (2 * i) + 2
}

fn sibling(i: usize) -> usize {
    assert!(i > 0, "The root node does not have a sibling");
    // In the heap representation, the left sibling is always odd
    // And the right sibling is the next node
    // We're assuming that the heap is complete
    if i % 2 == 1 {
        i + 1
    } else {
        i - 1
    }
}

/////////////////////
// Testing         //
/////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use blake2::{digest::consts::U32, Blake2b};
    use proptest::collection::vec;
    use proptest::prelude::*;

    prop_compose! {
        fn arb_tree(max_size: u32)
                   (v in vec(any::<u64>(), 2..max_size as usize)) -> (MerkleTree<Blake2b<U32>>, Vec<MTLeaf>) {
            let pks = vec![VerificationKey::default(); v.len()];
            let leaves = pks.into_iter().zip(v.into_iter()).map(|(key, stake)| MTLeaf(key, stake)).collect::<Vec<MTLeaf>>();
             (MerkleTree::<Blake2b<U32>>::create(&leaves), leaves)
        }
    }

    proptest! {
        // Test the relation that t.get_path(i) is a valid
        // proof for i
        #![proptest_config(ProptestConfig::with_cases(100))]
        #[test]
        fn test_create_proof((t, values) in arb_tree(30)) {
            values.iter().enumerate().for_each(|(i, _v)| {
                let pf = t.get_path(i);
                assert!(t.to_commitment().check(&values[i], &pf).is_ok());
            })
        }

        #[test]
        fn test_bytes_path((t, values) in arb_tree(30)) {
            values.iter().enumerate().for_each(|(i, _v)| {
                let pf = t.get_path(i);
                let bytes = pf.to_bytes();
                let deserialised = Path::from_bytes(&bytes).unwrap();
                assert!(t.to_commitment().check(&values[i], &deserialised).is_ok());

                let encoded = bincode::serialize(&pf).unwrap();
                let decoded: Path<Blake2b<U32>> = bincode::deserialize(&encoded).unwrap();
                assert!(t.to_commitment().check(&values[i], &decoded).is_ok());
            })
        }

        #[test]
        fn test_bytes_tree((t, values) in arb_tree(5)) {
            let bytes = t.to_bytes();
            let deserialised = MerkleTree::<Blake2b<U32>>::from_bytes(&bytes).unwrap();
            let tree = MerkleTree::<Blake2b<U32>>::create(&values);
            assert_eq!(tree.nodes, deserialised.nodes);

            let encoded = bincode::serialize(&t).unwrap();
            let decoded: MerkleTree::<Blake2b<U32>> = bincode::deserialize(&encoded).unwrap();
            assert_eq!(tree.nodes, decoded.nodes);
        }

        #[test]
        fn test_bytes_tree_commitment((t, values) in arb_tree(5)) {
            let encoded = bincode::serialize(&t.to_commitment()).unwrap();
            let decoded: MerkleTreeCommitment::<Blake2b<U32>> = bincode::deserialize(&encoded).unwrap();
            let tree_commitment = MerkleTree::<Blake2b<U32>>::create(&values).to_commitment();
            assert_eq!(tree_commitment.root, decoded.root);
        }
    }

    fn pow2_plus1(h: usize) -> usize {
        1 + 2_usize.pow(h as u32)
    }

    prop_compose! {
        // Returns values with a randomly generated path
        fn values_with_invalid_proof(max_height: usize)
                                    (h in 1..max_height)
                                    (v in vec(any::<u64>(), 2..pow2_plus1(h)),
                                     proof in vec(vec(any::<u8>(), 16), h)) -> (Vec<MTLeaf>, Vec<Vec<u8>>) {
            let pks = vec![VerificationKey::default(); v.len()];
            let leaves = pks.into_iter().zip(v.into_iter()).map(|(key, stake)| MTLeaf(key, stake)).collect::<Vec<MTLeaf>>();
            (leaves, proof)
        }
    }

    proptest! {
        #[test]
        fn test_create_invalid_proof(
            i in any::<usize>(),
            (values, proof) in values_with_invalid_proof(10)
        ) {
            let t = MerkleTree::<Blake2b<U32>>::create(&values[1..]);
            let index = i % (values.len() - 1);
            let path = Path{values: proof
                            .iter()
                            .map(|x|  Blake2b::<U32>::digest(x).to_vec())
                            .collect(),
                index,
                hasher: PhantomData::<Blake2b<U32>>::default()
                };
            assert!(t.to_commitment().check(&values[0], &path).is_err());
        }
    }
}
