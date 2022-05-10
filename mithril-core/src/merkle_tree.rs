//! Creation and verification of Merkle Trees
use crate::error::MerkleTreeError;
use digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::fmt::Debug;
use std::marker::PhantomData;

/// Path of hashes from root to leaf in a Merkle Tree. Contains all hashes on the path, and the index
/// of the leaf.
/// Used to verify the credentials of users and signatures.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Path<D: Digest + FixedOutput> {
    pub(crate) values: Vec<Vec<u8>>,
    pub(crate) index: usize,
    hasher: PhantomData<D>,
}

/// MerkleTree commitment. This structure differs from `MerkleTree` in that it does not contain
/// all elements, which are not always necessary.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleTreeCommitment<D: Digest + FixedOutput> {
    /// Root of the merkle commitment
    pub root: Vec<u8>,
    /// Number of leaves committed in the commitment
    pub nr_leaves: usize,
    hasher: PhantomData<D>,
}

impl<D> MerkleTreeCommitment<D>
where
    D: Digest + FixedOutput,
{
    /// Check an inclusion proof that `val` is part of the tree by traveling the whole path
    /// until the root.
    ///
    /// # Error
    /// Returns an error if the path is invalid.
    ///
    /// # Example
    /// ```
    /// # use rand_core::{OsRng, RngCore};
    /// # use mithril::merkle_tree::MerkleTree;
    /// # use blake2::Blake2b;
    /// # fn main() {
    /// let mut rng = OsRng::default();
    /// // We generate the keys.
    /// let mut keys = Vec::with_capacity(32);
    /// for _ in 0..32 {
    ///     let mut leaf = [0u8; 32];
    ///     rng.fill_bytes(&mut leaf);
    ///     keys.push(leaf.to_vec());
    /// }
    /// // Compute the Merkle tree of the keys.
    /// let mt = MerkleTree::<Blake2b>::create(&keys);
    /// // Compute the path of key in position 3.
    /// let path = mt.get_path(3);
    /// // Verify the proof of membership with respect to the merkle commitment.
    /// assert!(mt.to_commitment().check(&keys[3], &path).is_ok());
    ///
    /// # }
    pub fn check(&self, val: &[u8], proof: &Path<D>) -> Result<(), MerkleTreeError> {
        let mut idx = proof.index;

        let mut h = D::digest(val).to_vec();
        for p in &proof.values {
            if (idx & 0b1) == 0 {
                h = D::new().chain(h).chain(p).finalize().to_vec();
            } else {
                h = D::new().chain(p).chain(h).finalize().to_vec();
            }
            idx >>= 1;
        }

        if h == self.root {
            return Ok(());
        }
        Err(MerkleTreeError::InvalidPath)
    }

    /// Serializes the Merkle Tree commitment together with a message in a single vector of bytes.
    /// Outputs msg || self as a vector of bytes.
    pub fn concat_with_msg(&self, msg: &[u8]) -> Vec<u8>
    where
        D: Digest + FixedOutput,
    {
        let mut msgp = msg.to_vec();
        let mut bytes = self.root.clone();
        msgp.append(&mut bytes);

        msgp
    }
}

/// Tree of hashes, providing a commitment of data and its ordering.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MerkleTree<D>
where
    D: Digest + FixedOutput,
{
    /// The nodes are stored in an array heap:
    /// `nodes[0]` is the root,
    /// the parent of `nodes[i]` is `nodes[(i-1)/2]`
    /// the children of `nodes[i]` are `{nodes[2i + 1], nodes[2i + 2]}`
    /// All nodes have size `Output<D>::output_size()`, even leafs (which are
    /// hashed before committing them).
    pub(crate) nodes: Vec<Vec<u8>>,
    /// The leaves begin at `nodes[leaf_off]`
    leaf_off: usize,
    /// Number of leaves cached in the merkle tree
    pub(crate) n: usize,
    /// Phantom type to link the tree with its hasher
    hasher: PhantomData<D>,
}

impl<D> MerkleTree<D>
where
    D: Digest + FixedOutput,
{
    /// Provided a non-empty list of leaves, `create` generates its corresponding `MerkleTree`.
    pub fn create(leaves: &[Vec<u8>]) -> MerkleTree<D> {
        let n = leaves.len();
        assert!(n > 0, "MerkleTree::create() called with no leaves");

        let num_nodes = n + n.next_power_of_two() - 1;

        let mut nodes = vec![vec![0u8]; num_nodes];

        for i in 0..leaves.len() {
            nodes[num_nodes - n + i] = D::digest(&leaves[i].clone()).to_vec();
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
            nodes[i] = D::new().chain(left).chain(right).finalize().to_vec();
        }

        Self {
            nodes,
            n,
            leaf_off: num_nodes - n,
            hasher: PhantomData::default(),
        }
    }

    /// Convert merkle tree to a commitment. This function simply returns the root
    pub fn to_commitment(&self) -> MerkleTreeCommitment<D> {
        MerkleTreeCommitment {
            root: self.nodes[0].clone(),
            nr_leaves: self.n,
            hasher: self.hasher,
        }
    }

    /// Get the root of the tree
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

    fn idx_of_leaf(&self, i: usize) -> usize {
        self.leaf_off + i
    }

    /// Convert a `MerkleTree` into a byte string, containing $4 + n * S$ where $n$ is the
    /// number of nodes and $S$ the output size of the hash function.
    ///
    /// # Layout
    /// The layout of a `MerkleTree` is:
    /// * Number of leaves committed in the Merkle Tree
    /// * All nodes of the merkle tree (starting with the root)
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(8 + self.nodes.len() * D::output_size());
        result.extend_from_slice(
            &u64::try_from(self.n)
                .expect("Length must fit in u32")
                .to_be_bytes(),
        );
        for node in self.nodes.iter() {
            result.extend_from_slice(node);
        }
        result
    }

    /// Try to convert a byte string into a `MerkleTree`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MerkleTreeError> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let n = usize::try_from(u64::from_be_bytes(u64_bytes)).unwrap(); // todo: handle the conversion
        let num_nodes = n + n.next_power_of_two() - 1;
        let mut nodes = Vec::with_capacity(num_nodes);
        for i in 0..num_nodes {
            nodes.push(bytes[8 + i * D::output_size()..8 + (i + 1) * D::output_size()].to_vec());
        }
        Ok(Self {
            nodes,
            leaf_off: num_nodes - n,
            n,
            hasher: PhantomData::default(),
        })
    }
}

impl<D: Digest + Clone + FixedOutput> Path<D> {
    /// Convert to bytes
    ///
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

    /// Convert a byte slice into a Path
    pub fn from_bytes(bytes: &[u8]) -> Result<Path<D>, MerkleTreeError> {
        // todo: handle conversions
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let index = usize::try_from(u64::from_be_bytes(u64_bytes)).unwrap();
        u64_bytes.copy_from_slice(&bytes[8..16]);
        let len = usize::try_from(u64::from_be_bytes(u64_bytes)).unwrap();
        let mut values = Vec::with_capacity(len);
        for i in 0..(len as usize) {
            values.push(bytes[16 + i * D::output_size()..16 + (i + 1) * D::output_size()].to_vec());
        }

        Ok(Path {
            values,
            index,
            hasher: Default::default(),
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
    use bincode;
    use blake2::Blake2b;
    use proptest::collection::{hash_set, vec};
    use proptest::prelude::*;

    prop_compose! {
        fn arb_tree(max_size: u32)
                   (v in vec(vec(any::<u8>(), 2..16), 2..(max_size as usize))) -> (MerkleTree<Blake2b>, Vec<Vec<u8>>) {
             (MerkleTree::<Blake2b>::create(&v), v)
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
                let decoded: Path<Blake2b> = bincode::deserialize(&encoded).unwrap();
                assert!(t.to_commitment().check(&values[i], &decoded).is_ok());
            })
        }

        #[test]
        fn test_bytes_tree((t, values) in arb_tree(5)) {
            let bytes = t.to_bytes();
            let deserialised = MerkleTree::<Blake2b>::from_bytes(&bytes).unwrap();
            let tree = MerkleTree::<Blake2b>::create(&values);
            assert_eq!(tree.nodes, deserialised.nodes);

            let encoded = bincode::serialize(&t).unwrap();
            let decoded: MerkleTree::<Blake2b> = bincode::deserialize(&encoded).unwrap();
            assert_eq!(tree.nodes, decoded.nodes);
        }

        #[test]
        fn test_bytes_tree_commitment((t, values) in arb_tree(5)) {
            let encoded = bincode::serialize(&t.to_commitment()).unwrap();
            let decoded: MerkleTreeCommitment::<Blake2b> = bincode::deserialize(&encoded).unwrap();
            let tree_commitment = MerkleTree::<Blake2b>::create(&values).to_commitment();
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
                                    (vals in hash_set(vec(any::<u8>(), 2..16), pow2_plus1(h)),
                                     proof in vec(vec(any::<u8>(), 16), h)) -> (Vec<Vec<u8>>, Vec<Vec<u8>>) {
            (vals.into_iter().collect(), proof)
        }
    }

    proptest! {
        #[test]
        fn test_create_invalid_proof(
            i in any::<usize>(),
            (values, proof) in values_with_invalid_proof(10)
        ) {
            let t = MerkleTree::<Blake2b>::create(&values[1..]);
            let index = i % (values.len() - 1);
            let path = Path{values: proof
                            .iter()
                            .map(|x|  Blake2b::digest(x).to_vec())
                            .collect(),
                index,
                hasher: PhantomData::<Blake2b>::default()
                };
            assert!(t.to_commitment().check(&values[0], &path).is_err());
        }
    }
}
