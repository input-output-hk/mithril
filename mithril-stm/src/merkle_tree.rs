//! Creation and verification of Merkle Trees
use crate::error::MerkleTreeError;
use crate::multi_sig::VerificationKey;
use crate::stm::{Stake, StmVerificationKey};
use blake2::digest::{consts::U32, Digest, FixedOutput};
use blake2::Blake2b;
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
#[derive(Default, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Path<D: Digest> {
    pub(crate) values: Vec<Vec<u8>>,
    pub(crate) index: usize,
    hasher: PhantomData<D>,
}

/// Path of hashes for a batch of indices.
/// Contains the hashes and the corresponding merkle tree indices of given batch.
/// Used to verify the signatures are issued by the registered signers.
#[derive(Default, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BatchPath<D: Digest + FixedOutput> {
    pub(crate) values: Vec<Vec<u8>>,
    pub(crate) indices: Vec<usize>,
    pub(crate) hasher: PhantomData<D>,
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

/// Batch compatible `MerkleTree` commitment .
/// This structure differs from `MerkleTreeCommitment` in that it stores the number of leaves in the tree
/// as well as the root of the tree.
/// Number of leaves is required by the batch path generation/verification.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleTreeCommitmentBatchCompat<D: Digest> {
    /// Root of the merkle commitment.
    pub root: Vec<u8>,
    nr_leaves: usize,
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

impl MTLeaf {
    pub(crate) fn from_bytes(bytes: &[u8]) -> Result<Self, MerkleTreeError<Blake2b<U32>>> {
        let pk = StmVerificationKey::from_bytes(bytes)
            .map_err(|_| MerkleTreeError::SerializationError)?;
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[96..]);
        let stake = Stake::from_be_bytes(u64_bytes);
        Ok(MTLeaf(pk, stake))
    }
    pub(crate) fn to_bytes(self) -> [u8; 104] {
        let mut result = [0u8; 104];
        result[..96].copy_from_slice(&self.0.to_bytes());
        result[96..].copy_from_slice(&self.1.to_be_bytes());
        result
    }
}

impl From<MTLeaf> for (StmVerificationKey, Stake) {
    fn from(leaf: MTLeaf) -> (StmVerificationKey, Stake) {
        (leaf.0, leaf.1)
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

impl<D: Digest + Clone + FixedOutput> Path<D> {
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

impl<D: Digest + FixedOutput> BatchPath<D> {
    /// Convert the `BatchPath` into byte representation.
    ///
    /// # Layout
    /// The layout of a `BatchPath` is
    /// * Length of values
    /// * Length of indices
    /// * Values
    /// * Indices
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        let len_v = self.values.len();
        let len_i = self.indices.len();

        output.extend_from_slice(&u64::try_from(len_v).unwrap().to_be_bytes());
        output.extend_from_slice(&u64::try_from(len_i).unwrap().to_be_bytes());

        for value in &self.values {
            output.extend_from_slice(value.as_slice())
        }

        for &index in &self.indices {
            output.extend_from_slice(&u64::try_from(index).unwrap().to_be_bytes());
        }
        output
    }

    /// Try to convert a byte string into a `BatchPath`.
    // todo: We should not panic if the size of the slice is invalid (I believe `bytes[offset + i * 8..offset + (i + 1) * 8]` will panic if bytes is not large enough.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MerkleTreeError<D>> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let len_v = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;

        u64_bytes.copy_from_slice(&bytes[8..16]);
        let len_i = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;

        let mut values = Vec::with_capacity(len_v);
        for i in 0..len_v {
            values.push(
                bytes[16 + i * <D as Digest>::output_size()
                    ..16 + (i + 1) * <D as Digest>::output_size()]
                    .to_vec(),
            );
        }
        let offset = 16 + len_v * <D as Digest>::output_size();

        let mut indices = Vec::with_capacity(len_v);
        for i in 0..len_i {
            u64_bytes.copy_from_slice(&bytes[offset + i * 8..offset + (i + 1) * 8]);
            indices.push(
                usize::try_from(u64::from_be_bytes(u64_bytes))
                    .map_err(|_| MerkleTreeError::SerializationError)?,
            );
        }

        Ok(BatchPath {
            values,
            indices,
            hasher: Default::default(),
        })
    }
}

impl<D: Clone + Digest + FixedOutput> MerkleTreeCommitment<D> {
    /// Check an inclusion proof that `val` is part of the tree by traveling the whole path until the root.
    /// # Error
    /// If the merkle tree path is invalid, then the function fails.
    pub fn check(&self, val: &MTLeaf, proof: &Path<D>) -> Result<(), MerkleTreeError<D>> {
        let mut idx = proof.index;

        let mut h = D::digest(val.to_bytes()).to_vec();
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

impl<D: Clone + Digest> MerkleTreeCommitmentBatchCompat<D> {
    /// Serializes the Merkle Tree commitment together with a message in a single vector of bytes.
    /// Outputs `msg || self` as a vector of bytes.
    // todo: Do we need to concat msg to whole commitment (nr_leaves and root) or just the root?
    pub fn concat_with_msg(&self, msg: &[u8]) -> Vec<u8>
    where
        D: Digest,
    {
        let mut msgp = msg.to_vec();
        let mut bytes = self.root.clone();
        msgp.append(&mut bytes);

        msgp
    }

    /// Check a proof of a batched opening. The indices must be ordered.
    ///
    /// # Error
    /// Returns an error if the proof is invalid.
    // todo: Update doc.
    // todo: Simplify the algorithm.
    // todo: Maybe we want more granular errors, rather than only `BatchPathInvalid`
    pub fn check(
        &self,
        batch_val: &Vec<MTLeaf>,
        proof: &BatchPath<D>,
    ) -> Result<(), MerkleTreeError<D>>
    where
        D: FixedOutput,
    {
        if batch_val.len() != proof.indices.len() {
            return Err(MerkleTreeError::BatchPathInvalid(proof.clone()));
        }
        let mut ordered_indices: Vec<usize> = proof.indices.clone();
        ordered_indices.sort_unstable();

        if ordered_indices != proof.indices {
            return Err(MerkleTreeError::BatchPathInvalid(proof.clone()));
        }

        let nr_nodes = self.nr_leaves + self.nr_leaves.next_power_of_two() - 1;

        ordered_indices = ordered_indices
            .into_iter()
            .map(|i| i + self.nr_leaves.next_power_of_two() - 1)
            .collect();

        let mut idx = ordered_indices[0];
        // First we need to hash the leave values
        let mut leaves: Vec<Vec<u8>> = batch_val
            .iter()
            .map(|val| D::digest(val.to_bytes()).to_vec())
            .collect();

        let mut values = proof.values.clone();

        while idx > 0 {
            let mut new_hashes = Vec::with_capacity(ordered_indices.len());
            let mut new_indices = Vec::with_capacity(ordered_indices.len());
            let mut i = 0;
            idx = parent(idx);
            while i < ordered_indices.len() {
                new_indices.push(parent(ordered_indices[i]));
                if ordered_indices[i] & 1 == 0 {
                    new_hashes.push(
                        D::new()
                            .chain(values.get(0).ok_or(MerkleTreeError::SerializationError)?)
                            .chain(&leaves[i])
                            .finalize()
                            .to_vec(),
                    );
                    values.remove(0);
                } else {
                    let sibling = sibling(ordered_indices[i]);
                    if i < ordered_indices.len() - 1 && ordered_indices[i + 1] == sibling {
                        new_hashes.push(
                            D::new()
                                .chain(&leaves[i])
                                .chain(&leaves[i + 1])
                                .finalize()
                                .to_vec(),
                        );
                        i += 1;
                    } else if sibling < nr_nodes {
                        new_hashes.push(
                            D::new()
                                .chain(&leaves[i])
                                .chain(values.get(0).ok_or(MerkleTreeError::SerializationError)?)
                                .finalize()
                                .to_vec(),
                        );
                        values.remove(0);
                    } else {
                        new_hashes.push(
                            D::new()
                                .chain(&leaves[i])
                                .chain(&D::digest([0u8]))
                                .finalize()
                                .to_vec(),
                        );
                    }
                }
                i += 1;
            }
            leaves = new_hashes.clone();
            ordered_indices = new_indices.clone();
        }

        if leaves.len() == 1 && leaves[0] == self.root {
            return Ok(());
        }

        Err(MerkleTreeError::BatchPathInvalid(proof.clone()))
    }
}

impl<D: Digest + FixedOutput> MerkleTree<D> {
    /// Provided a non-empty list of leaves, `create` generates its corresponding `MerkleTree`.
    pub fn create(leaves: &[MTLeaf]) -> MerkleTree<D> {
        let n = leaves.len();
        assert!(n > 0, "MerkleTree::create() called with no leaves");

        let num_nodes = n + n.next_power_of_two() - 1;

        let mut nodes = vec![vec![0u8]; num_nodes];

        for i in 0..leaves.len() {
            nodes[num_nodes - n + i] = D::digest(leaves[i].to_bytes()).to_vec();
        }

        for i in (0..num_nodes - n).rev() {
            let z = D::digest([0u8]).to_vec();
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

    /// Convert merkle tree to a batch compatible commitment.
    /// This function simply returns the root and the number of leaves in the tree.
    pub fn to_commitment_batch_compat(&self) -> MerkleTreeCommitmentBatchCompat<D> {
        MerkleTreeCommitmentBatchCompat {
            root: self.nodes[0].clone(),
            nr_leaves: self.n,
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
                D::digest([0u8]).to_vec()
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

    /// Get a path for a batch of leaves. The indices must be ordered. We use the Octopus algorithm to
    /// avoid redundancy with nodes in the path. Let `x1, . . . , xk` be the indices of elements we
    /// want to produce an opening for. The algorithm takes as input `x1, . . ., xk`, and  proceeds as follows:
    /// 1. Initialise the proof vector, `proof = []`.
    /// 2. Given an input vector `v = v1, . . .,vl`, if `v.len() == 1`, return `proof`, else, continue.
    /// 3. Map each `vi` to the corresponding number of the leaf (by adding the offset).
    /// 4. Initialise a new empty vector `p = []`. Next, iterate over each element `vi`
    ///     a. Append the parent of `vi` to `p`
    ///     b. Compute the sibling, `si` of `vi`
    ///     c. If `si == v(i+1)` then do nothing, and skip step four for `v(i+1)`. Else append `si` to `proof`
    /// 5. Iterate from step 2 with input vector `p`
    ///
    /// # Panics
    /// If the indices provided are out of bounds (higher than the number of elements
    /// committed in the `MerkleTree`) or are not ordered, the function fails.
    // todo: Update doc.
    pub fn get_batched_path(&self, indices: Vec<usize>) -> BatchPath<D>
    where
        D: FixedOutput,
    {
        assert!(
            !indices.is_empty(),
            "get_batched_path() called with no indices"
        );
        for i in &indices {
            assert!(
                i < &self.n,
                "Proof index out of bounds: asked for {} out of {}",
                i,
                self.n
            );
        }

        let mut ordered_indices: Vec<usize> = indices.clone();
        ordered_indices.sort_unstable();

        assert_eq!(ordered_indices, indices, "Indices should be ordered");

        ordered_indices = ordered_indices
            .into_iter()
            .map(|i| self.idx_of_leaf(i))
            .collect();

        let mut idx = ordered_indices[0];
        let mut proof = Vec::new();

        while idx > 0 {
            let mut new_indices = Vec::with_capacity(ordered_indices.len());
            let mut i = 0;
            idx = parent(idx);
            while i < ordered_indices.len() {
                new_indices.push(parent(ordered_indices[i]));
                let sibling = sibling(ordered_indices[i]);
                if i < ordered_indices.len() - 1 && ordered_indices[i + 1] == sibling {
                    i += 1;
                } else if sibling < self.nodes.len() {
                    proof.push(self.nodes[sibling].clone());
                }
                i += 1;
            }
            ordered_indices = new_indices.clone();
        }

        BatchPath {
            values: proof,
            indices,
            hasher: Default::default(),
        }
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
    use rand::{seq::IteratorRandom, thread_rng};

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
        fn test_bytes_tree_commitment((t, values) in arb_tree(5)) {
            let encoded = bincode::serialize(&t.to_commitment()).unwrap();
            let decoded: MerkleTreeCommitment::<Blake2b<U32>> = bincode::deserialize(&encoded).unwrap();
            let tree_commitment = MerkleTree::<Blake2b<U32>>::create(&values).to_commitment();
            assert_eq!(tree_commitment.root, decoded.root);
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
        fn test_create_batch_proof((t, values) in arb_tree(30)) {
            let mut mt_index_list :Vec<usize> = Vec::new();
            for (i, _v) in values.iter().enumerate() {
                mt_index_list.push(i);
            }
            mt_index_list.sort_unstable();
            let batch_proof = t.get_batched_path(mt_index_list);
            assert!(t.to_commitment_batch_compat().check(&values, &batch_proof).is_ok());
        }

        #[test]
        fn test_bytes_batch_path((t, values) in arb_tree(30)) {
            let mut mt_index_list :Vec<usize> = Vec::new();
            for (i, _v) in values.iter().enumerate() {
                mt_index_list.push(i);
            }
            mt_index_list.sort_unstable();

            let bp = t.get_batched_path(mt_index_list);

            let bytes = &bp.to_bytes();
            let deserialized = BatchPath::from_bytes(bytes).unwrap();
            assert!(t.to_commitment_batch_compat().check(&values, &deserialized).is_ok());

            let encoded = bincode::serialize(&bp).unwrap();
            let decoded: BatchPath<Blake2b<U32>> = bincode::deserialize(&encoded).unwrap();
            assert!(t.to_commitment_batch_compat().check(&values, &decoded).is_ok());
        }

        #[test]
        fn test_bytes_tree_commitment_batch_compat((t, values) in arb_tree(5)) {
            let encoded = bincode::serialize(&t.to_commitment_batch_compat()).unwrap();
            let decoded: MerkleTreeCommitmentBatchCompat::<Blake2b<U32>> = bincode::deserialize(&encoded).unwrap();
            let tree_commitment = MerkleTree::<Blake2b<U32>>::create(&values).to_commitment_batch_compat();
            assert_eq!(tree_commitment.root, decoded.root);
            assert_eq!(tree_commitment.nr_leaves, decoded.nr_leaves);

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


        #[test]
        fn test_create_invalid_batch_proof(
            i in any::<usize>(),
            (values, proof) in values_with_invalid_proof(10)
        ) {
            let t = MerkleTree::<Blake2b<U32>>::create(&values[1..]);
            let indices = vec![i % (values.len() - 1); values.len() / 2];
            let batch_values = vec![values[i % (values.len() - 1)]; values.len() / 2];
            let path = BatchPath{values: proof
                            .iter()
                            .map(|x|  Blake2b::<U32>::digest(x).to_vec())
                            .collect(),
                indices,
                hasher: PhantomData::<Blake2b<U32>>::default()
                };
            assert!(t.to_commitment_batch_compat().check(&batch_values, &path).is_err());
        }
    }

    // prop_compose! {
    //     fn arb_tree_arb_index_list(max_size: u32)
    //                (v in vec(any::<u64>(), 2..max_size as usize)) -> (MerkleTree<Blake2b<U32>>, Vec<MTLeaf>, Vec<&'static usize>) {
    //         let mut rng = thread_rng();
    //         let size = v.len();
    //         let pks = vec![VerificationKey::default(); size];
    //         let leaves = pks.into_iter().zip(v.into_iter()).map(|(key, stake)| MTLeaf(key, stake)).collect::<Vec<MTLeaf>>();
    //         let mut indices :Vec<usize> = Vec::with_capacity(size);
    //         for (i, _v) in leaves.iter().enumerate() {
    //             indices.push(i);
    //         }
    //         let mt_index_list: Vec<&usize> = indices.iter().choose_multiple(&mut rng, (size/10)*7);
    //          (MerkleTree::<Blake2b<U32>>::create(&leaves), leaves, mt_index_list)
    //     }
    // }
}
