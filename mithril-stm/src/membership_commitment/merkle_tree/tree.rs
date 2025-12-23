use std::marker::PhantomData;

use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};

use crate::StmResult;

use super::{
    MerkleBatchPath, MerklePath, MerkleTreeBatchCommitment, MerkleTreeCommitment, MerkleTreeError,
    MerkleTreeLeaf, left_child, parent, right_child, sibling,
};

/// Tree of hashes, providing a commitment of data and its ordering.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MerkleTree<D: Digest, L: MerkleTreeLeaf> {
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
    /// Phantom type to link the tree with its leaves
    leaves: PhantomData<L>,
}

impl<D: Digest + FixedOutput, L: MerkleTreeLeaf> MerkleTree<D, L> {
    /// Provided a non-empty list of leaves, `create` generates its corresponding `MerkleTree`.
    pub(crate) fn new(leaves: &[L]) -> MerkleTree<D, L> {
        let n = leaves.len();
        assert!(n > 0, "MerkleTree::create() called with no leaves");

        let num_nodes = n + n.next_power_of_two() - 1;

        let mut nodes = vec![vec![0u8]; num_nodes];

        for i in 0..leaves.len() {
            nodes[num_nodes - n + i] = D::digest(leaves[i].as_bytes_for_merkle_tree()).to_vec();
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
            nodes[i] = D::new().chain_update(left).chain_update(right).finalize().to_vec();
        }

        Self {
            nodes,
            n,
            leaf_off: num_nodes - n,
            hasher: PhantomData,
            leaves: PhantomData,
        }
    }

    /// Provided a non-empty list of leaves, `create` generates its corresponding `MerkleTree`.
    #[deprecated(since = "0.5.0", note = "Use `new` instead")]
    pub fn create(leaves: &[L]) -> MerkleTree<D, L> {
        Self::new(leaves)
    }

    /// Return the index of the leaf.
    fn get_leaf_index(&self, i: usize) -> usize {
        self.leaf_off + i
    }

    /// Convert merkle tree to a batch compatible commitment.
    /// This function simply returns the root and the number of leaves in the tree.
    pub(crate) fn to_merkle_tree_batch_commitment(&self) -> MerkleTreeBatchCommitment<D, L> {
        MerkleTreeBatchCommitment::new(self.nodes[0].clone(), self.n)
    }

    /// Convert merkle tree to a batch compatible commitment.
    /// This function simply returns the root and the number of leaves in the tree.
    #[deprecated(
        since = "0.5.0",
        note = "Use `to_merkle_tree_batch_commitment` instead"
    )]
    pub fn to_commitment_batch_compat(&self) -> MerkleTreeBatchCommitment<D, L> {
        Self::to_merkle_tree_batch_commitment(self)
    }

    /// Get a path for a batch of leaves. The indices must be ordered. We use the Octopus algorithm to
    /// avoid redundancy with nodes in the path. Let `x1, . . . , xk` be the indices of elements we
    /// want to produce an opening for. The algorithm takes as input `x1, . . ., xk`, and  proceeds as follows:
    /// 1. Initialise the proof vector, `proof = []`.
    /// 2. Given an input vector `v = v1, . . .,vl`, if `v.len() == 1`, return `proof`, else, continue.
    /// 3. Map each `vi` to the corresponding number of the leaf (by adding the offset).
    /// 4. Initialise a new empty vector `p = []`. Next, iterate over each element `vi`
    ///    a. Append the parent of `vi` to `p`
    ///    b. Compute the sibling, `si` of `vi`
    ///    c. If `si == v(i+1)` then do nothing, and skip step four for `v(i+1)`. Else append `si` to `proof`
    /// 5. Iterate from step 2 with input vector `p`
    ///
    /// # Panics
    /// If the indices provided are out of bounds (higher than the number of elements
    /// committed in the `MerkleTree`) or are not ordered, the function fails.
    // todo: Update doc.
    pub(crate) fn compute_merkle_tree_batch_path(&self, indices: Vec<usize>) -> MerkleBatchPath<D>
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

        ordered_indices = ordered_indices.into_iter().map(|i| self.get_leaf_index(i)).collect();

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
            ordered_indices.clone_from(&new_indices);
        }

        MerkleBatchPath::new(proof, indices)
    }

    /// Get a path for a batch of leaves. The indices must be ordered. We use the Octopus algorithm to
    /// avoid redundancy with nodes in the path. Let `x1, . . . , xk` be the indices of elements we
    /// want to produce an opening for. The algorithm takes as input `x1, . . ., xk`, and  proceeds as follows:
    /// 1. Initialise the proof vector, `proof = []`.
    /// 2. Given an input vector `v = v1, . . .,vl`, if `v.len() == 1`, return `proof`, else, continue.
    /// 3. Map each `vi` to the corresponding number of the leaf (by adding the offset).
    /// 4. Initialise a new empty vector `p = []`. Next, iterate over each element `vi`
    ///    a. Append the parent of `vi` to `p`
    ///    b. Compute the sibling, `si` of `vi`
    ///    c. If `si == v(i+1)` then do nothing, and skip step four for `v(i+1)`. Else append `si` to `proof`
    /// 5. Iterate from step 2 with input vector `p`
    ///
    /// # Panics
    /// If the indices provided are out of bounds (higher than the number of elements
    /// committed in the `MerkleTree`) or are not ordered, the function fails.
    // todo: Update doc.
    #[deprecated(since = "0.5.0", note = "Use `compute_merkle_tree_batch_path` instead")]
    pub fn get_batched_path(&self, indices: Vec<usize>) -> MerkleBatchPath<D>
    where
        D: FixedOutput,
    {
        Self::compute_merkle_tree_batch_path(self, indices)
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
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(bytes.get(..8).ok_or(MerkleTreeError::SerializationError)?);
        let n = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;
        let num_nodes = n + n.next_power_of_two() - 1;
        let mut nodes = Vec::with_capacity(num_nodes);
        for i in 0..num_nodes {
            nodes.push(
                bytes
                    .get(
                        8 + i * <D as Digest>::output_size()
                            ..8 + (i + 1) * <D as Digest>::output_size(),
                    )
                    .ok_or(MerkleTreeError::SerializationError)?
                    .to_vec(),
            );
        }
        Ok(Self {
            nodes,
            leaf_off: num_nodes - n,
            n,
            hasher: PhantomData,
            leaves: PhantomData,
        })
    }

    /// Convert merkle tree to a commitment. This function simply returns the root.
    pub(crate) fn to_merkle_tree_commitment(&self) -> MerkleTreeCommitment<D, L> {
        MerkleTreeCommitment::new(self.nodes[0].clone()) // Use private constructor
    }

    /// Convert merkle tree to a commitment. This function simply returns the root.
    #[deprecated(since = "0.5.0", note = "Use `to_merkle_tree_commitment` instead")]
    pub fn to_commitment(&self) -> MerkleTreeCommitment<D, L> {
        Self::to_merkle_tree_commitment(self)
    }

    /// Get a path (hashes of siblings of the path to the root node)
    /// for the `i`th value stored in the tree.
    /// Requires `i < self.n`
    pub(crate) fn compute_merkle_tree_path(&self, i: usize) -> MerklePath<D> {
        assert!(
            i < self.n,
            "Proof index out of bounds: asked for {} out of {}",
            i,
            self.n
        );
        let mut idx = self.get_leaf_index(i);
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

        MerklePath::new(proof, i)
    }

    /// Get a path (hashes of siblings of the path to the root node)
    /// for the `i`th value stored in the tree.
    /// Requires `i < self.n`
    #[deprecated(since = "0.5.0", note = "Use `compute_merkle_tree_path` instead")]
    pub fn get_path(&self, i: usize) -> MerklePath<D> {
        Self::compute_merkle_tree_path(self, i)
    }
}

#[cfg(test)]
mod tests {
    use blake2::{Blake2b, digest::consts::U32};
    use proptest::{collection::vec, prelude::*};
    use rand::{rng, seq::IteratorRandom};

    use crate::{
        membership_commitment::MerkleTreeConcatenationLeaf, signature_scheme::BlsVerificationKey,
    };

    use super::*;

    fn pow2_plus1(h: usize) -> usize {
        1 + 2_usize.pow(h as u32)
    }

    prop_compose! {
        fn arb_tree(max_size: u32)
                   (v in vec(any::<u64>(), 2..max_size as usize)) -> (MerkleTree<Blake2b<U32>, MerkleTreeConcatenationLeaf>, Vec<MerkleTreeConcatenationLeaf>) {
            let pks = vec![BlsVerificationKey::default(); v.len()];
            let leaves = pks.into_iter().zip(v.into_iter()).map(|(key, stake)| MerkleTreeConcatenationLeaf(key, stake)).collect::<Vec<MerkleTreeConcatenationLeaf>>();
             (MerkleTree::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::new(&leaves), leaves)
        }
    }

    proptest! {
        // Test the relation that t.get_path(i) is a valid
        // proof for i
        #![proptest_config(ProptestConfig::with_cases(100))]
        #[test]
        fn test_create_proof((t, values) in arb_tree(30)) {
            values.iter().enumerate().for_each(|(i, _v)| {
                let pf = t.compute_merkle_tree_path(i);
                assert!(t.to_merkle_tree_commitment().verify_leaf_membership_from_path(&values[i], &pf).is_ok());
            })
        }

        #[test]
        fn test_bytes_path((t, values) in arb_tree(30)) {
            values.iter().enumerate().for_each(|(i, _v)| {
                let pf = t.compute_merkle_tree_path(i);
                let bytes = pf.to_bytes();
                let deserialised = MerklePath::from_bytes(&bytes).unwrap();
                assert!(t.to_merkle_tree_commitment().verify_leaf_membership_from_path(&values[i], &deserialised).is_ok());
            })
        }

        #[test]
        fn test_bytes_tree_commitment((t, values) in arb_tree(5)) {
            let encoded = t.to_merkle_tree_commitment().to_bytes();
            let decoded = MerkleTreeCommitment::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::from_bytes(&encoded).unwrap();

            let tree_commitment = MerkleTree::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::new(&values).to_merkle_tree_commitment();
            assert_eq!(tree_commitment.root, decoded.root);
        }

        #[test]
        fn test_bytes_tree((t, values) in arb_tree(5)) {
            let bytes = t.to_bytes();
            let deserialised = MerkleTree::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::from_bytes(&bytes).unwrap();
            let tree = MerkleTree::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::new(&values);
            assert_eq!(tree.nodes, deserialised.nodes);
        }

        #[test]
        fn test_bytes_tree_commitment_batch_compat((t, values) in arb_tree(5)) {
            let encoded = t.to_merkle_tree_batch_commitment().to_bytes();
            let decoded = MerkleTreeBatchCommitment::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::from_bytes(&encoded).unwrap();
            let tree_commitment = MerkleTree::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::new(&values).to_merkle_tree_batch_commitment();
            assert_eq!(tree_commitment.root, decoded.root);
            assert_eq!(tree_commitment.get_number_of_leaves(), decoded.get_number_of_leaves());

        }

    }

    prop_compose! {
        // Returns values with a randomly generated path
        fn values_with_invalid_proof(max_height: usize)
                                    (h in 1..max_height)
                                    (v in vec(any::<u64>(), 2..pow2_plus1(h)),
                                     proof in vec(vec(any::<u8>(), 16), h)) -> (Vec<MerkleTreeConcatenationLeaf>, Vec<Vec<u8>>) {
            let pks = vec![BlsVerificationKey::default(); v.len()];
            let leaves = pks.into_iter().zip(v.into_iter()).map(|(key, stake)| MerkleTreeConcatenationLeaf(key, stake)).collect::<Vec<MerkleTreeConcatenationLeaf>>();
            (leaves, proof)
        }
    }

    proptest! {
        #[test]
        fn test_create_invalid_proof(
            i in any::<usize>(),
            (values, proof) in values_with_invalid_proof(10)
        ) {
            let t = MerkleTree::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::new(&values[1..]);
            let index = i % (values.len() - 1);
            let path_values = proof. iter().map(|x|  Blake2b::<U32>::digest(x).to_vec()).collect();
            let path = MerklePath::new(path_values, index);
            assert!(t.to_merkle_tree_commitment().verify_leaf_membership_from_path(&values[0], &path).is_err());
        }

        #[test]
        fn test_create_invalid_batch_proof(
            i in any::<usize>(),
            (values, proof) in values_with_invalid_proof(10)
        ) {
            let t = MerkleTree::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::new(&values[1..]);
            let indices = vec![i % (values.len() - 1); values.len() / 2];
            let batch_values = vec![values[i % (values.len() - 1)]; values.len() / 2];
            let path = MerkleBatchPath{values: proof
                            .iter()
                            .map(|x|  Blake2b::<U32>::digest(x).to_vec())
                            .collect(),
                indices,
                hasher: PhantomData::<Blake2b<U32>>
                };
            assert!(t.to_merkle_tree_batch_commitment().verify_leaves_membership_from_batch_path(&batch_values, &path).is_err());
        }
    }

    prop_compose! {
        fn arb_tree_arb_batch(max_size: u32)
                   (v in vec(any::<u64>(), 2..max_size as usize)) -> (MerkleTree<Blake2b<U32>, MerkleTreeConcatenationLeaf>, Vec<MerkleTreeConcatenationLeaf>, Vec<usize>) {
            let mut rng = rng();
            let size = v.len();
            let pks = vec![BlsVerificationKey::default(); size];
            let leaves = pks.into_iter().zip(v.into_iter()).map(|(key, stake)| MerkleTreeConcatenationLeaf(key, stake)).collect::<Vec<MerkleTreeConcatenationLeaf>>();

            let indices: Vec<usize> = (0..size).collect();
            let mut mt_list: Vec<usize> = indices.into_iter().choose_multiple(&mut rng, size * 2 / 10 + 1);
            mt_list.sort_unstable();

            let mut batch_values: Vec<MerkleTreeConcatenationLeaf> = Vec::with_capacity(mt_list.len());
            for i in mt_list.iter() {
                batch_values.push(leaves[*i]);
            }

            (MerkleTree::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::new(&leaves), batch_values, mt_list)
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(100))]
        #[test]
        fn test_create_batch_proof((t, batch_values, indices) in arb_tree_arb_batch(30)) {
            let batch_proof = t.compute_merkle_tree_batch_path(indices);
            assert!(t.to_merkle_tree_batch_commitment().verify_leaves_membership_from_batch_path(&batch_values, &batch_proof).is_ok());
        }

        #[test]
        fn test_bytes_batch_path((t, batch_values, indices) in arb_tree_arb_batch(30)) {
            let bp = t.compute_merkle_tree_batch_path(indices);

            let bytes = &bp.to_bytes();
            let deserialized = MerkleBatchPath::from_bytes(bytes).unwrap();
            assert!(t.to_merkle_tree_batch_commitment().verify_leaves_membership_from_batch_path(&batch_values, &deserialized).is_ok());
        }
    }
    mod golden {
        use super::*;
        const GOLDEN_BYTES: &[u8; 40] = &[
            0, 0, 0, 0, 0, 0, 0, 4, 178, 30, 231, 127, 65, 247, 162, 149, 33, 29, 147, 148, 224,
            156, 96, 113, 140, 42, 98, 166, 137, 14, 69, 29, 28, 244, 161, 145, 207, 146, 236, 249,
        ];

        fn golden_value() -> MerkleTreeBatchCommitment<Blake2b<U32>, MerkleTreeConcatenationLeaf> {
            let number_of_leaves = 4;
            let pks = vec![BlsVerificationKey::default(); number_of_leaves];
            let stakes: Vec<u64> = (0..number_of_leaves).map(|i| i as u64).collect();
            let leaves = pks
                .into_iter()
                .zip(stakes)
                .map(|(key, stake)| MerkleTreeConcatenationLeaf(key, stake))
                .collect::<Vec<MerkleTreeConcatenationLeaf>>();

            let tree = MerkleTree::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::new(&leaves);

            tree.to_merkle_tree_batch_commitment()
        }

        #[test]
        fn golden_conversions() {
            let value =
                MerkleTreeBatchCommitment::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::from_bytes(
                    GOLDEN_BYTES,
                )
                .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);

            let serialized =
                MerkleTreeBatchCommitment::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::to_bytes(
                    &value,
                );
            let golden_serialized = MerkleTreeBatchCommitment::to_bytes(&golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }

    mod golden_json {
        use super::*;
        const GOLDEN_JSON: &str = r#"
        {
            "root": [178, 30, 231, 127, 65, 247, 162, 149, 33, 29, 147, 148, 224, 156, 96, 113, 140, 42, 98, 166, 137, 14, 69, 29, 28, 244, 161, 145, 207, 146, 236, 249],
            "nr_leaves": 4,
            "hasher": null
        }"#;

        fn golden_value() -> MerkleTreeBatchCommitment<Blake2b<U32>, MerkleTreeConcatenationLeaf> {
            let number_of_leaves = 4;
            let pks = vec![BlsVerificationKey::default(); number_of_leaves];
            let stakes: Vec<u64> = (0..number_of_leaves).map(|i| i as u64).collect();
            let leaves = pks
                .into_iter()
                .zip(stakes)
                .map(|(key, stake)| MerkleTreeConcatenationLeaf(key, stake))
                .collect::<Vec<MerkleTreeConcatenationLeaf>>();

            let tree = MerkleTree::<Blake2b<U32>, MerkleTreeConcatenationLeaf>::new(&leaves);

            tree.to_merkle_tree_batch_commitment()
        }

        #[test]
        fn golden_conversions() {
            let value = serde_json::from_str(GOLDEN_JSON)
                .expect("This JSON deserialization should not fail");
            assert_eq!(golden_value(), value);

            let serialized =
                serde_json::to_string(&value).expect("This JSON serialization should not fail");
            let golden_serialized = serde_json::to_string(&golden_value())
                .expect("This JSON serialization should not fail");
            assert_eq!(golden_serialized, serialized);
        }
    }
}
