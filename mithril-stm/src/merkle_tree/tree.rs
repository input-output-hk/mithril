use std::marker::PhantomData;

use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};

use crate::error::MerkleTreeError;
use crate::merkle_tree::{
    BatchPath, MTLeaf, MerkleTreeCommitment, MerkleTreeCommitmentBatchCompat, Path, left_child,
    parent, right_child, sibling,
};

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
            hasher: PhantomData,
        }
    }

    /// Get the root of the tree.
    pub fn root(&self) -> &Vec<u8> {
        &self.nodes[0]
    }

    /// Return the index of the leaf.
    fn idx_of_leaf(&self, i: usize) -> usize {
        self.leaf_off + i
    }

    /// Convert merkle tree to a batch compatible commitment.
    /// This function simply returns the root and the number of leaves in the tree.
    pub fn to_commitment_batch_compat(&self) -> MerkleTreeCommitmentBatchCompat<D> {
        MerkleTreeCommitmentBatchCompat::new(self.nodes[0].clone(), self.n)
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
            ordered_indices.clone_from(&new_indices);
        }

        BatchPath::new(proof, indices)
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
        })
    }

    /// Convert merkle tree to a commitment. This function simply returns the root.
    pub fn to_commitment(&self) -> MerkleTreeCommitment<D> {
        MerkleTreeCommitment::new(self.nodes[0].clone()) // Use private constructor
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

        Path::new(proof, i)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bls_multi_signature::VerificationKey;
    use blake2::{Blake2b, digest::consts::U32};
    use proptest::collection::vec;
    use proptest::prelude::*;
    use rand::{rng, seq::IteratorRandom};

    fn pow2_plus1(h: usize) -> usize {
        1 + 2_usize.pow(h as u32)
    }

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

                let encoded = bincode::serde::encode_to_vec(&pf, bincode::config::legacy()).unwrap();
                let (decoded,_) = bincode::serde::decode_from_slice::<Path<Blake2b<U32>>,_>(&encoded, bincode::config::legacy()).unwrap();
                assert!(t.to_commitment().check(&values[i], &decoded).is_ok());
            })
        }

        #[test]
        fn test_bytes_tree_commitment((t, values) in arb_tree(5)) {
            let encoded = bincode::serde::encode_to_vec(t.to_commitment(), bincode::config::legacy()).unwrap();
            let (decoded,_) = bincode::serde::decode_from_slice::<MerkleTreeCommitment<Blake2b<U32>>,_>(&encoded, bincode::config::legacy()).unwrap();

            let tree_commitment = MerkleTree::<Blake2b<U32>>::create(&values).to_commitment();
            assert_eq!(tree_commitment.root, decoded.root);
        }

        #[test]
        fn test_bytes_tree((t, values) in arb_tree(5)) {
            let bytes = t.to_bytes();
            let deserialised = MerkleTree::<Blake2b<U32>>::from_bytes(&bytes).unwrap();
            let tree = MerkleTree::<Blake2b<U32>>::create(&values);
            assert_eq!(tree.nodes, deserialised.nodes);

            let encoded = bincode::serde::encode_to_vec(&t, bincode::config::legacy()).unwrap();
            let (decoded,_) = bincode::serde::decode_from_slice::<MerkleTree<Blake2b<U32>>,_>(&encoded, bincode::config::legacy()).unwrap();
            assert_eq!(tree.nodes, decoded.nodes);
        }

        #[test]
        fn test_bytes_tree_commitment_batch_compat((t, values) in arb_tree(5)) {
            let encoded = bincode::serde::encode_to_vec(t.to_commitment_batch_compat(), bincode::config::legacy()).unwrap();
            let (decoded,_) = bincode::serde::decode_from_slice::<MerkleTreeCommitmentBatchCompat<Blake2b<U32>>,_>(&encoded, bincode::config::legacy()).unwrap();
            let tree_commitment = MerkleTree::<Blake2b<U32>>::create(&values).to_commitment_batch_compat();
            assert_eq!(tree_commitment.root, decoded.root);
            assert_eq!(tree_commitment.get_nr_leaves(), decoded.get_nr_leaves());

        }

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
            let path_values = proof. iter().map(|x|  Blake2b::<U32>::digest(x).to_vec()).collect();
            let path = Path::new(path_values, index);
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
                hasher: PhantomData::<Blake2b<U32>>
                };
            assert!(t.to_commitment_batch_compat().check(&batch_values, &path).is_err());
        }
    }

    prop_compose! {
        fn arb_tree_arb_batch(max_size: u32)
                   (v in vec(any::<u64>(), 2..max_size as usize)) -> (MerkleTree<Blake2b<U32>>, Vec<MTLeaf>, Vec<usize>) {
            let mut rng = rng();
            let size = v.len();
            let pks = vec![VerificationKey::default(); size];
            let leaves = pks.into_iter().zip(v.into_iter()).map(|(key, stake)| MTLeaf(key, stake)).collect::<Vec<MTLeaf>>();

            let indices: Vec<usize> = (0..size).collect();
            let mut mt_list: Vec<usize> = indices.into_iter().choose_multiple(&mut rng, size * 2 / 10 + 1);
            mt_list.sort_unstable();

            let mut batch_values: Vec<MTLeaf> = Vec::with_capacity(mt_list.len());
            for i in mt_list.iter() {
                batch_values.push(leaves[*i]);
            }

            (MerkleTree::<Blake2b<U32>>::create(&leaves), batch_values, mt_list)
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(100))]
        #[test]
        fn test_create_batch_proof((t, batch_values, indices) in arb_tree_arb_batch(30)) {
            let batch_proof = t.get_batched_path(indices);
            assert!(t.to_commitment_batch_compat().check(&batch_values, &batch_proof).is_ok());
        }

        #[test]
        fn test_bytes_batch_path((t, batch_values, indices) in arb_tree_arb_batch(30)) {
            let bp = t.get_batched_path(indices);

            let bytes = &bp.to_bytes();
            let deserialized = BatchPath::from_bytes(bytes).unwrap();
            assert!(t.to_commitment_batch_compat().check(&batch_values, &deserialized).is_ok());

            let encoded = bincode::serde::encode_to_vec(&bp, bincode::config::legacy()).unwrap();
            let (decoded,_) = bincode::serde::decode_from_slice::<BatchPath<Blake2b<U32>>,_>(&encoded, bincode::config::legacy()).unwrap();
            assert!(t.to_commitment_batch_compat().check(&batch_values, &decoded).is_ok());
        }
    }
}
