use ff::Field;

use crate::circuits::halo2::hash::{HashCPU, PoseidonHash};
use crate::circuits::halo2::off_circuit::error::MerkleTreeError;
use crate::circuits::halo2::types::{JubjubBase, Target};

type F = JubjubBase;

#[derive(Debug, Copy, Clone)]
pub struct MTLeaf(pub [u8; 64], pub Target);

impl MTLeaf {
    pub fn to_field(&self) -> [F; 3] {
        let mut elements = [F::ZERO; 3];
        let mut u_bytes = [0u8; 32];
        let mut v_bytes = [0u8; 32];
        u_bytes.copy_from_slice(&self.0[0..32]);
        v_bytes.copy_from_slice(&self.0[32..64]);
        let u = JubjubBase::from_bytes_le(&u_bytes)
            .into_option()
            .expect("invalid VK u-coordinate bytes");
        let v = JubjubBase::from_bytes_le(&v_bytes)
            .into_option()
            .expect("invalid VK v-coordinate bytes");
        elements[0] = u;
        elements[1] = v;
        elements[2] = self.1;
        elements
    }

    pub fn to_bytes(&self) -> [u8; 96] {
        let mut bytes = [0u8; 96];
        bytes[0..64].copy_from_slice(&self.0);
        bytes[64..96].copy_from_slice(&self.1.to_bytes_le());
        bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MerkleTreeError> {
        let bytes = bytes.get(0..96).ok_or(MerkleTreeError::SerializationError)?;
        let target_bytes: [u8; 32] = bytes[64..96]
            .try_into()
            .map_err(|_| MerkleTreeError::SerializationError)?;
        let target = Target::from_bytes_le(&target_bytes)
            .into_option()
            .ok_or(MerkleTreeError::SerializationError)?;
        let vk_bytes: [u8; 64] = bytes[0..64]
            .try_into()
            .map_err(|_| MerkleTreeError::SerializationError)?;

        Ok(Self(vk_bytes, target))
    }
}

#[derive(Clone, Copy, Debug)]
// The position of the sibling in the tree.
pub enum Position {
    Left,
    Right,
}

impl From<Position> for F {
    fn from(value: Position) -> Self {
        match value {
            Position::Left => F::ZERO,
            Position::Right => F::ONE,
        }
    }
}

#[derive(Clone, Debug)]
// Struct defining the witness of the MT proof.
pub struct MerklePath {
    // Sibling nodes corresponding to a field value F representing some
    // hash and whether the position is left or right.
    // if position == Position::Left, then sibling is on the left
    // if position == Position::Right, then sibling is on the right
    pub siblings: Vec<(Position, F)>,
}

impl MerklePath {
    pub fn new(siblings: Vec<(Position, F)>) -> Self {
        Self { siblings }
    }

    pub fn get_siblings(&self) -> &[(Position, F)] {
        &self.siblings
    }

    // Function to compute (off circuit) the Merkle tree root given the leaf and the
    // sibling nodes.
    pub fn compute_root(&self, leaf: MTLeaf) -> F {
        let digest = PoseidonHash::hash(&leaf.to_field());

        // Compute the Merkle root.
        self.siblings.iter().fold(digest, |acc, x| match x.0 {
            // if sibling is on the left => hash(sibling, node)
            Position::Left => PoseidonHash::hash(&[x.1, acc]),
            // if sibling is on the right => hash(node, sibling)
            Position::Right => PoseidonHash::hash(&[acc, x.1]),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MerkleTree {
    /// The nodes are stored in an array heap:
    /// * `nodes[0]` is the root,
    /// * the parent of `nodes[i]` is `nodes[(i-1)/2]`
    /// * the children of `nodes[i]` are `{nodes[2i + 1], nodes[2i + 2]}`
    /// * All nodes have size `Output<D>::output_size()`, even leafs (which are hashed before committing them).
    nodes: Vec<F>,
    /// The leaves begin at `nodes[leaf_off]`.
    leaf_off: usize,
    /// Number of leaves cached in the merkle tree.
    n: usize,
}

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
    if i % 2 == 1 { i + 1 } else { i - 1 }
}

impl MerkleTree {
    /// Provided a non-empty list of leaves, `create` generates its corresponding `MerkleTree`.
    pub fn create(leaves: &[MTLeaf]) -> MerkleTree {
        let n = leaves.len();
        assert!(n > 0, "MerkleTree::create() called with no leaves");

        let num_nodes = n + n.next_power_of_two() - 1;
        let mut nodes = vec![F::ZERO; num_nodes];

        for i in 0..leaves.len() {
            nodes[num_nodes - n + i] = PoseidonHash::hash(&leaves[i].to_field());
        }

        let z = PoseidonHash::hash(&[F::ZERO]);
        for i in (0..num_nodes - n).rev() {
            let left = if left_child(i) < num_nodes {
                nodes[left_child(i)]
            } else {
                z
            };
            let right = if right_child(i) < num_nodes {
                nodes[right_child(i)]
            } else {
                z
            };
            nodes[i] = PoseidonHash::hash(&[left, right]);
        }

        Self {
            nodes,
            n,
            leaf_off: num_nodes - n,
        }
    }

    /// Get the root of the tree.
    pub fn root(&self) -> F {
        self.nodes[0]
    }

    /// Return the index of the leaf.
    fn idx_of_leaf(&self, i: usize) -> usize {
        self.leaf_off + i
    }

    pub fn get_path(&self, i: usize) -> MerklePath {
        assert!(
            i < self.n,
            "Proof index out of bounds: asked for {} out of {}",
            i,
            self.n
        );
        let mut idx = self.idx_of_leaf(i);
        let z = PoseidonHash::hash(&[F::ZERO]);
        let mut proof = Vec::new();

        while idx > 0 {
            let h = if sibling(idx) < self.nodes.len() {
                self.nodes[sibling(idx)]
            } else {
                z
            };
            let pos = {
                if (idx & 0b1) == 0 {
                    Position::Left
                } else {
                    Position::Right
                }
            };
            proof.push((pos, h));
            idx = parent(idx);
        }

        MerklePath::new(proof)
    }
    pub fn to_merkle_tree_commitment(&self) -> MerkleTreeCommitment {
        MerkleTreeCommitment::new(self.nodes[0], self.n as u32)
    }
}

#[derive(Debug, Clone)]
pub struct MerkleTreeCommitment {
    merkle_root: F,
    nr_leaves: u32,
}

impl MerkleTreeCommitment {
    pub fn new(merkle_root: F, nr_leaves: u32) -> Self {
        Self {
            merkle_root,
            nr_leaves,
        }
    }
}

impl From<MerkleTreeCommitment> for Vec<u8> {
    fn from(mt_commit: MerkleTreeCommitment) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&mt_commit.merkle_root.to_bytes_le());
        bytes.extend_from_slice(&mt_commit.nr_leaves.to_le_bytes());
        bytes
    }
}

impl TryFrom<&[u8]> for MerkleTreeCommitment {
    type Error = &'static str;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        if bytes.len() != 36 {
            return Err("Invalid byte length for MerkleTreeCommitment");
        }

        let merkle_root = JubjubBase::from_bytes_le(bytes[0..32].try_into().unwrap()).unwrap();
        let nr_leaves = u32::from_le_bytes(bytes[32..36].try_into().unwrap());

        Ok(MerkleTreeCommitment {
            merkle_root,
            nr_leaves,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::signature_scheme::{SchnorrSigningKey, SchnorrVerificationKey};
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    fn create_leaf(value: F) -> MTLeaf {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        // Retry until STM VK derivation succeeds (edge-case scalars); test-only guard
        // to avoid deterministic-seed flakes in golden vectors.
        let sk = loop {
            let sk = SchnorrSigningKey::generate(&mut rng)
                .expect("Failed to generate STM signing key");
            if SchnorrVerificationKey::new_from_signing_key(sk.clone()).is_ok() {
                break sk;
            }
        };
        let stm_vk = SchnorrVerificationKey::new_from_signing_key(sk)
            .expect("Failed to build STM verification key from signing key");
        MTLeaf(stm_vk.to_bytes(), value)
    }

    #[test]
    fn test_merkle_tree_creation() {
        let leaves = vec![
            create_leaf(F::from(1u64)),
            create_leaf(F::from(2u64)),
            create_leaf(F::from(3u64)),
            create_leaf(F::from(4u64)),
        ];

        let tree = MerkleTree::create(&leaves);
        assert_eq!(tree.n, leaves.len(), "Number of leaves mismatch");
        assert!(
            tree.root() != F::ZERO,
            "Root should not be zero when tree is valid"
        );
    }

    #[test]
    fn test_merkle_path_generation() {
        let leaves = vec![
            create_leaf(F::from(1u64)),
            create_leaf(F::from(2u64)),
            create_leaf(F::from(3u64)),
            create_leaf(F::from(4u64)),
        ];

        let tree = MerkleTree::create(&leaves);

        for i in 0..leaves.len() {
            let path = tree.get_path(i);
            assert!(
                !path.siblings.is_empty(),
                "Path should not be empty for any leaf"
            );
        }
    }

    #[test]
    fn test_merkle_path_verification() {
        let leaves = vec![
            create_leaf(F::from(1u64)),
            create_leaf(F::from(2u64)),
            create_leaf(F::from(3u64)),
            create_leaf(F::from(4u64)),
            create_leaf(F::from(5u64)),
            create_leaf(F::from(6u64)),
        ];

        let tree = MerkleTree::create(&leaves);

        for (i, _leaf) in leaves.iter().enumerate() {
            let path = tree.get_path(i);
            let computed_root = path.compute_root(leaves[i]);
            assert_eq!(
                tree.root(),
                computed_root,
                "Computed root does not match the actual root"
            );
        }
    }

    #[test]
    fn test_leaf_bytes_layout() {
        let mut rng = ChaCha20Rng::from_seed([1u8; 32]);
        // Retry until STM VK derivation succeeds (edge-case scalars); test-only guard
        // to avoid deterministic-seed flakes in golden vectors.
        let sk = loop {
            let sk = SchnorrSigningKey::generate(&mut rng)
                .expect("Failed to generate STM signing key");
            if SchnorrVerificationKey::new_from_signing_key(sk.clone()).is_ok() {
                break sk;
            }
        };
        let stm_vk = SchnorrVerificationKey::new_from_signing_key(sk)
            .expect("Failed to build STM verification key from signing key");
        let target = F::random(&mut rng);
        let leaf = MTLeaf(stm_vk.to_bytes(), target);
        let bytes = leaf.to_bytes();
        assert_eq!(bytes.len(), 96);
        assert_eq!(&bytes[0..64], &stm_vk.to_bytes());
    }
}
