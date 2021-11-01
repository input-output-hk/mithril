//! Creation and verification of Merkle Trees
use crate::Path;
use std::fmt::Debug;

/// This trait describes a hashing algorithm. For mithril we need
/// (1) a way to inject stored values into the tree
/// (2) a way to combine hashes
/// (H_p is used for both of these in the paper)
pub trait MTHashLeaf<L> {
    type F: Eq + Clone + Debug;

    /// Create a new hasher
    fn new() -> Self;

    /// This should be some "null" representative
    fn zero() -> Self::F;

    /// How to extract hashes as bytes
    fn as_bytes(h: &Self::F) -> Vec<u8>;

    /// How to map (or label) values with their hash values
    fn inject(&mut self, v: &L) -> Self::F;

    /// Combine (and hash) two hash values
    fn hash_children(&mut self, left: &Self::F, right: &Self::F) -> Self::F;

    /// Hash together an arbitrary number of values,
    /// Reducing the input with `zero()` as the initial value
    /// and `hash_children` as the operation
    fn hash(&mut self, leaf: &[Self::F]) -> Self::F {
        leaf.iter()
            .fold(Self::zero(), |h, l| self.hash_children(&h, l))
    }
}

#[derive(Debug, Clone)]
pub struct MerkleTree<L, H>
where
    H: MTHashLeaf<L>,
{
    // The nodes are stored in an array heap:
    // nodes[0] is the root,
    // the parent of nodes[i] is nodes[(i-1)/2]
    // the children of nodes[i] are {nodes[2i + 1], nodes[2i + 2]}
    nodes: Vec<H::F>,

    // The leaves begin at nodes[leaf_off]
    leaf_off: usize,

    // Number of leaves cached here
    n: usize,
}

impl<L, H> MerkleTree<L, H>
where
    H: MTHashLeaf<L>,
{
    /// converting a single L to bytes, and then calling H::from_bytes() should result
    /// in an H::F
    pub fn create(leaves: &[L]) -> MerkleTree<L, H> {
        let n = leaves.len();
        let mut hasher = H::new();
        assert!(n > 0, "MerkleTree::create() called with no leaves");

        let num_nodes = n + n.next_power_of_two() - 1;

        let mut nodes = vec![H::zero(); num_nodes];

        // Get the hasher, potentially creating it for this thread.
        for i in 0..n {
            nodes[num_nodes - n + i] = hasher.inject(&leaves[i]);
        }

        for i in (0..num_nodes - n).rev() {
            let z = H::zero();
            let left = if left_child(i) < num_nodes {
                &nodes[left_child(i)]
            } else {
                &z
            };
            let right = if right_child(i) < num_nodes {
                &nodes[right_child(i)]
            } else {
                &left
            };
            nodes[i] = hasher.hash_children(left, right);
        }

        Self {
            nodes,
            n,
            leaf_off: num_nodes - n,
        }
    }

    /// Check an inclusion proof that `val` is the `i`th leaf stored in the tree.
    /// Requires i < self.n
    pub fn check(&self, val: &L, i: usize, proof: &Path<H::F>) -> bool {
        assert!(
            i < self.n,
            "check index out of bounds: asked for {} out of {}",
            i,
            self.n
        );
        let mut idx = i;

        let mut hasher = H::new();
        let mut h = hasher.inject(val);
        for p in &proof.0 {
            if (idx & 0b1) == 0 {
                h = hasher.hash_children(&h, p);
            } else {
                h = hasher.hash_children(p, &h);
            }
            idx >>= 1;
        }

        h == self.nodes[0]
    }

    pub fn root_to_bytes(&self) -> Vec<u8> {
        H::as_bytes(&self.nodes[0])
    }

    /// Get a path (hashes of siblings of the path to the root node
    /// for the `i`th value stored in the tree.
    /// Requires `i < self.n`
    pub fn get_path(&self, i: usize) -> Path<H::F> {
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
                &self.nodes[sibling(idx)]
            } else {
                &self.nodes[idx]
            };
            proof.push(h.clone());
            idx = parent(idx);
        }

        Path(proof)
    }

    fn idx_of_leaf(&self, i: usize) -> usize {
        self.leaf_off + i
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
    use proptest::collection::{hash_set, vec};
    use proptest::prelude::*;

    prop_compose! {
        fn arb_tree(max_size: u32)
                   (v in vec(any::<u64>(), 2..(max_size as usize))) -> (MerkleTree<u64, blake2::Blake2b>, Vec<u64>) {
             (MerkleTree::<u64, blake2::Blake2b>::create(&v), v)
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
                assert!(t.check(&values[i], i, &pf));
            })
        }
    }

    fn pow2_plus1(h: usize) -> usize {
        1 + 2_usize.pow(h as u32)
    }

    prop_compose! {
        // Returns values with a randomly generated path
        fn values_with_invalid_proof(max_height: usize)
                                    (h in 1..max_height)
                                    (vals in hash_set(any::<u64>(), pow2_plus1(h)),
                                     proof in vec(any::<u64>(), h)) -> (Vec<u64>, Vec<u64>) {
            (vals.into_iter().collect(), proof)
        }
    }

    proptest! {
        #[test]
        fn test_create_invalid_proof(
            i in any::<usize>(),
            (values, proof) in values_with_invalid_proof(10)
        ) {
            let t = MerkleTree::<u64, blake2::Blake2b>::create(&values[1..]);
            let idx = i % (values.len() - 1);
            let mut hasher = <blake2::Blake2b as MTHashLeaf<u64>>::new();
            let path = Path(proof
                            .iter()
                            .map(|x| hasher.inject(x))
                            .collect());
            assert!(!t.check(&values[0], idx, &path));
        }
    }
}
