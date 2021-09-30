//! Creation and verification of Merkle Trees using the Neptune hash.
use crate::Path;

pub trait HashLeaf<L> {
    type F: Eq + Clone + std::fmt::Debug;

    fn new() -> Self;
    fn zero() -> Self::F;
    fn inject(v: &L) -> Self::F;
    fn as_bytes(h: &Self::F) -> Vec<u8>;
    fn hash_children(&mut self, left: &Self::F, right: &Self::F) -> Self::F;
    fn hash(&mut self, leaf: &[Self::F]) -> Self::F {
        leaf.into_iter()
            .fold(Self::zero(), |h, l| self.hash_children(&h, &l))
    }
}

pub mod digest {
    use super::HashLeaf;
    use sha3::Digest;

    impl<T: ark_ff::ToBytes, D: Digest> HashLeaf<T> for D {
        type F = Vec<u8>;

        fn new() -> Self {
            Self::new()
        }

        fn inject(v: &T) -> Self::F {
            ark_ff::to_bytes!(v).unwrap()
        }

        fn zero() -> Self::F {
            vec![0]
        }

        fn as_bytes(h: &Self::F) -> Vec<u8> {
            h.to_vec()
        }

        fn hash_children(&mut self, left: &Self::F, right: &Self::F) -> Self::F {
            let input: &[u8] = &[&left[..], &right[..]].concat();

            D::digest(input)[..].to_vec()
        }
    }
}

#[derive(Debug, Clone)]
pub struct MerkleTree<L,H>
where
    H: HashLeaf<L>,
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

impl<L,H> MerkleTree<L,H>
where
    H: HashLeaf<L>,
{
    /// converting a single V to bytes, and then calling H::from_bytes() should result
    /// in an H::F
    pub fn create(leaves: &[L]) -> MerkleTree<L,H> {
        let n = leaves.len();
        let mut hasher = H::new();
        assert!(n > 0, "MerkleTree::create() called with no leaves");

        let num_nodes = n + n.next_power_of_two() - 1;

        let mut nodes = vec![H::zero(); num_nodes];

        // Get the hasher, potentially creating it for this thread.
        // let constants = PoseidonConstants::new();
        // let mut hasher: MithrilHasher<F> = Poseidon::new(&constants);
        for i in 0..n {
            let leaf_hash = H::inject(&leaves[i]);
            nodes[num_nodes - n + i] = hasher.hash(&[leaf_hash]);
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
            nodes: nodes,
            n: n,
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
        let leaf_hash = H::inject(val);
        let mut h = hasher.hash(&[leaf_hash]);
        for p in &proof.0 {
            if (idx & 0b1) == 0 {
                h = hasher.hash_children(&h, p);
            } else {
                h = hasher.hash_children(p, &h);
            }
            idx = idx >> 1;
        }

        h == self.nodes[0]
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        H::as_bytes(&self.nodes[0])
    }

    // TODO: Does this belong in this module?
    pub fn concat_with_msg(&self, msg: &[u8]) -> Vec<u8> {
        let mut msgp = msg.to_vec();
        let mut bytes = self.to_bytes();
        msgp.append(&mut bytes);

        msgp
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
                   (v in vec(any::<u64>(), 2..(max_size as usize))) -> (MerkleTree<u64, sha3::Sha3_256>, Vec<u64>) {
             (MerkleTree::<u64, sha3::Sha3_256>::create(&v), v)
        }
    }

    proptest! {
        // Test the relation that t.get_path(i) is a valid
        // proof for i
        #![proptest_config(ProptestConfig::with_cases(100))]
        #[test]
        fn test_create_proof((t, values) in arb_tree(30)) {
            for i in 0..values.len() {
                let pf = t.get_path(i as usize);
                assert!(t.check(&values[i], i as usize, &pf));
            }
        }
    }

    fn pow2_plus1(h: usize) -> usize {
        1 + (2 as usize).pow(h as u32)
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
            let t = MerkleTree::<u64, sha3::Sha3_256>::create(&values[1..]);
            let idx = i % (values.len() - 1);

            let path = Path(proof
                            .iter()
                            .map(|x| sha3::Sha3_256::inject(x))
                            .collect());
            assert!(!t.check(&values[0], idx, &path));
        }
    }
}
