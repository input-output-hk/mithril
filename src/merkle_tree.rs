//! Creation and verification of Merkle Trees using the Neptune hash.

use crate::mithril_curves::wrapper::{MithrilField, MithrilFieldWrapper};
use crate::mithril_hash::{IntoHash, MithrilHasher};
use crate::Path;
use ark_ff::biginteger::BigInteger;
use neptune::poseidon::{Poseidon, PoseidonConstants};

#[derive(Debug, Clone)]
pub struct MerkleTree<F>
where
    F: MithrilField,
{
    // The nodes are stored in an array heap:
    // nodes[0] is the root,
    // the parent of nodes[i] is nodes[(i-1)/2]
    // the children of nodes[i] are {nodes[2i + 1], nodes[2i + 2]}
    nodes: Vec<MithrilFieldWrapper<F>>,

    // The leaves begin at nodes[leaf_off]
    leaf_off: usize,

    // Number of leaves cached here
    n: usize,
}

impl<F> MerkleTree<F>
where
    F: MithrilField,
{
    pub fn create<V: IntoHash<F>>(leaves: &[V]) -> MerkleTree<F> {
        let n = leaves.len();
        assert!(n > 0, "MerkleTree::create() called with no leaves");

        let num_nodes = n + n.next_power_of_two() - 1;

        let mut nodes: Vec<MithrilFieldWrapper<F>> =
            vec![MithrilFieldWrapper(F::zero()); num_nodes];

        // Get the hasher, potentially creating it for this thread.
        let constants = PoseidonConstants::new();
        let mut hasher: MithrilHasher<F> = Poseidon::new(&constants);
        for i in 0..n {
            nodes[num_nodes - n + i] = hash_leaf(&mut hasher, &leaves[i]);
        }

        for i in (0..num_nodes - n).rev() {
            let left = if left_child(i) < num_nodes {
                nodes[left_child(i)]
            } else {
                MithrilFieldWrapper(F::zero())
            };
            let right = if right_child(i) < num_nodes {
                nodes[right_child(i)]
            } else {
                left
            };
            nodes[i] = hash_nodes(&mut hasher, left, right);
        }

        Self {
            nodes: nodes,
            n: n,
            leaf_off: num_nodes - n,
        }
    }

    /// Check an inclusion proof that `val` is the `i`th leaf stored in the tree.
    /// Requires i < self.n
    pub fn check<V: IntoHash<F>>(&self, val: &V, i: usize, proof: &Path<F>) -> bool {
        assert!(
            i < self.n,
            "check index out of bounds: asked for {} out of {}",
            i,
            self.n
        );
        let mut idx = i;

        // Get the hasher, potentially creating it for this thread.
        let constants = PoseidonConstants::new();
        let mut hasher: MithrilHasher<F> = Poseidon::new(&constants);
        let mut h = hash_leaf(&mut hasher, val);
        for p in &proof.0 {
            if (idx & 0b1) == 0 {
                h = hash_nodes(&mut hasher, h, *p);
            } else {
                h = hash_nodes(&mut hasher, *p, h);
            }
            idx = idx >> 1;
        }

        h == self.nodes[0]
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.nodes[0].0.into_repr().to_bytes_le()
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
    pub fn get_path(&self, i: usize) -> Path<F> {
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
                self.nodes[sibling(idx)]
            } else {
                self.nodes[idx]
            };
            proof.push(h);
            idx = parent(idx);
        }

        Path(proof)
    }

    fn idx_of_leaf(&self, i: usize) -> usize {
        self.leaf_off + i
    }
}

//////////////////
// Hash Helpers //
//////////////////

fn hash_leaf<'a, F: MithrilField, V: IntoHash<F>>(
    hasher: &mut MithrilHasher<'a, F>,
    leaf: &V,
) -> MithrilFieldWrapper<F> {
    leaf.into_hash(hasher)
}

fn hash_nodes<'a, F: MithrilField>(
    hasher: &mut MithrilHasher<'a, F>,
    left: MithrilFieldWrapper<F>,
    right: MithrilFieldWrapper<F>,
) -> MithrilFieldWrapper<F> {
    hasher.reset();
    hasher.input(left).unwrap();
    hasher.input(right).unwrap();
    hasher.hash()
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
// /////////////////////
// // Testing         //
// /////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use ark_bls12_377::Fr;
    use proptest::collection::{hash_set, vec};
    use proptest::prelude::*;

    prop_compose! {
        fn arb_tree(max_size: u32)
                   (v in vec(any::<u64>(), 2..(max_size as usize))) -> (MerkleTree<Fr>, Vec<u64>) {
             (MerkleTree::create(&v), v)
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
            let t = MerkleTree::<Fr>::create(&values[1..]);
            let constants = PoseidonConstants::new();
            let mut hasher = Poseidon::new(&constants);

            let idx = i % (values.len() - 1);

            let path = Path(proof.iter().map(|x| x.into_hash(&mut hasher)).collect());
            assert!(!t.check(&values[0], idx, &path));
        }
    }
}
