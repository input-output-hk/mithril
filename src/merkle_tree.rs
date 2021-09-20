//! Creation and verification of Merkle Trees using the Neptune hash.

use crate::Path;
use blstrs::Bls12;
use neptune::poseidon::{Poseidon, PoseidonConstants};
use neptune::{scalar_from_u64, Scalar};

use lazy_static::lazy_static;
use std::cell::RefCell;
use std::thread_local;

lazy_static! {
    /// Poseidon needs a reference to the Constants to create a new instance.
    static ref CONSTS: PoseidonConstants<Bls12, typenum::U2> = PoseidonConstants::new();
}

thread_local! {
    /// Creates a new hasher only once per thread.
    static HASHER: RefCell<Poseidon<'static, Bls12, typenum::U2>> = {
        RefCell::new(Poseidon::new(&CONSTS))
    };
}

pub type Hash = neptune::Scalar;

pub type MerkleHasher<'a> = Poseidon<'a, Bls12, typenum::U2>;

#[derive(Debug)]
pub struct MerkleTree {
    // The nodes are stored in an array heap:
    // nodes[0] is the root,
    // the parent of nodes[i] is nodes[(i-1)/2]
    // the children of nodes[i] are {nodes[2i + 1], nodes[2i + 2]}
    nodes: Vec<Hash>,

    // The leaves begin at nodes[leaf_off]
    leaf_off: usize,

    // Number of leaves cached here
    n: usize,
}

pub trait IntoHash {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Scalar;
}

impl MerkleTree {
    pub fn create<V: IntoHash>(leaves: &[V]) -> MerkleTree {
        let n = leaves.len();
        assert!(
            n > 1,
            "MerkleTree::create() called with fewer than 2 leaves"
        );
        assert!(
            (n & (n - 1) == 0),
            "MerkleTree::create() called with non-power of 2 leaves {}",
            n
        );

        let num_nodes = 2 * n - 1;

        let mut nodes = vec![scalar_from_u64(0); num_nodes];

        // Get the hasher, potentially creating it for this thread.
        HASHER.with(|hasher| {
            for i in 0..n {
                nodes[num_nodes - n + i] = hash_leaf(&mut hasher.borrow_mut(), &leaves[i]);
            }

            for i in (0..num_nodes - n).rev() {
                nodes[i] = hash_nodes(
                    &mut hasher.borrow_mut(),
                    nodes[left_child(i)],
                    nodes[right_child(i)],
                );
            }
        });

        Self {
            nodes: nodes,
            n: n,
            leaf_off: num_nodes - n,
        }
    }

    /// Check an inclusion proof that `val` is the `i`th leaf stored in the tree.
    /// Requires i < self.n
    pub fn check<V: IntoHash>(&self, val: &V, i: usize, proof: &Path) -> bool {
        assert!(
            i < self.n,
            "check index out of bounds: asked for {} out of {}",
            i,
            self.n
        );
        let mut idx = i;
        let height = (self.n as f64).log2().ceil() as usize;

        // Get the hasher, potentially creating it for this thread.
        let h = HASHER.with(|hasher| {
            let mut h = hash_leaf(&mut hasher.borrow_mut(), val);
            for k in 1..=height {
                if (idx & 0b1) == 0 {
                    h = hash_nodes(&mut hasher.borrow_mut(), h, proof.0[k - 1]);
                } else {
                    h = hash_nodes(&mut hasher.borrow_mut(), proof.0[k - 1], h);
                }
                idx = idx >> 1;
            }
            h
        });

        h == self.nodes[0]
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.nodes[0].to_bytes_le().to_vec()
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
    pub fn get_path(&self, i: usize) -> Path {
        assert!(
            i < self.n,
            "Proof index out of bounds: asked for {} out of {}",
            i,
            self.n
        );
        let mut idx = self.idx_of_leaf(i);
        let mut proof = Vec::new();

        while idx > 0 {
            proof.push(self.nodes[sibling(idx)]);
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

fn hash_leaf<'a, V: IntoHash>(hasher: &mut MerkleHasher<'a>, leaf: &V) -> Hash {
    leaf.into_hash(hasher)
}

fn hash_nodes<'a>(hasher: &mut MerkleHasher<'a>, left: Hash, right: Hash) -> Hash {
    hasher.reset();
    hasher.input(left).unwrap();
    hasher.input(right).unwrap();
    hasher.hash()
}

fn hash_binary<'a, A>(hasher: &mut MerkleHasher<'a>, left: Hash, right: Hash) -> Hash {
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

/////////////////////
// Value Instances //
/////////////////////

impl IntoHash for u64 {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        hasher.reset();
        hasher.input(scalar_from_u64(*self)).unwrap();
        hasher.hash()
    }
}

impl IntoHash for Scalar {
    fn into_hash<'a>(&self, _mh: &mut MerkleHasher<'a>) -> Hash {
        *self
    }
}

impl<V: IntoHash> IntoHash for Vec<V> {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        assert!(self.len() > 0, "Can not convert empty slice to Hash");
        let mut h = self[0].into_hash(hasher);
        for val in self {
            h = (h, val.into_hash(hasher)).into_hash(hasher);
        }

        h
    }
}

impl<V1: IntoHash, V2: IntoHash> IntoHash for (V1, V2) {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        let h1 = self.0.into_hash(hasher);
        let h2 = self.1.into_hash(hasher);
        hasher.reset();
        hasher.input(h1).unwrap();
        hasher.input(h2).unwrap();
        hasher.hash()
    }
}

impl<V: IntoHash> IntoHash for Option<V> {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        if let Some(inner) = self {
            inner.into_hash(hasher)
        } else {
            0u64.into_hash(hasher)
        }
    }
}

impl IntoHash for blstrs::G1Affine {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        let x = blstrs::FpRepr::from(self.x()).0.to_vec();
        let y = blstrs::FpRepr::from(self.y()).0.to_vec();
        (x, y).into_hash(hasher)
    }
}

impl IntoHash for blstrs::FpRepr {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        self.0.to_vec().into_hash(hasher)
    }
}

impl IntoHash for blstrs::Fp {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        blstrs::FpRepr::from(*self).into_hash(hasher)
    }
}

impl IntoHash for blstrs::Fp2 {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        vec![self.c0(), self.c1()].into_hash(hasher)
    }
}

impl IntoHash for blstrs::G1Projective {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        vec![self.x(), self.y(), self.z()].into_hash(hasher)
    }
}

impl IntoHash for blstrs::G2Projective {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        vec![self.x(), self.y(), self.z()].into_hash(hasher)
    }
}

impl IntoHash for crate::msp::MspPk {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        vec![
            self.mvk.into_hash(hasher),
            self.k1.into_hash(hasher),
            self.k2.into_hash(hasher),
        ]
        .into_hash(hasher)
    }
}

impl IntoHash for crate::msp::MspMvk {
    fn into_hash<'a>(&self, hasher: &mut MerkleHasher<'a>) -> Hash {
        self.0.into_hash(hasher)
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
        fn arb_tree(max_height: u32)
                   (height in 1..10)
                   (v in vec(any::<u64>(), (2 as usize).pow(height as u32))) -> (MerkleTree, Vec<u64>) {
             (MerkleTree::create(&v), v)
        }
    }

    proptest! {
        // Test the relation that t.get_path(i) is a valid
        // proof for i
        #![proptest_config(ProptestConfig::with_cases(10))]
        #[test]
        fn test_create_proof((t, values) in arb_tree(10)) {
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
            let t = MerkleTree::create(&values[1..]);
            let constants = PoseidonConstants::new();
            let mut hasher = Poseidon::new(&constants);

            let idx = i % (values.len() - 1);

            let path = Path(proof.iter().map(|x| x.into_hash(&mut hasher)).collect());
            assert!(!t.check(&values[0], idx, &path));
        }
    }
}
