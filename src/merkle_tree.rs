use bellperson::bls::Bls12;
use neptune::poseidon::{Poseidon, PoseidonConstants};
use neptune::{scalar_from_u64, Scalar};
use proptest::prelude::*;

type PoseidonHasher<'a> = Poseidon<'a, Bls12>;
type T = neptune::Scalar;

pub struct MerkleTree<'a> {
    // The nodes are stored in an array heap:
    // nodes[0] is the root,
    // the parent of nodes[i] is nodes[(i-1)/2]
    // the children of nodes[i] are {nodes[2i + 1], nodes[2i + 2]}
    nodes: Vec<T>,

    // The leaves begin at nodes[leaf_off]
    leaf_off: usize,

    // Number of leaves cached here
    n: usize,

    // The Poseidon hash state
    hasher: PoseidonHasher<'a>,
}

pub trait Value {
    fn as_scalar<'a>(&self, hasher: &mut Poseidon<'a, Bls12>) -> Scalar;
}

impl <'a> MerkleTree<'a> {
    pub fn new<V:Value>(constants: &'a PoseidonConstants<Bls12, typenum::U2>,
                        leaves: &'a [V]) -> Self {
        let mut hasher = Poseidon::new(&constants);
        let n = leaves.len();
        let num_nodes = 2*n - 1;

        let mut nodes = vec![scalar_from_u64(0); num_nodes];

        for i in 0..n {
            nodes[num_nodes-n+i] = hash_leaf(&mut hasher, &leaves[i]);
        }

        for i in (0..num_nodes-n).rev() {
            nodes[i] = hash_nodes(&mut hasher,
                                  nodes[left_child(i)],
                                  nodes[right_child(i)]);
        }

        Self {
            nodes,
            hasher,
            n,
            leaf_off: num_nodes-n,
        }
    }

    fn idx_of_leaf(&self, i: usize) -> usize {
        self.leaf_off + i
    }

    pub fn check<V:Value>(&mut self, val: &V, id: usize, proof: &[T]) -> bool {
        assert!(id < self.n,
                format!("check index out of bounds: asked for {} out of {}", id, self.n));
        let mut i = id;
        let height = (self.n as f64).log2().ceil() as usize;

        let mut h = hash_leaf(&mut self.hasher, val);
        for k in 1..=height {
            if (i & 0b1) == 0 {
                h = hash_nodes(&mut self.hasher, h, proof[k-1]);
            } else {
                h = hash_nodes(&mut self.hasher, proof[k-1], h);
            }
            i = i >> 1;
        }

        h == self.nodes[0]
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.nodes[0].to_bytes_le().to_vec()
    }

    pub fn concat_with_msg(&self, msg: &[u8]) -> Vec<u8> {
        let mut msgp = msg.to_vec();
        let mut bytes = self.to_bytes();
        msgp.append(&mut bytes);

        msgp
    }

    pub fn get_path(&self, i: usize) -> Vec<T> {
        assert!(i < self.n,
                format!("Proof index out of bounds: asked for {} out of {}", i, self.n));
        let mut idx = self.idx_of_leaf(i);
        let mut proof = Vec::new();

        while idx > 0 {
            proof.push(self.nodes[sibling(idx)]);
            idx = parent(idx);
        }

        proof
    }
}

fn hash_leaf<'a, V:Value>(hasher: &mut PoseidonHasher<'a>, leaf: &V) -> T {
    leaf.as_scalar(hasher)
}

fn hash_nodes<'a>(hasher: &mut PoseidonHasher<'a>, left: T, right: T) -> T {
    hasher.reset();
    hasher.input(left).unwrap();
    hasher.input(right).unwrap();
    hasher.hash()
}

fn parent(i: usize) -> usize {
    assert!(i > 0, "The root node does not have a parent");
    (i - 1)/2
}

fn left_child(i: usize) -> usize {
    (2*i) + 1
}

fn right_child(i: usize) -> usize {
    (2*i) + 2
}

fn sibling(i: usize) -> usize {
    assert!(i > 0, "The root node does not have a sibling");
    // In the heap representation, the left sibling is always odd
    // And the right sibling is the next node
    // We're assuming that the heap is complete
    if i % 2 == 1 { i + 1 } else { i - 1 }
}


impl Value for u64 {
    fn as_scalar<'a>(&self, hasher: &mut Poseidon<'a, Bls12>) -> Scalar {
        hasher.reset();
        hasher.input(scalar_from_u64(*self)).unwrap();
        hasher.hash()
    }
}

impl Value for [u64; 4] {
    fn as_scalar<'a>(&self, hasher: &mut Poseidon<'a, Bls12>) -> Scalar {
        hasher.reset();
        hasher.input(scalar_from_u64(self[0])).unwrap();
        hasher.input(scalar_from_u64(self[1])).unwrap();
        let h1 = hasher.hash();
        hasher.reset();
        hasher.input(scalar_from_u64(self[2])).unwrap();
        hasher.input(scalar_from_u64(self[3])).unwrap();
        let h2 = hasher.hash();
        hasher.reset();
        hasher.input(h1).unwrap();
        hasher.input(h2).unwrap();
        hasher.hash()
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]
    #[test]
    fn test_create_proof(
        values in (1..10)
            .prop_flat_map(|height| {
                prop::collection::vec(
                    any::<u64>(),
                    (2 as usize).pow(height as u32))
            })
    ) {
        println!("Input generated");
        let constants: PoseidonConstants<Bls12, typenum::U2> = PoseidonConstants::new();
        let mut t = MerkleTree::new(&constants, &values);
        println!("Values : {:?}", values.len());
        for i in 0..values.len() {
            let pf = t.get_path(i as usize);
            assert!(t.check(&values[i], i as usize, &pf));
        }
    }
}
