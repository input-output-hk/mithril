use crate::Path;
use blstrs::Bls12;
use neptune::poseidon::{Poseidon, PoseidonConstants};
use neptune::{scalar_from_u64, Scalar, Arity};
use proptest::prelude::*;

pub type Hash                   = neptune::Scalar;
pub type MerkleHasher<'a,A>     = Poseidon<'a, Bls12, A>;
pub type MerkleHashConstants<A> = PoseidonConstants<Bls12,A>;

// This is parameterized by arity: it may be better to use something other
// than U2 for injecting values into `Scalar`.
pub struct MerkleTree<'a, A = typenum::U2>
where
    A: Arity<Scalar> + typenum::IsGreaterOrEqual<typenum::U2>
{
    // The nodes are stored in an array heap:
    // nodes[0] is the root,
    // the parent of nodes[i] is nodes[(i-1)/2]
    // the children of nodes[i] are {nodes[2i + 1], nodes[2i + 2]}
    nodes: Vec<Hash>,

    // The leaves begin at nodes[leaf_off]
    leaf_off: usize,

    // Number of leaves cached here
    n: usize,

    // The Poseidon hash state
    hasher: Poseidon<'a, Bls12, A>,
}


pub fn new_constants<A:Arity<Scalar>>() -> MerkleHashConstants<A> {
    PoseidonConstants::new()
}


// This is parameterized by arity: it may be better to use something other
// than U2 for injecting values into `Scalar`.
pub trait Value<A>
where A:Arity<Scalar>
{
    fn as_scalar<'a>(&self, hasher: &mut Poseidon<'a, Bls12, A>) -> Hash;
}

impl<'a, A> MerkleTree<'a, A>
where
    A: Arity<Scalar> + typenum::IsGreaterOrEqual<typenum::U2>,
{
    pub fn create<V:Value<A>>(constants: &'a PoseidonConstants<Bls12, A>,
                              leaves: &[V]) -> MerkleTree<'a, A>
    where
        V: Value<A>
    {

        let mut hasher: Poseidon<'a, Bls12, A> = Poseidon::new(&constants);
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
            nodes: nodes,
            hasher: hasher,
            n: n,
            leaf_off: num_nodes-n,
        }
    }

    /// Check an inclusion proof that `val` is the `i`th leaf stored in the tree.
    /// Requires i < self.n
    pub fn check<V:Value<A>>(&mut self, val: &V, i: usize, proof: &[Hash]) -> bool {
        assert!(i < self.n,
                "check index out of bounds: asked for {} out of {}", i, self.n);
        let mut idx = i;
        let height = (self.n as f64).log2().ceil() as usize;

        let mut h = hash_leaf(&mut self.hasher, val);
        for k in 1..=height {
            if (idx & 0b1) == 0 {
                h = hash_nodes(&mut self.hasher, h, proof[k-1]);
            } else {
                h = hash_nodes(&mut self.hasher, proof[k-1], h);
            }
            idx = idx >> 1;
        }

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
        assert!(i < self.n,
                "Proof index out of bounds: asked for {} out of {}", i, self.n);
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

fn hash_leaf<'a, A, V>(hasher: &mut Poseidon<'a, Bls12, A>, leaf: &V) -> Hash
where
    A: Arity<Scalar>,
    V: Value<A>
{
    leaf.as_scalar(hasher)
}

fn hash_nodes<'a, A>(hasher: &mut Poseidon<'a, Bls12, A>, left: Hash, right: Hash) -> Hash
where
    A: Arity<Scalar>,
{
    hasher.reset();
    hasher.input(left).unwrap();
    hasher.input(right).unwrap();
    hasher.hash()
}

fn hash_binary<'a, A>(hasher: &mut Poseidon<'a, Bls12, A>, left: Hash, right: Hash) -> Hash
where
    A: Arity<Scalar> + typenum::IsGreaterOrEqual<typenum::U2>,
{
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

/////////////////////
// Value Instances //
/////////////////////

impl<A> Value<A> for u64
where
    A: Arity<Scalar>,
{
    fn as_scalar<'a>(&self, hasher: &mut Poseidon<'a, Bls12, A>) -> Scalar {
        hasher.reset();
        hasher.input(scalar_from_u64(*self)).unwrap();
        hasher.hash()
    }
}

impl<A> Value<A> for Scalar
where
    A: Arity<Scalar>,
{
    fn as_scalar<'a>(&self, hasher: &mut Poseidon<'a, Bls12, A>) -> Scalar {
        *self
    }
}

impl<A> Value<A> for Vec<u64>
where
    A: Arity<Scalar> + typenum::IsGreaterOrEqual<typenum::U2>,
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        assert!(self.len() > 0, "Can not convert empty slice to Hash");
        let mut h = self[0].as_scalar(hasher);
        for val in self {
            h = (h, val.as_scalar(hasher)).as_scalar(hasher);
        }

        h
    }
}

impl<A,V1,V2> Value<A> for (V1, V2)
where
    A: Arity<Scalar> + typenum::IsGreaterOrEqual<typenum::U2>,
    V1: Value<A>,
    V2: Value<A>
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        let h1 = self.0.as_scalar(hasher);
        let h2 = self.1.as_scalar(hasher);
        hasher.reset();
        hasher.input(h1).unwrap();
        hasher.input(h2).unwrap();
        hasher.hash()
    }
}

impl<V,A> Value<A> for Option<V>
where
    A: Arity<Scalar> + typenum::IsGreaterOrEqual<typenum::U2>,
    V: Value<A>
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        if let Some(inner) = self {
            inner.as_scalar(hasher)
        } else {
            0u64.as_scalar(hasher)
        }
    }
}

impl<A> Value<A> for blstrs::G1Affine
where
    A: Arity<Scalar> + typenum::IsGreaterOrEqual<typenum::U2>,
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        let x = blstrs::FpRepr::from(self.x()).0.to_vec();
        let y = blstrs::FpRepr::from(self.y()).0.to_vec();

        (x,y).as_scalar(hasher)
    }
}

/////////////////////
// Testing         //
/////////////////////

proptest! {
    // Test the relation that t.get_path(i) is a valid
    // proof for i
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
        let constants : PoseidonConstants<Bls12, typenum::U2> = PoseidonConstants::new();
        let mut t = MerkleTree::create(&constants, &values);
        for i in 0..values.len() {
            let Path(pf) = t.get_path(i as usize);
            assert!(t.check(&values[i], i as usize, &pf));
        }
    }
}
