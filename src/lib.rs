#![allow(dead_code, unused_variables)]

pub mod party;
pub mod key_reg;
pub mod ref_str;
pub mod msp;
pub mod merkle_tree;
pub mod proof;

use rand::{Rng, rngs::OsRng};
use neptune::poseidon::Arity;
use merkle_tree::{MerkleHasher, Value, Hash};

pub type Unknown = usize;

pub type Stake = u64;

pub type PartyId = usize;

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct Index(u64);

impl Index {
    pub fn random() -> Self {
        Self(OsRng::default().gen())
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Path(Vec<Hash>);

// Writen as phi in the paper
pub fn ev_lt_phi(ev: u64, stake: Stake, total_stake: Stake) -> bool {
    unimplemented!()
}


impl<A> Value<A> for msp::PK
where
    A: Arity<Hash> + typenum::IsGreaterOrEqual<typenum::U2>
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        unimplemented!()
    }
}
