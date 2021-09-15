#![allow(dead_code, unused_variables)]

pub mod party;
pub mod key_reg;
pub mod ref_str;
pub mod msp;
pub mod merkle_tree;
pub mod proof;

use neptune::poseidon::Arity;
use merkle_tree::{MerkleHasher, Value, Hash};

pub type Unknown = usize;

#[derive(Clone, PartialEq, Copy)]
pub struct Stake(pub f32);

pub type PartyId = usize;

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct Index(pub u64);

#[derive(Clone, PartialEq, Eq)]
pub struct Path(Vec<Hash>);

// Writen as phi in the paper
// XXX: What should it be implemented as?
pub fn scaling_function(stake: Stake) -> u64 {
    unimplemented!()
}


impl<A> Value<A> for Stake
where
    A: Arity<Hash> + typenum::IsGreaterOrEqual<typenum::U2>
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        let Stake(s) = self;
        let bits = s.to_bits() as u64;
        bits.as_scalar(hasher)
    }
}

impl<A> Value<A> for msp::PK
where
    A: Arity<Hash> + typenum::IsGreaterOrEqual<typenum::U2>
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        unimplemented!()
    }
}
