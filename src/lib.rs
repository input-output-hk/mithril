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

#[derive(Clone, PartialEq, Copy)]
pub struct Phi(f64);

// Writen as phi in the paper
pub fn ev_lt_phi(phi: Phi, ev: f64, stake: Stake, total_stake: Stake) -> bool {
    //TODO: Fix this, casting to f64 isn't safe
    let w = (stake as f64) / (total_stake as f64);

    ev < 1.0 - (1.0 - phi.0).powf(w)
}

impl<A> Value<A> for blstrs::G1Affine
where
    A: Arity<Hash> + typenum::IsGreaterOrEqual<typenum::U2>,
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        let x = blstrs::FpRepr::from(self.x()).0.to_vec();
        let y = blstrs::FpRepr::from(self.y()).0.to_vec();

        (x,y).as_scalar(hasher)
    }
}

impl<A> Value<A> for blstrs::FpRepr
where
    A: Arity<Hash> + typenum::IsGreaterOrEqual<typenum::U2>,
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        self.0.to_vec().as_scalar(hasher)
    }
}

impl<A> Value<A> for blstrs::Fp
where
    A: Arity<Hash> + typenum::IsGreaterOrEqual<typenum::U2>,
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        blstrs::FpRepr::from(*self).as_scalar(hasher)
    }
}

impl<A> Value<A> for blstrs::Fp2
where
    A: Arity<Hash> + typenum::IsGreaterOrEqual<typenum::U2>,
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        vec![self.c0(), self.c1()].as_scalar(hasher)
    }
}

impl<A> Value<A> for blstrs::G1Projective
where
    A: Arity<Hash> + typenum::IsGreaterOrEqual<typenum::U2>,
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        vec![self.x(), self.y(), self.z()].as_scalar(hasher)
    }
}

impl<A> Value<A> for blstrs::G2Projective
where
    A: Arity<Hash> + typenum::IsGreaterOrEqual<typenum::U2>,
{
    fn as_scalar<'a>(&self, hasher: &'a mut MerkleHasher<A>) -> Hash {
        vec![self.x(), self.y(), self.z()].as_scalar(hasher)
    }
}
