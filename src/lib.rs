#![allow(dead_code, unused_variables)]

pub mod party;
pub mod key_reg;
pub mod ref_str;
pub mod msp;
pub mod merkle_tree;
pub mod proof;

use rand::{Rng, rngs::OsRng};
use merkle_tree::Hash;

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

