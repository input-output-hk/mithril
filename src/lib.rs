#![allow(dead_code, unused_variables)]

pub mod stm;
pub mod key_reg;
pub mod msp;
pub mod merkle_tree;
pub mod proof;

pub type Stake = u64;

pub type PartyId = usize;

pub type Index = u64;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Path(Vec<merkle_tree::Hash>);

// Writen as phi in the paper
pub fn ev_lt_phi(phi_f: f64, ev: u64, stake: Stake, total_stake: Stake) -> bool {
    //TODO: Fix this, casting to f64 isn't safe
    let w = (stake as f64) / (total_stake as f64);
    let phi = 1.0 - (1.0 - phi_f).powf(w);
    let ev_as_f64 = ev as f64 / 2_f64.powf(64.0);
    // println!("{} {}", phi, ev_as_f64);
    ev_as_f64 < phi
}

