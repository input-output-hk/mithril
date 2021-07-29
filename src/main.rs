#![allow(dead_code, unused_variables)]

mod party;
mod key_reg;
mod ref_str;
mod msp;
mod merkle_tree;

pub type Unknown = usize;

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct Stake(Unknown);

pub type PartyId = usize;

// Writen as phi in the paper
// XXX: What should it be implemented as?
pub fn scaling_function(stake: Stake) -> Unknown {
    stake.0
}

fn main() {
    println!("Hello, world!");
}
