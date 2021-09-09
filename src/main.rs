#![allow(dead_code, unused_variables)]

mod party;
mod key_reg;
mod ref_str;
mod msp;
mod merkle_tree;
mod proof;

pub type Unknown = usize;

#[derive(Clone, PartialEq, Copy)]
pub struct Stake(f32);

pub type PartyId = usize;

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct Index(Unknown);

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct Path(Unknown);

// Writen as phi in the paper
// XXX: What should it be implemented as?
pub fn scaling_function(stake: Stake) -> Unknown {
    unimplemented!()
}

fn main() {
    println!("Hello, world!");
}
