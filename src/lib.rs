#![allow(dead_code, unused_variables)]

pub mod party;
pub mod key_reg;
pub mod ref_str;
pub mod msp;
pub mod merkle_tree;
pub mod proof;

pub type Unknown = usize;

#[derive(Clone, PartialEq, Copy)]
pub struct Stake(pub f32);

pub type PartyId = usize;

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct Index(pub Unknown);

#[derive(Clone, PartialEq, Eq)]
pub struct Path(Vec<merkle_tree::Hash>);

// Writen as phi in the paper
// XXX: What should it be implemented as?
pub fn scaling_function(stake: Stake) -> Unknown {
    unimplemented!()
}
