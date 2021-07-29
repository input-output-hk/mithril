use super::{Stake, Unknown};
use crate::msp::PK;

pub struct MerkleTree { }

impl MerkleTree {
    pub fn create(items: &[Option<(PK, Stake)>]) -> Self {
        unimplemented!()
    }

    pub fn check(&self, item: Unknown, index: Unknown, path: &[Unknown]) -> bool {
        unimplemented!()
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        unimplemented!()
    }
}
