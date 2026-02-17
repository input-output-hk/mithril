//! Reexports and type aliases for the Halo2 prototype (kept close to mithril-circuits to minimize diff).

use ff::Field;

pub use midnight_curves::{Bls12, Fq as JubjubBase, Fr as JubjubScalar, JubjubExtended as Jubjub};

pub type Target = JubjubBase;
pub type SignedMessageWithoutPrefix = JubjubBase;
pub type MerkleRoot = JubjubBase;
pub type LotteryIndex = u32;

// Merkle witness types

#[derive(Debug, Copy, Clone)]
pub struct MTLeaf(pub [u8; 64], pub Target);

#[derive(Clone, Copy, Debug)]
// The position of the sibling in the tree.
pub enum Position {
    Left,
    Right,
}

impl From<Position> for JubjubBase {
    fn from(value: Position) -> Self {
        match value {
            Position::Left => JubjubBase::ZERO,
            Position::Right => JubjubBase::ONE,
        }
    }
}

#[derive(Clone, Debug)]
// Struct defining the witness of the MT proof.
pub struct MerklePath {
    // Sibling nodes corresponding to a field value F representing some
    // hash and whether the position is left or right.
    // if position == Position::Left, then sibling is on the left
    // if position == Position::Right, then sibling is on the right
    pub siblings: Vec<(Position, JubjubBase)>,
}

impl MerklePath {
    pub fn new(siblings: Vec<(Position, JubjubBase)>) -> Self {
        Self { siblings }
    }
}
