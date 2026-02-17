//! Halo2-facing type aliases and witness shapes for the STM SNARK circuit.
//!
//! This module bridges STM domain concepts (message, lottery index, Merkle proof)
//! to circuit-oriented types consumed by the Halo2 relation and gadgets.

use ff::Field;

pub use midnight_curves::{Bls12, Fq as JubjubBase, Fr as JubjubScalar, JubjubExtended as Jubjub};

/// Lottery threshold value used by the circuit for signer eligibility checks.
pub type Target = JubjubBase;
/// Signed message value used by the circuit transcript, without any domain prefix.
pub type SignedMessageWithoutPrefix = JubjubBase;
/// Merkle root public input committed by the STM membership commitment tree.
pub type MerkleRoot = JubjubBase;
/// Lottery index (`i`) used for per-lottery checks in witness entries.
pub type LotteryIndex = u32;

/// Merkle-tree leaf material used by Halo2 witness construction.
///
/// The first field stores the serialized verification key bytes, and the second
/// field stores the lottery target value associated with that signer.
#[derive(Debug, Copy, Clone)]
pub struct MTLeaf(pub [u8; 64], pub Target);

/// Position of a sibling node relative to the current hash in a Merkle path.
#[derive(Clone, Copy, Debug)]
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

/// Merkle authentication path used by the Halo2 circuit witness.
///
/// Each entry stores sibling position and sibling hash value for one tree level.
#[derive(Clone, Debug)]
pub struct MerklePath {
    /// Ordered list of `(position, sibling_hash)` from leaf level to root level.
    pub siblings: Vec<(Position, JubjubBase)>,
}

impl MerklePath {
    /// Creates a new Merkle path from ordered sibling entries.
    pub fn new(siblings: Vec<(Position, JubjubBase)>) -> Self {
        Self { siblings }
    }
}
