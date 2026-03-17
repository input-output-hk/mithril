//! Circuit witness and instance boundary types for the Halo2 STM circuit.
//!
//! This module defines the exact public/private shapes consumed by the relation, while
//! keeping them separate from lower-level circuit primitives in `types.rs`.

use ff::Field;

use crate::LotteryIndex;
use crate::signature_scheme::{
    PrimeOrderProjectivePoint, SchnorrVerificationKey, UniqueSchnorrSignature,
};

use super::types::{CircuitBase, CircuitBaseField};

/// Lottery target value used by the circuit for signer eligibility checks.
pub type LotteryTargetValue = CircuitBaseField;
/// Signed message value used by the circuit transcript, without any domain prefix.
pub type SignedMessageWithoutPrefix = CircuitBaseField;
/// Merkle root public input committed by the STM membership commitment tree.
pub type MerkleRoot = CircuitBaseField;
/// Circuit statement/instance type, representing the public inputs to the STM SNARK circuit.
pub(crate) type CircuitInstance = (MerkleRoot, SignedMessageWithoutPrefix);

/// Merkle-tree leaf material used by Halo2 witness construction.
///
/// The first field stores the signer's verification key, and the second
/// field stores the lottery target value associated with that signer.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CircuitMerkleTreeLeaf(pub SchnorrVerificationKey, pub LotteryTargetValue);

impl CircuitMerkleTreeLeaf {
    pub fn verification_key(&self) -> SchnorrVerificationKey {
        self.0
    }

    pub fn verification_key_point(&self) -> PrimeOrderProjectivePoint {
        self.0.0
    }

    pub fn lottery_target_value(&self) -> LotteryTargetValue {
        self.1
    }
}

/// Position of a sibling node relative to the current hash in a Merkle path.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Position {
    Left,
    Right,
}

impl From<Position> for CircuitBase {
    fn from(position: Position) -> Self {
        match position {
            Position::Left => Self::ZERO,
            Position::Right => Self::ONE,
        }
    }
}

impl From<Position> for CircuitBaseField {
    fn from(position: Position) -> Self {
        CircuitBase::from(position).into()
    }
}

/// Merkle authentication path used by the Halo2 circuit witness.
///
/// Each entry stores sibling position and sibling hash value for one tree level.
#[derive(Clone, Debug)]
pub struct MerklePath {
    /// Ordered list of `(position, sibling_hash)` from leaf level to root level.
    pub siblings: Vec<(Position, CircuitBaseField)>,
}

impl MerklePath {
    /// Creates a new Merkle path from ordered sibling entries.
    pub fn new(siblings: Vec<(Position, CircuitBaseField)>) -> Self {
        Self { siblings }
    }
}

/// Single circuit witness entry consumed by the Halo2 relation.
#[derive(Clone, Debug)]
pub struct CircuitWitnessEntry {
    pub(crate) leaf: CircuitMerkleTreeLeaf,
    pub(crate) merkle_path: MerklePath,
    pub(crate) unique_schnorr_signature: UniqueSchnorrSignature,
    pub(crate) lottery_index: LotteryIndex,
}

/// Full circuit witness consumed by the Halo2 relation.
pub(crate) type CircuitWitness = Vec<CircuitWitnessEntry>;
