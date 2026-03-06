//! Internal re-exports for Midnight backend traits/types used by Halo2 modules.
//!
//! This keeps `circuit.rs` and `gadgets.rs` free of direct `midnight_*` imports.

pub(crate) use midnight_circuits::ecc::curves::CircuitCurve as CircuitCurveTrait;
pub(crate) use midnight_circuits::instructions::{
    ArithInstructions, AssertionInstructions, AssignmentInstructions, BinaryInstructions,
    ControlFlowInstructions, ConversionInstructions, DecompositionInstructions, EccInstructions,
    EqualityInstructions, PublicInputInstructions,
};
pub(crate) use midnight_circuits::types::{
    AssignedBit, AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve,
};
#[cfg(test)]
pub(crate) use midnight_curves::Bls12;
pub(crate) use midnight_curves::{Fq as BackendJubjubBase, JubjubExtended as BackendJubjub};
pub(crate) use midnight_proofs::circuit::{Layouter, Value};
pub(crate) use midnight_proofs::plonk::Error;
pub(crate) use midnight_zk_stdlib::{Relation, ZkStdLib, ZkStdLibArch};

#[cfg(test)]
pub(crate) mod test_reexports {
    pub(crate) use midnight_circuits::{hash::poseidon::PoseidonChip, instructions::hash::HashCPU};
}
