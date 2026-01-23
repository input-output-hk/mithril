//! Hash helpers and CPU hashing interface for the Halo2 prototype.

pub use midnight_circuits::instructions::HashToCurveCPU;
pub use midnight_circuits::instructions::hash::HashCPU;
use midnight_circuits::{
    ecc::hash_to_curve::HashToCurveGadget, ecc::native::EccChip, hash::poseidon::PoseidonChip,
    types::AssignedNative,
};

use crate::circuits::halo2::types::{Jubjub, JubjubBase};

pub type PoseidonHash = PoseidonChip<JubjubBase>;
pub type JubjubHashToCurve = HashToCurveGadget<
    JubjubBase,
    Jubjub,
    AssignedNative<JubjubBase>,
    PoseidonChip<JubjubBase>,
    EccChip<Jubjub>,
>;
