//! Halo2 IVC circuit prototype integration (feature-gated by `future_snark`).
//!
//! This module is the landing zone for the recursive SNARK / IVC prototype
//! while it is being moved into `mithril-stm`.
//!
//! The code in this module is moved from the standalone recursive prototype and
//! is kept locally here until the recursive circuit is wired into STM.

pub(crate) use midnight_curves::{
    Fq as JubjubBase, Fr as JubjubScalar, JubjubExtended as Jubjub, JubjubSubgroup,
};

pub(crate) use midnight_circuits::{
    ecc::{
        curves::CircuitCurve,
        foreign::{ForeignEccChip, ForeignEccConfig, nb_foreign_ecc_chip_columns},
        hash_to_curve::HashToCurveGadget,
        native::{EccChip, EccConfig, NB_EDWARDS_COLS},
    },
    field::{
        NativeChip, NativeConfig, NativeGadget,
        decomposition::{
            chip::{P2RDecompositionChip, P2RDecompositionConfig},
            pow2range::Pow2RangeChip,
        },
        foreign::FieldChip,
        native::{NB_ARITH_COLS, NB_ARITH_FIXED_COLS},
    },
    hash::poseidon::{
        NB_POSEIDON_ADVICE_COLS, NB_POSEIDON_FIXED_COLS, PoseidonChip, PoseidonConfig,
    },
    instructions::{
        ArithInstructions, AssertionInstructions, AssignmentInstructions, BinaryInstructions,
        ControlFlowInstructions, ConversionInstructions, EccInstructions, EqualityInstructions,
        HashInstructions, HashToCurveCPU, PublicInputInstructions, ZeroInstructions, hash::HashCPU,
    },
    types::{
        AssignedBit, AssignedByte, AssignedForeignPoint, AssignedNative, AssignedNativePoint,
        AssignedScalarOfNativeCurve, ComposableChip, Instantiable,
    },
    verifier::{
        self, Accumulator, AssignedAccumulator, AssignedVk, BlstrsEmulation, Msm, SelfEmulation,
        VerifierGadget,
    },
};

pub(crate) use midnight_proofs::{
    circuit::{Layouter, SimpleFloorPlanner, Value},
    plonk::{Circuit, ConstraintSystem, Error, VerifyingKey},
    poly::{EvaluationDomain, kzg::KZGCommitmentScheme},
};

pub mod circuit;
pub mod config;
pub mod gadget;
pub mod helpers;
pub mod io;
pub mod state;

#[cfg(test)]
pub mod tests;

type S = BlstrsEmulation;
type F = <S as SelfEmulation>::F;
type C = <S as SelfEmulation>::C;

type E = <S as SelfEmulation>::Engine;
type CBase = <C as CircuitCurve>::Base;

type NG = NativeGadget<F, P2RDecompositionChip<F>, NativeChip<F>>;

const K: u32 = 19;

pub const PREIMAGE_SIZE: usize = 190;
/// Byte range of the next Merkle root within the protocol message preimage.
pub const PREIMAGE_NEXT_MERKLE_ROOT_BYTES: std::ops::Range<usize> = 69..101;
/// Byte range of the next protocol parameters within the protocol message preimage.
pub const PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES: std::ops::Range<usize> = 137..169;
/// Byte range of the current epoch within the protocol message preimage.
pub const PREIMAGE_CURRENT_EPOCH_BYTES: std::ops::Range<usize> = 182..190;

pub(crate) const CERT_VK_NAME: &str = "cert_vk";
pub(crate) const IVC_ONE_NAME: &str = "ivc_one_vk";

pub(crate) const DST_UNIQUE_SIGNATURE: JubjubBase = JubjubBase::from_raw([2, 2, 0, 0]);
pub(crate) const DST_SCHNORR_SIGNATURE: JubjubBase = JubjubBase::from_raw([2, 3, 0, 0]);

type JubjubHashToCurve = HashToCurveGadget<
    JubjubBase,
    Jubjub,
    AssignedNative<JubjubBase>,
    PoseidonChip<JubjubBase>,
    EccChip<Jubjub>,
>;

type PoseidonHash = PoseidonChip<JubjubBase>;

pub(crate) type Target = JubjubBase;
