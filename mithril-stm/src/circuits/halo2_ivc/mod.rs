//! Halo2 IVC circuit prototype integration (feature-gated by `future_snark`).
//!
//! This module is the landing zone for the recursive SNARK / IVC prototype
//! while it is being moved into `mithril-stm`.
//!
//! The code in this module is moved from the standalone recursive prototype and
//! is kept locally here until the recursive circuit is wired into STM.

pub(crate) use midnight_curves::JubjubExtended as Jubjub;

pub(crate) use midnight_circuits::{
    ecc::{
        curves::CircuitCurve,
        foreign::{ForeignEccChip, ForeignEccConfig, nb_foreign_ecc_chip_columns},
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
    hash::sha256::{NB_SHA256_ADVICE_COLS, NB_SHA256_FIXED_COLS},
    instructions::{
        ArithInstructions, AssertionInstructions, AssignmentInstructions, BinaryInstructions,
        ControlFlowInstructions, ConversionInstructions, EccInstructions, EqualityInstructions,
        HashInstructions, PublicInputInstructions, ZeroInstructions,
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

pub(crate) mod certificate_proof;
pub(crate) mod circuit;
pub(crate) mod config;
pub(crate) mod errors;
pub(crate) mod gadget;
pub(crate) mod io;
#[cfg(test)]
pub(crate) mod protocol_message;
pub(crate) mod state;
pub(crate) mod types;

#[cfg(test)]
pub(crate) mod tests;

pub(crate) use types::ProtocolMessagePreimage;

type S = BlstrsEmulation;
type F = <S as SelfEmulation>::F;
type C = <S as SelfEmulation>::C;

type E = <S as SelfEmulation>::Engine;
type CBase = <C as CircuitCurve>::Base;

type NG = NativeGadget<F, P2RDecompositionChip<F>, NativeChip<F>>;

// Degree of the recursive circuit
pub(crate) const K: u32 = 19;

pub const PREIMAGE_SIZE: usize = 190;
/// Byte range of the next Merkle-tree commitment within the protocol message preimage.
pub const PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES: std::ops::Range<usize> = 69..101;
/// Byte range of the next protocol parameters within the protocol message preimage.
pub const PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES: std::ops::Range<usize> = 137..169;
/// Byte range of the current epoch within the protocol message preimage.
pub const PREIMAGE_CURRENT_EPOCH_BYTES: std::ops::Range<usize> = 182..190;

pub(crate) const CERTIFICATE_VERIFICATION_KEY_NAME: &str = "cert_vk";
pub(crate) const IVC_VERIFICATION_KEY_NAME: &str = "ivc_one_vk";

/// Circuit verification key of the recursive circuit used for production.
/// It is created using the circuit verification key of the non-recursive
/// circuit and the SRS from Midnight's power of tau ceremony
pub const RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION: &[u8] =
    include_bytes!("recursive_circuit_verification_key_for_production.bin");
