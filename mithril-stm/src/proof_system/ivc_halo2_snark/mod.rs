use midnight_curves::Bls12;
use midnight_proofs::{
    plonk::{ProvingKey, VerifyingKey},
    poly::kzg::KZGCommitmentScheme,
};

use crate::circuits::halo2::types::CircuitBase;

pub(crate) mod errors;
pub(crate) mod proof;
mod prover_input;
mod rolling_state;
mod setup;
mod unsafe_setup_helpers;
pub(crate) mod verifier_setup;

/// PLONK verifying key used across the IVC setup
/// (certificate and recursive circuits share the same field/curve parameterization).
pub(crate) type CircuitVerifyingKey = VerifyingKey<CircuitBase, KZGCommitmentScheme<Bls12>>;
/// PLONK proving key used by the IVC circuit.
pub(crate) type CircuitProvingKey = ProvingKey<CircuitBase, KZGCommitmentScheme<Bls12>>;
