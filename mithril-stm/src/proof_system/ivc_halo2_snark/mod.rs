use midnight_curves::Bls12;
use midnight_proofs::{
    plonk::{ProvingKey, VerifyingKey},
    poly::kzg::KZGCommitmentScheme,
};

use crate::circuits::halo2::types::CircuitBase;

pub(crate) mod errors;
pub(crate) mod proof;
mod prover_input;
mod prover_setup;
pub(crate) mod rolling_state;
pub(crate) mod temp_ivc_prover_cache;
mod unsafe_setup_helpers;
pub(crate) mod verifier_setup;

pub(crate) use prover_setup::IvcProverSetup;
pub(crate) use temp_ivc_prover_cache::load_ivc_prover_setup;
pub(crate) use unsafe_setup_helpers::{TempCertificateKeyProvider, TempIvcKeyProvider};

/// PLONK verifying key used across the IVC setup
/// (certificate and recursive circuits share the same field/curve parameterization).
pub(crate) type CircuitVerifyingKey = VerifyingKey<CircuitBase, KZGCommitmentScheme<Bls12>>;
/// PLONK proving key used by the IVC circuit.
pub(crate) type CircuitProvingKey = ProvingKey<CircuitBase, KZGCommitmentScheme<Bls12>>;
