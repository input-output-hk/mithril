//! `IvcProver` and `IvcProof`: the proving-session handle and its emitted IVC proof.

use std::sync::Arc;

use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};
use rand_core::{CryptoRng, RngCore};

use crate::{circuits::halo2_ivc::state::State, proof_system::ivc_halo2_snark::setup::IvcSetup};

/// Per-session IVC prover handle.
// TODO: remove this allow dead_code directive when the IVC prover is wired into STM
#[allow(dead_code)]
pub(crate) struct IvcProver<R: RngCore + CryptoRng> {
    /// Shared, cached setup (SRS, verifying keys, proving key, fixed-base maps).
    pub(crate) ivc_setup: Arc<IvcSetup>,
    /// Randomness source used during proof generation.
    pub(crate) rng: R,
}

/// IVC proof emitted at the end of a proving step.
// TODO: remove this allow dead_code directive when the IVC prover emits this proof
#[allow(dead_code)]
pub(crate) struct IvcProof {
    /// Externally-verifiable proof bytes (Blake2b transcript).
    pub(crate) proof_bytes: Vec<u8>,
    /// Chain state the proof commits to.
    pub(crate) state: State,
    /// Folded accumulator the proof commits to.
    pub(crate) accumulator: Accumulator<BlstrsEmulation>,
}
