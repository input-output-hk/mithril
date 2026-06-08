//! `IvcProver` and `IvcProof`: the proving-session handle and its emitted IVC proof.

use std::sync::Arc;

use group::Group;
use midnight_circuits::{
    types::Instantiable,
    verifier::{Accumulator, AssignedAccumulator, BlstrsEmulation},
};
use midnight_curves::{Bls12, G1Projective, G2Affine};
use midnight_proofs::{
    plonk::prepare,
    poly::kzg::KZGCommitmentScheme,
    transcript::{CircuitTranscript, Transcript},
};
use rand_core::{CryptoRng, RngCore};

use crate::{
    StmResult,
    circuits::{
        halo2::types::CircuitBase,
        halo2_ivc::{
            errors::IvcCircuitError,
            state::{Global, State},
            types::IvcProofBytes,
        },
    },
    proof_system::ivc_halo2_snark::setup::IvcSetup,
};

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
    pub(crate) proof_bytes: IvcProofBytes,
    /// Chain state the proof commits to.
    pub(crate) state: State,
    /// Folded accumulator the proof commits to.
    pub(crate) accumulator: Accumulator<BlstrsEmulation>,
}

// TODO: remove this allow dead_code directive when IvcProof::verify is called
#[allow(dead_code)]
impl IvcProof {
    /// Verifies the IVC proof (Blake2b transcript) and its folded accumulator.
    ///
    /// This is the externally-verifiable form of the proof emitted at the end of
    /// each proving step. It checks:
    /// 1. The Blake2b KZG opening equations against the IVC verifying key.
    /// 2. The folded accumulator pairing equation.
    pub(crate) fn verify_blake2b(&self, global: &Global, ivc_setup: &IvcSetup) -> StmResult<()> {
        let public_inputs: Vec<CircuitBase> = [
            global.as_public_input(),
            self.state.as_public_input(),
            AssignedAccumulator::as_public_input(&self.accumulator),
        ]
        .concat();

        let mut transcript =
            CircuitTranscript::<blake2b_simd::State>::init_from_bytes(self.proof_bytes.as_bytes());

        let dual_msm = prepare::<
            CircuitBase,
            KZGCommitmentScheme<Bls12>,
            CircuitTranscript<blake2b_simd::State>,
        >(
            &ivc_setup.ivc_verifying_key,
            &[&[G1Projective::identity()]],
            &[&[&public_inputs]],
            &mut transcript,
        )
        .map_err(|_| IvcCircuitError::RecursiveProofTranscriptPreparationFailed)?;

        transcript
            .assert_empty()
            .map_err(|_| IvcCircuitError::RecursiveProofTranscriptNotFullyConsumed)?;

        if !dual_msm.check(&ivc_setup.srs_verifier_params) {
            return Err(IvcCircuitError::RecursiveProofKzgOpeningFailed.into());
        }

        let tau_g2: G2Affine = ivc_setup.srs_verifier_params.s_g2().into();
        if !self.accumulator.check(&tau_g2, &ivc_setup.combined_fixed_bases) {
            return Err(IvcCircuitError::RecursiveProofAccumulatorFailed.into());
        }

        Ok(())
    }
}
