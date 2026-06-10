//! `IvcProver` and `IvcProof`: the proving-session handle and its emitted IVC proof.

use std::{marker::PhantomData, sync::Arc};

use group::Group;
use midnight_circuits::{
    types::Instantiable,
    verifier::{Accumulator, AssignedAccumulator, BlstrsEmulation},
};
use midnight_curves::{Bls12, G1Projective};
use midnight_proofs::{
    plonk::prepare,
    poly::{commitment::PolynomialCommitmentScheme, kzg::KZGCommitmentScheme},
    transcript::{CircuitTranscript, Hashable, Sampleable, Transcript, TranscriptHash},
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
    proof_system::ivc_halo2_snark::{setup::IvcSetup, verifier_setup::IvcVerifierSetup},
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
///
/// `H` is the transcript hash used to produce this proof and must be used to verify it.
/// It is a zero-cost phantom: no `H`-dependent data is stored, but it prevents accidentally
/// verifying a Poseidon-produced proof via the Blake2b path and vice versa.
// TODO: remove this allow dead_code directive when the IVC prover emits this proof
#[allow(dead_code)]
pub(crate) struct IvcProof<H> {
    /// Externally-verifiable proof bytes.
    pub(crate) proof_bytes: IvcProofBytes,
    /// Chain state the proof commits to.
    pub(crate) state: State,
    /// Folded accumulator the proof commits to.
    pub(crate) accumulator: Accumulator<BlstrsEmulation>,
    /// Phantom marker tying the proof to its transcript hash type.
    pub(crate) _hash: PhantomData<H>,
}

// TODO: remove this allow dead_code directive when IvcProof::verify is called
#[allow(dead_code)]
impl<H> IvcProof<H>
where
    H: TranscriptHash,
    CircuitBase: Sampleable<H> + Hashable<H>,
    <KZGCommitmentScheme<Bls12> as PolynomialCommitmentScheme<CircuitBase>>::Commitment:
        Hashable<H>,
{
    /// Verifies the IVC proof and its folded accumulator using transcript hash `H`.
    ///
    /// Checks:
    /// 1. The KZG opening equations against the IVC verifying key.
    /// 2. The folded accumulator pairing equation.
    pub(crate) fn verify(
        &self,
        global: &Global,
        verifier_setup: &IvcVerifierSetup,
    ) -> StmResult<()> {
        let public_inputs: Vec<CircuitBase> = [
            global.as_public_input(),
            self.state.as_public_input(),
            AssignedAccumulator::as_public_input(&self.accumulator),
        ]
        .concat();

        let mut transcript = CircuitTranscript::<H>::init_from_bytes(self.proof_bytes.as_bytes());

        let dual_msm = prepare::<CircuitBase, KZGCommitmentScheme<Bls12>, CircuitTranscript<H>>(
            &verifier_setup.ivc_verifying_key,
            &[&[G1Projective::identity()]],
            &[&[&public_inputs]],
            &mut transcript,
        )
        .map_err(|_| IvcCircuitError::RecursiveProofTranscriptPreparationFailed)?;

        transcript
            .assert_empty()
            .map_err(|_| IvcCircuitError::RecursiveProofTranscriptNotFullyConsumed)?;

        if !dual_msm.check(&verifier_setup.verifier_params) {
            return Err(IvcCircuitError::RecursiveProofKzgOpeningFailed.into());
        }

        if !self.accumulator.check(&verifier_setup.tau_g2, &verifier_setup.combined_fixed_bases) {
            return Err(IvcCircuitError::RecursiveProofAccumulatorFailed.into());
        }

        Ok(())
    }
}
