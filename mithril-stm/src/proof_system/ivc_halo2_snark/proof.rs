//! `IvcProver` and `IvcProof`: the proving-session handle and its emitted IVC proof.

use std::{marker::PhantomData, sync::Arc};

use anyhow::anyhow;
use ff::FromUniformBytes;
use midnight_circuits::{
    hash::poseidon::PoseidonState,
    types::Instantiable,
    verifier::{Accumulator, AssignedAccumulator, BlstrsEmulation},
};
use midnight_curves::Bls12;
use midnight_proofs::{
    plonk::create_proof,
    poly::{
        commitment::PolynomialCommitmentScheme,
        kzg::{KZGCommitmentScheme, params::ParamsKZG},
    },
    transcript::{CircuitTranscript, Hashable, Sampleable, Transcript, TranscriptHash},
};
use rand_core::{CryptoRng, RngCore};

use crate::{
    AggregateVerificationKeyForSnark, MembershipDigest, SnarkProof, StmResult,
    circuits::{
        halo2::types::CircuitBase,
        halo2_ivc::{
            circuit::IvcCircuitData,
            state::{Global, State},
            types::{
                CertificateProofBytes, EpochNumber, IvcProofBytes, ProtocolMessagePreimage,
                StepCounter,
            },
        },
    },
    proof_system::ivc_halo2_snark::{
        CircuitProvingKey, prover_input::IvcProverInput, rolling_state::IvcRollingState,
        setup::IvcProvingSetup,
    },
};

/// Per-session IVC prover handle.
// TODO: remove this allow dead_code directive when the IVC prover is wired into STM
#[allow(dead_code)]
pub(crate) struct IvcProver<R: RngCore + CryptoRng> {
    /// Shared, cached setup (SRS, verifying keys, proving key, fixed-base maps).
    pub(crate) ivc_setup: Arc<IvcProvingSetup>,
    /// Randomness source used during proof generation.
    pub(crate) rng: R,
}

/// IVC proof emitted at the end of a proving step.
///
/// The `H` type parameter records the transcript hash used to generate `proof_bytes`.
/// The external verifier-facing proof always uses `H = blake2b_simd::State`.
// TODO: remove this allow dead_code directive when the IVC prover is wired into STM
#[allow(dead_code)]
pub(crate) struct IvcProof<H> {
    /// Externally-verifiable proof bytes generated under transcript hash `H`.
    pub(crate) proof_bytes: IvcProofBytes,
    /// Chain state the proof commits to.
    pub(crate) state: State,
    /// Folded accumulator the proof commits to.
    pub(crate) accumulator: Accumulator<BlstrsEmulation>,
    _phantom: PhantomData<H>,
}

/// Transcript-generic IVC proof generation helper.
///
/// Calls `create_proof` with the committed-instance layout the IVC circuit expects:
/// `&[&[&[], public_inputs]]` (one circuit, one instance group, empty committed
/// instance, then the field-element public inputs). Returns the finalised transcript
/// bytes on success.
fn prove_with_transcript<H: TranscriptHash>(
    srs: &ParamsKZG<Bls12>,
    proving_key: &CircuitProvingKey,
    circuit_data: &IvcCircuitData,
    public_inputs: &[CircuitBase],
    rng: &mut (impl RngCore + CryptoRng),
) -> StmResult<Vec<u8>>
where
    CircuitBase: Sampleable<H> + Hashable<H> + std::hash::Hash + Ord + FromUniformBytes<64>,
    <KZGCommitmentScheme<Bls12> as PolynomialCommitmentScheme<CircuitBase>>::Commitment:
        Hashable<H>,
{
    let mut transcript = CircuitTranscript::<H>::init();
    create_proof::<CircuitBase, KZGCommitmentScheme<Bls12>, CircuitTranscript<H>, IvcCircuitData>(
        srs,
        proving_key,
        std::slice::from_ref(circuit_data),
        1,
        &[&[&[], public_inputs]],
        rng,
        &mut transcript,
    )
    .map_err(|e| anyhow!("IVC proof generation failed: {e}"))?;
    Ok(transcript.finalize())
}

// TODO: remove this allow dead_code directive when the IVC prover is wired into STM
#[allow(dead_code)]
impl<R: RngCore + CryptoRng> IvcProver<R> {
    /// Advances the IVC chain by one step.
    ///
    /// Internally calls [`IvcProverInput::prepare`] to fold accumulators and build the
    /// witness, then generates the proofs required by the step type:
    ///
    /// - **Genesis** (`step_counter == 0`): two proofs — a Poseidon proof that initialises
    ///   the rolling state for the next step, and a Blake2b proof returned to the caller.
    /// - **Same-epoch** (`cert_epoch == chain_epoch`): one Blake2b proof only. The rolling
    ///   state is unchanged; the caller retains the existing [`IvcRollingState`].
    /// - **Next-epoch** (`cert_epoch == chain_epoch + 1`): two proofs — a Poseidon proof
    ///   that advances the rolling state, and a Blake2b proof returned to the caller.
    ///
    /// Note: `snark_proof`, `message`, and `aggregate_verification_key` are unused at
    /// genesis — [`IvcProverInput::prepare`] ignores them on the genesis path. A future
    /// refactor may wrap them in `Option<IvcCertificateInput>`.
    pub(crate) fn prove<D: MembershipDigest>(
        &mut self,
        snark_proof: SnarkProof<D>,
        message: &[u8],
        aggregate_verification_key: &AggregateVerificationKeyForSnark<D>,
        global: &Global,
        protocol_message_preimage: &ProtocolMessagePreimage,
        rolling_state: &IvcRollingState,
    ) -> StmResult<(IvcProof<blake2b_simd::State>, Option<IvcRollingState>)> {
        let is_genesis = rolling_state.state().step_counter == StepCounter::ZERO;
        let chain_epoch = rolling_state.state().current_epoch;
        let certificate_epoch = protocol_message_preimage.current_epoch();
        let is_next_epoch =
            certificate_epoch.as_field() == chain_epoch.as_field() + EpochNumber::new(1).as_field();

        // Prepare the witness, next state, and folded next accumulator.
        // prepare() borrows snark_proof; snark_proof is still owned afterward.
        let input = IvcProverInput::prepare(
            &snark_proof,
            message,
            aggregate_verification_key,
            global,
            protocol_message_preimage,
            rolling_state,
            &self.ivc_setup,
        )?;

        // At genesis the circuit ignores cert bytes (gated on is_not_genesis); pass empty to
        // match the asset generator and avoid unnecessary serialization work.
        let certificate_proof_bytes = if is_genesis {
            CertificateProofBytes::empty()
        } else {
            snark_proof.into_circuit_proof_bytes()
        };

        let circuit_data = IvcCircuitData::try_new(
            global.clone(),
            rolling_state.state().clone(),
            input.witness,
            certificate_proof_bytes,
            rolling_state.ivc_proof().clone(),
            rolling_state.accumulator().clone(),
            &self.ivc_setup.certificate_verifying_key,
            &self.ivc_setup.ivc_verifying_key,
        )?;

        // Public inputs for the new step: [global | next_state | next_accumulator].
        let public_inputs: Vec<CircuitBase> = [
            global.as_public_input(),
            input.next_state.as_public_input(),
            AssignedAccumulator::as_public_input(&input.next_accumulator),
        ]
        .concat();

        // Genesis and next-epoch steps update the rolling state with a fresh Poseidon proof.
        // Same-epoch steps leave the rolling state unchanged (return None).
        let next_rolling_state = if is_genesis || is_next_epoch {
            let poseidon_bytes = prove_with_transcript::<PoseidonState<CircuitBase>>(
                &self.ivc_setup.srs,
                &self.ivc_setup.ivc_proving_key,
                &circuit_data,
                &public_inputs,
                &mut self.rng,
            )?;
            Some(IvcRollingState::new(
                input.next_state.clone(),
                IvcProofBytes::new(poseidon_bytes),
                input.next_accumulator.clone(),
                rolling_state.genesis_signature(),
            ))
        } else {
            None
        };

        // Always generate a Blake2b proof for the external verifier.
        let blake2b_bytes = prove_with_transcript::<blake2b_simd::State>(
            &self.ivc_setup.srs,
            &self.ivc_setup.ivc_proving_key,
            &circuit_data,
            &public_inputs,
            &mut self.rng,
        )?;
        let external_proof = IvcProof {
            proof_bytes: IvcProofBytes::new(blake2b_bytes),
            state: input.next_state,
            accumulator: input.next_accumulator,
            _phantom: PhantomData,
        };

        Ok((external_proof, next_rolling_state))
    }
}
