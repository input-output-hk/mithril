//! `IvcProver` and `IvcProof`: the proving-session handle and its emitted IVC proof.

use std::{marker::PhantomData, sync::Arc};

use ff::FromUniformBytes;
use group::Group;
use midnight_circuits::{
    hash::poseidon::PoseidonState,
    types::Instantiable,
    verifier::{Accumulator, AssignedAccumulator, BlstrsEmulation},
};
use midnight_curves::{Bls12, G1Projective};
use midnight_proofs::{
    plonk::{create_proof, prepare},
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
        CircuitProvingKey,
        errors::IvcProofError,
        prover_input::IvcProverInput,
        rolling_state::IvcRollingState,
        setup::IvcProvingSetup,
        verifier_setup::IvcVerifierSetup,
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
/// `H` is the transcript hash used to produce this proof and must be used to verify it.
/// It is a zero-cost phantom: no `H`-dependent data is stored, but it prevents accidentally
/// verifying a Poseidon-produced proof via the Blake2b path and vice versa.
// TODO: remove this allow dead_code directive when the IVC prover is wired into STM
#[allow(dead_code)]
pub(crate) struct IvcProof<H: TranscriptHash> {
    /// Externally-verifiable proof bytes generated under transcript hash `H`.
    proof_bytes: IvcProofBytes,
    /// Chain state the proof commits to.
    state: State,
    /// Folded accumulator the proof commits to.
    accumulator: Accumulator<BlstrsEmulation>,
    /// Phantom marker tying the proof to its transcript hash type.
    hash: PhantomData<H>,
}

// TODO: remove this allow dead_code directive when the IVC prover is wired into STM
#[allow(dead_code)]
impl<H: TranscriptHash> IvcProof<H> {
    /// Bundle the outputs of a single proving step into a typed proof.
    pub(crate) fn new(
        proof_bytes: IvcProofBytes,
        state: State,
        accumulator: Accumulator<BlstrsEmulation>,
    ) -> Self {
        Self {
            proof_bytes,
            state,
            accumulator,
            hash: PhantomData,
        }
    }
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
    .map_err(|e| IvcProofError::ProofGenerationFailed(e.to_string()))?;
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
        let external_proof = IvcProof::new(
            IvcProofBytes::new(blake2b_bytes),
            input.next_state,
            input.next_accumulator,
        );

        Ok((external_proof, next_rolling_state))
    }
}

// TODO: remove this allow dead_code directive when IvcProof::verify is called
#[allow(dead_code)]
impl<H: TranscriptHash> IvcProof<H>
where
    CircuitBase: Sampleable<H> + Hashable<H>,
    <KZGCommitmentScheme<Bls12> as PolynomialCommitmentScheme<CircuitBase>>::Commitment:
        Hashable<H>,
{
    /// Verifies the IVC proof and its folded accumulator using transcript hash `H`.
    ///
    /// Checks:
    /// 1. The KZG opening equations against the IVC verifying key.
    /// 2. The folded accumulator pairing equation.
    ///
    /// # Invariant
    ///
    /// `global` and `verifier_setup` must be built from the same certificate and IVC verifying
    /// keys. If they differ, the public inputs fed to the KZG opening check will not match the
    /// proof transcript and verification will return [`IvcProofError::KzgOpeningFailed`].
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

        let mut transcript =
            CircuitTranscript::<H>::init_from_bytes(self.proof_bytes.as_bytes());

        let dual_msm =
            prepare::<CircuitBase, KZGCommitmentScheme<Bls12>, CircuitTranscript<H>>(
                verifier_setup.ivc_verifying_key(),
                &[&[G1Projective::identity()]],
                &[&[&public_inputs]],
                &mut transcript,
            )
            .map_err(|_| IvcProofError::TranscriptPreparationFailed)?;

        transcript
            .assert_empty()
            .map_err(|_| IvcProofError::TranscriptNotFullyConsumed)?;

        if !dual_msm.check(verifier_setup.verifier_params()) {
            return Err(IvcProofError::KzgOpeningFailed.into());
        }

        if !self.accumulator.check(
            verifier_setup.tau_g2(),
            verifier_setup.combined_fixed_bases(),
        ) {
            return Err(IvcProofError::AccumulatorFailed.into());
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use midnight_curves::G2Affine;

    use crate::{
        circuits::halo2_ivc::{
            state::Global,
            tests::common::{
                asset_readers::{
                    load_embedded_following_certificate_in_epoch_asset,
                    load_embedded_next_epoch_step_output_asset,
                    load_embedded_recursive_chain_state_asset,
                    load_embedded_verification_context_asset,
                },
                generators::{build_asset_generation_setup, build_recursive_global},
            },
            types::IvcProofBytes,
        },
        proof_system::ivc_halo2_snark::{
            errors::IvcProofError, verifier_setup::IvcVerifierSetup,
        },
    };

    use super::IvcProof;

    fn build_proof_verifier_context() -> (Global, IvcVerifierSetup) {
        let ctx = load_embedded_verification_context_asset()
            .expect("verification context asset should load");
        let setup = build_asset_generation_setup();
        let global = build_recursive_global(
            &setup,
            &ctx.certificate_verifying_key,
            &ctx.recursive_verifying_key,
        );
        let verifier_setup = IvcVerifierSetup::from_parts(
            ctx.verifier_params,
            ctx.verifier_tau_in_g2,
            ctx.recursive_verifying_key,
            ctx.combined_fixed_bases,
        );
        (global, verifier_setup)
    }

    #[test]
    fn ivc_proof_verify_accepts_stored_recursive_step_output() {
        let ctx = load_embedded_verification_context_asset()
            .expect("verification context asset should load");
        let step_output = load_embedded_next_epoch_step_output_asset()
            .expect("recursive step output asset should load");

        let setup = build_asset_generation_setup();
        let global = build_recursive_global(
            &setup,
            &ctx.certificate_verifying_key,
            &ctx.recursive_verifying_key,
        );
        let verifier_setup = IvcVerifierSetup::from_parts(
            ctx.verifier_params,
            ctx.verifier_tau_in_g2,
            ctx.recursive_verifying_key,
            ctx.combined_fixed_bases,
        );

        let proof = IvcProof::<blake2b_simd::State>::new(
            step_output.ivc_proof,
            step_output.next_state,
            step_output.next_accumulator,
        );

        proof
            .verify(&global, &verifier_setup)
            .expect("stored recursive step output should pass IvcProof::verify");
    }

    #[test]
    fn ivc_proof_verify_rejects_tampered_proof_bytes() {
        let (global, verifier_setup) = build_proof_verifier_context();
        let step_output = load_embedded_next_epoch_step_output_asset()
            .expect("recursive step output asset should load");

        let mut tampered_bytes = step_output.ivc_proof.as_bytes().to_vec();
        let mid = tampered_bytes.len() / 2;
        tampered_bytes[mid] ^= 0xff;

        let proof = IvcProof::<blake2b_simd::State>::new(
            IvcProofBytes::new(tampered_bytes),
            step_output.next_state,
            step_output.next_accumulator,
        );

        let err = proof
            .verify(&global, &verifier_setup)
            .expect_err("tampered proof bytes should be rejected by IvcProof::verify");
        assert_eq!(
            err.downcast_ref::<IvcProofError>(),
            Some(&IvcProofError::KzgOpeningFailed),
            "tampered bytes must fail the KZG opening check, got: {err}"
        );
    }

    #[test]
    fn ivc_proof_verify_rejects_mismatched_state() {
        let (global, verifier_setup) = build_proof_verifier_context();
        let step_output = load_embedded_next_epoch_step_output_asset()
            .expect("recursive step output asset should load");
        let same_epoch = load_embedded_following_certificate_in_epoch_asset()
            .expect("same-epoch step output asset should load");

        let proof = IvcProof::<blake2b_simd::State>::new(
            step_output.ivc_proof,
            same_epoch.next_state,
            step_output.next_accumulator,
        );

        let err = proof
            .verify(&global, &verifier_setup)
            .expect_err("state from a different proof should be rejected by IvcProof::verify");
        assert_eq!(
            err.downcast_ref::<IvcProofError>(),
            Some(&IvcProofError::KzgOpeningFailed),
            "mismatched state corrupts public inputs and must fail the KZG opening check, got: {err}"
        );
    }

    #[test]
    fn ivc_proof_verify_rejects_mismatched_accumulator() {
        let (global, verifier_setup) = build_proof_verifier_context();
        let step_output = load_embedded_next_epoch_step_output_asset()
            .expect("recursive step output asset should load");
        let same_epoch = load_embedded_following_certificate_in_epoch_asset()
            .expect("same-epoch step output asset should load");

        let proof = IvcProof::<blake2b_simd::State>::new(
            step_output.ivc_proof,
            step_output.next_state,
            same_epoch.next_accumulator,
        );

        let err = proof.verify(&global, &verifier_setup).expect_err(
            "accumulator from a different proof should be rejected by IvcProof::verify",
        );
        assert_eq!(
            err.downcast_ref::<IvcProofError>(),
            Some(&IvcProofError::KzgOpeningFailed),
            "mismatched accumulator corrupts public inputs and must fail the KZG opening check, got: {err}"
        );
    }

    #[test]
    fn ivc_proof_verify_rejects_poseidon_proof_bytes() {
        let (global, verifier_setup) = build_proof_verifier_context();
        let chain_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load");

        let proof = IvcProof::<blake2b_simd::State>::new(
            chain_state.ivc_proof,
            chain_state.state,
            chain_state.accumulator,
        );

        let err = proof
            .verify(&global, &verifier_setup)
            .expect_err("Poseidon proof bytes should be rejected by IvcProof::<Blake2b>::verify");
        assert_eq!(
            err.downcast_ref::<IvcProofError>(),
            Some(&IvcProofError::KzgOpeningFailed),
            "Poseidon bytes via Blake2b path must fail the KZG opening check, got: {err}"
        );
    }

    #[test]
    fn ivc_proof_verify_rejects_wrong_tau_g2() {
        let ctx = load_embedded_verification_context_asset()
            .expect("verification context asset should load");
        let step_output = load_embedded_next_epoch_step_output_asset()
            .expect("recursive step output asset should load");
        let setup = build_asset_generation_setup();
        let global = build_recursive_global(
            &setup,
            &ctx.certificate_verifying_key,
            &ctx.recursive_verifying_key,
        );
        let verifier_setup = IvcVerifierSetup::from_parts(
            ctx.verifier_params,
            G2Affine::default(),
            ctx.recursive_verifying_key,
            ctx.combined_fixed_bases,
        );

        let proof = IvcProof::<blake2b_simd::State>::new(
            step_output.ivc_proof,
            step_output.next_state,
            step_output.next_accumulator,
        );

        let err = proof
            .verify(&global, &verifier_setup)
            .expect_err("wrong tau_g2 should cause the accumulator pairing check to fail");
        assert_eq!(
            err.downcast_ref::<IvcProofError>(),
            Some(&IvcProofError::AccumulatorFailed),
            "wrong tau_g2 must fail the accumulator check (not the KZG check), got: {err}"
        );
    }
}
