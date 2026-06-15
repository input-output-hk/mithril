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
            types::{CertificateProofBytes, IvcProofBytes, ProtocolMessagePreimage},
        },
    },
    proof_system::ivc_halo2_snark::{
        CircuitProvingKey, errors::IvcProofError, prover_input::IvcProverInput,
        rolling_state::IvcRollingState, setup::IvcProverSetup, verifier_setup::IvcVerifierSetup,
    },
    signature_scheme::StandardSchnorrSignature,
};

/// Per-session IVC prover handle.
// TODO: remove this allow dead_code directive when the IVC prover is wired into STM
#[allow(dead_code)]
pub(crate) struct IvcProver<R: RngCore + CryptoRng> {
    /// Shared, cached setup (SRS, verifying keys, proving key, fixed-base maps).
    pub(crate) ivc_setup: Arc<IvcProverSetup>,
    /// Randomness source used during proof generation.
    pub(crate) rng: R,
}

/// Bootstrap input for the first [`IvcProver::prove`] call in an IVC chain.
///
/// Passed as `genesis_bootstrap: Some(...)` when `rolling_state = None`. Supplies the
/// data needed to run the internal genesis IVC step before processing the first certificate.
///
// TODO(#3147): Wire genesis bootstrap inputs into the aggregator call site.
// TODO: remove this allow dead_code directive when IvcProver::prove is wired into STM
#[allow(dead_code)]
pub(crate) struct IvcGenesisBootstrapInput {
    /// Schnorr half of the Lagrange-era dual genesis signature (Ed25519 + Schnorr). Carried
    /// forward through every rolling state for in-circuit verification of the genesis message.
    /// Populated from `AncillaryGenesisData::genesis_schnorr_signature()` (see issue #3147).
    pub(crate) genesis_signature: StandardSchnorrSignature,
    /// Protocol message preimage of the genesis certificate. Needed by the internal genesis IVC
    /// step to set the lookahead fields in the genesis output state.
    /// Populated from `AncillaryGenesisData::genesis_message()`, which is
    /// `genesis_certificate.protocol_message.rigid_preimage()` (see issue #3147).
    pub(crate) genesis_protocol_message_preimage: ProtocolMessagePreimage,
}

/// IVC proof emitted at the end of a proving step.
///
/// `H` is the transcript hash used to produce this proof and must be used to verify it.
/// It is a zero-cost phantom: no `H`-dependent data is stored, but it prevents accidentally
/// verifying a Poseidon-produced proof via the Blake2b path and vice versa.
// TODO: remove this allow dead_code directive when the IVC prover emits this proof
#[allow(dead_code)]
pub(crate) struct IvcProof<H: TranscriptHash> {
    /// Externally-verifiable proof bytes.
    proof_bytes: IvcProofBytes,
    /// Chain state the proof commits to.
    state: State,
    /// Folded accumulator the proof commits to.
    accumulator: Accumulator<BlstrsEmulation>,
    /// Phantom marker tying the proof to its transcript hash type.
    hash: PhantomData<H>,
}

impl<H: TranscriptHash> IvcProof<H> {
    /// Bundle the outputs of a single proving step into a typed proof.
    ///
    /// `H` is inferred from the prover's own type parameter, so the proof's hash type
    /// is bound to the hash used to produce it without any runtime check.
    #[cfg_attr(not(test), allow(dead_code))]
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

        let mut transcript = CircuitTranscript::<H>::init_from_bytes(self.proof_bytes.as_bytes());

        let dual_msm = prepare::<CircuitBase, KZGCommitmentScheme<Bls12>, CircuitTranscript<H>>(
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

impl<H: TranscriptHash> IvcProof<H>
where
    CircuitBase: Sampleable<H> + Hashable<H> + std::hash::Hash + Ord + FromUniformBytes<64>,
    <KZGCommitmentScheme<Bls12> as PolynomialCommitmentScheme<CircuitBase>>::Commitment:
        Hashable<H>,
{
    /// Calls `create_proof` with the committed-instance layout the IVC circuit expects:
    /// `&[&[&[], public_inputs]]` (one circuit, one instance group, empty committed
    /// instance, then the field-element public inputs). Returns the finalised transcript
    /// bytes on success.
    fn prove_with_transcript(
        srs: &ParamsKZG<Bls12>,
        proving_key: &CircuitProvingKey,
        circuit_data: &IvcCircuitData,
        public_inputs: &[CircuitBase],
        rng: &mut (impl RngCore + CryptoRng),
    ) -> StmResult<Vec<u8>> {
        let mut transcript = CircuitTranscript::<H>::init();
        create_proof::<
            CircuitBase,
            KZGCommitmentScheme<Bls12>,
            CircuitTranscript<H>,
            IvcCircuitData,
        >(
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
}

// TODO: remove this allow dead_code directive when the IVC prover is wired into STM
#[allow(dead_code)]
impl<R: RngCore + CryptoRng> IvcProver<R> {
    /// Advances the IVC chain by one step.
    ///
    /// Exactly one of `rolling_state` and `genesis_bootstrap` must be `Some`:
    ///
    /// - `rolling_state = Some(rs), genesis_bootstrap = None`: normal step. `rs` carries the
    ///   previous step's output. The transition type (same-epoch / next-epoch) is determined
    ///   from the certificate epoch vs the chain epoch recorded in `rs`.
    /// - `rolling_state = None, genesis_bootstrap = Some(bootstrap)`: genesis bootstrap.
    ///   Called at the first certificate (Epoch 1). Internally runs a genesis IVC step using
    ///   `bootstrap.genesis_signature` and `bootstrap.genesis_protocol_message_preimage`,
    ///   then immediately runs the Epoch 1 step with the supplied certificate inputs.
    ///   Returns the Epoch 1 Blake2b proof and the updated rolling state.
    ///
    /// Both `Some` or both `None` returns [`IvcProofError::InvalidProvingContext`].
    ///
    /// Returns `(proof, next_rolling_state)`. `next_rolling_state` is `Some` on next-epoch
    /// steps (rolling state must advance) and `None` on same-epoch steps.
    pub(crate) fn prove<D: MembershipDigest>(
        &mut self,
        snark_proof: SnarkProof<D>,
        message: &[u8],
        aggregate_verification_key: &AggregateVerificationKeyForSnark<D>,
        global: &Global,
        protocol_message_preimage: &ProtocolMessagePreimage,
        rolling_state: Option<&IvcRollingState>,
        // TODO(#3147): Wire genesis_bootstrap into the aggregator call site.
        genesis_bootstrap: Option<IvcGenesisBootstrapInput>,
    ) -> StmResult<(IvcProof<blake2b_simd::State>, Option<IvcRollingState>)> {
        // Exactly one of rolling_state or genesis_bootstrap must be Some.
        // rolling_state must not be a genesis state (step_counter == 0): the bootstrap path
        // is the only supported entry point for the first certificate.
        if rolling_state.is_some() == genesis_bootstrap.is_some()
            || rolling_state.map_or(false, |rs| rs.is_genesis())
        {
            return Err(IvcProofError::InvalidProvingContext.into());
        }
        let genesis_seeded_state: Option<IvcRollingState> = match genesis_bootstrap {
            Some(bootstrap) => Some(self.run_genesis_step(
                &snark_proof,
                message,
                aggregate_verification_key,
                global,
                &bootstrap,
            )?),
            None => None,
        };
        let effective_rolling_state: &IvcRollingState =
            genesis_seeded_state.as_ref().or(rolling_state).unwrap();

        let certificate_epoch = protocol_message_preimage.current_epoch();
        let is_next_epoch = effective_rolling_state.is_next_epoch(certificate_epoch);

        // Prepare the witness, next state, and folded next accumulator.
        // prepare() borrows snark_proof; snark_proof is still owned afterward.
        let prover_input = IvcProverInput::prepare(
            &snark_proof,
            message,
            aggregate_verification_key,
            global,
            protocol_message_preimage,
            effective_rolling_state,
            &self.ivc_setup,
        )?;

        let certificate_proof_bytes = snark_proof.into_circuit_proof_bytes();

        let circuit_data = IvcCircuitData::try_new(
            global.clone(),
            effective_rolling_state.state().clone(),
            prover_input.witness,
            certificate_proof_bytes,
            effective_rolling_state.ivc_proof().clone(),
            effective_rolling_state.accumulator().clone(),
            &self.ivc_setup.certificate_verifying_key,
            &self.ivc_setup.ivc_verifying_key,
        )?;

        // Public inputs for the new step: [global | next_state | next_accumulator].
        let public_inputs: Vec<CircuitBase> = [
            global.as_public_input(),
            prover_input.next_state.as_public_input(),
            AssignedAccumulator::as_public_input(&prover_input.next_accumulator),
        ]
        .concat();

        // Next-epoch steps update the rolling state with a fresh Poseidon proof.
        // Same-epoch steps leave the rolling state unchanged (return None).
        let next_rolling_state = if is_next_epoch {
            let poseidon_bytes = IvcProof::<PoseidonState<CircuitBase>>::prove_with_transcript(
                &self.ivc_setup.srs,
                &self.ivc_setup.ivc_proving_key,
                &circuit_data,
                &public_inputs,
                &mut self.rng,
            )?;
            Some(IvcRollingState::new(
                prover_input.next_state.clone(),
                IvcProofBytes::new(poseidon_bytes),
                prover_input.next_accumulator.clone(),
                effective_rolling_state.genesis_signature(),
            ))
        } else {
            None
        };

        let blake2b_bytes = IvcProof::<blake2b_simd::State>::prove_with_transcript(
            &self.ivc_setup.srs,
            &self.ivc_setup.ivc_proving_key,
            &circuit_data,
            &public_inputs,
            &mut self.rng,
        )?;
        let proof = IvcProof::new(
            IvcProofBytes::new(blake2b_bytes),
            prover_input.next_state,
            prover_input.next_accumulator,
        );

        Ok((proof, next_rolling_state))
    }

    /// Runs the genesis IVC step internally during bootstrap.
    ///
    /// Builds a zero genesis rolling state from `bootstrap.genesis_signature`, calls
    /// [`IvcProverInput::prepare`] with the genesis preimage, and generates a Poseidon proof
    /// to seed the rolling state. The resulting rolling state is returned for immediate use
    /// in the Epoch 1 step.
    ///
    /// `snark_proof`, `message`, and `aggregate_verification_key` are passed through to
    /// `prepare` but are ignored on the genesis path (`step_counter == 0`).
    fn run_genesis_step<D: MembershipDigest>(
        &mut self,
        snark_proof: &SnarkProof<D>,
        message: &[u8],
        aggregate_verification_key: &AggregateVerificationKeyForSnark<D>,
        global: &Global,
        bootstrap: &IvcGenesisBootstrapInput,
    ) -> StmResult<IvcRollingState> {
        let combined_fixed_base_names: Vec<String> =
            self.ivc_setup.combined_fixed_bases.keys().cloned().collect();
        let genesis_rolling_state =
            IvcRollingState::genesis(bootstrap.genesis_signature, &combined_fixed_base_names);

        let genesis_prover_input = IvcProverInput::prepare(
            snark_proof,
            message,
            aggregate_verification_key,
            global,
            &bootstrap.genesis_protocol_message_preimage,
            &genesis_rolling_state,
            &self.ivc_setup,
        )?;

        let genesis_circuit_data = IvcCircuitData::try_new(
            global.clone(),
            genesis_rolling_state.state().clone(),
            genesis_prover_input.witness,
            CertificateProofBytes::empty(),
            genesis_rolling_state.ivc_proof().clone(),
            genesis_rolling_state.accumulator().clone(),
            &self.ivc_setup.certificate_verifying_key,
            &self.ivc_setup.ivc_verifying_key,
        )?;

        let genesis_public_inputs: Vec<CircuitBase> = [
            global.as_public_input(),
            genesis_prover_input.next_state.as_public_input(),
            AssignedAccumulator::as_public_input(&genesis_prover_input.next_accumulator),
        ]
        .concat();

        let poseidon_bytes = IvcProof::<PoseidonState<CircuitBase>>::prove_with_transcript(
            &self.ivc_setup.srs,
            &self.ivc_setup.ivc_proving_key,
            &genesis_circuit_data,
            &genesis_public_inputs,
            &mut self.rng,
        )?;

        Ok(IvcRollingState::new(
            genesis_prover_input.next_state,
            IvcProofBytes::new(poseidon_bytes),
            genesis_prover_input.next_accumulator,
            bootstrap.genesis_signature,
        ))
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
        proof_system::ivc_halo2_snark::{errors::IvcProofError, verifier_setup::IvcVerifierSetup},
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
        // Exercises the `IvcProof::verify` high-level API end-to-end against the
        // stored next-epoch Blake2b proof, confirming that the unified verification
        // path (KZG opening + accumulator pairing) accepts a known-good proof.
        let verification_context = load_embedded_verification_context_asset()
            .expect("verification context asset should load");
        let step_output = load_embedded_next_epoch_step_output_asset()
            .expect("recursive step output asset should load");

        let setup = build_asset_generation_setup();
        let global = build_recursive_global(
            &setup,
            &verification_context.certificate_verifying_key,
            &verification_context.recursive_verifying_key,
        );

        let verifier_setup = IvcVerifierSetup::from_parts(
            verification_context.verifier_params,
            verification_context.verifier_tau_in_g2,
            verification_context.recursive_verifying_key,
            verification_context.combined_fixed_bases,
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
        // A single flipped byte anywhere in the proof transcript must cause `verify` to
        // return `Err`: the KZG opening equations are computed over the raw bytes, so
        // any corruption propagates to a bad MSM and the `dual_msm.check` fails.
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
        // Substituting the state from a different proof step changes the public inputs
        // fed to `prepare`, causing `dual_msm.check` to fail against the unmodified
        // proof bytes.
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
        // Substituting the accumulator from a different proof step corrupts the public
        // inputs fed to `prepare` (the accumulator is serialised into them), so
        // `dual_msm.check` fails before the pairing equation is ever reached.
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
        // Constructing an `IvcProof<blake2b_simd::State>` with Poseidon-transcript bytes
        // and verifying it with the Blake2b path must fail: the two transcript formats
        // are not interchangeable.
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
        // A verifier setup with a wrong tau_g2 but otherwise correct parameters passes
        // the KZG opening check (tau_g2 is not part of verifier_params) and fails at
        // the accumulator pairing equation, exercising the AccumulatorFailed path.
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

    mod slow {
        use std::sync::{Arc, OnceLock};

        use midnight_circuits::hash::poseidon::PoseidonState;
        use rand_core::OsRng;
        use sha2::{Digest, Sha256};
        use tempfile::tempdir;

        use crate::{
            AggregateVerificationKeyForSnark, MithrilMembershipDigest, Parameters, SnarkProof,
            circuits::{
                halo2::types::CircuitBase,
                halo2_ivc::{
                    K,
                    state::Global,
                    tests::common::{
                        CERTIFICATE_CIRCUIT_DEGREE,
                        asset_readers::{
                            VerificationContextAsset, load_embedded_verification_context_asset,
                        },
                        generators::{
                            build_asset_generation_setup, build_genesis_base_case_next_state,
                            build_genesis_protocol_message_preimage, build_recursive_global,
                            build_same_epoch_certificate_asset_data,
                            next_message_and_preimage_for_step,
                            same_epoch_message_and_preimage_for_step,
                            setup::{
                                AssetGenerationSetup, GENESIS_EPOCH, QUORUM_SIZE, SIGNER_COUNT,
                                TOTAL_STAKE,
                            },
                            transitions::build_next_certificate_asset_data,
                        },
                    },
                    types::ProtocolMessagePreimage,
                },
                trusted_setup::build_provider_with_unsafe_srs,
            },
            proof_system::{
                halo2_snark::CircuitVerificationKey,
                ivc_halo2_snark::{
                    setup::IvcProverSetup,
                    unsafe_setup_helpers::{TempCertificateKeyProvider, TempIvcKeyProvider},
                    verifier_setup::IvcVerifierSetup,
                },
            },
        };

        use super::super::{IvcGenesisBootstrapInput, IvcProof, IvcProver};

        fn shared_ivc_setup() -> Arc<IvcProverSetup> {
            static CELL: OnceLock<Arc<IvcProverSetup>> = OnceLock::new();
            CELL.get_or_init(|| {
                let temp_dir = tempdir().expect("temp dir creation should succeed");
                let trusted_setup_provider = build_provider_with_unsafe_srs(temp_dir.path(), K);
                let srs = Arc::new(
                    trusted_setup_provider
                        .get_trusted_setup_parameters()
                        .expect("unsafe SRS should load"),
                );
                let parameters = Parameters {
                    k: QUORUM_SIZE as u64,
                    m: (QUORUM_SIZE * 10) as u64,
                    phi_f: 0.2,
                };
                let merkle_tree_depth = SIGNER_COUNT.next_power_of_two().trailing_zeros();
                let cert_provider = TempCertificateKeyProvider::new(
                    Arc::clone(&srs),
                    parameters,
                    merkle_tree_depth,
                );
                let cert_vk = cert_provider
                    .get_verifying_key()
                    .expect("certificate verifying key keygen should succeed");
                let ivc_provider = TempIvcKeyProvider::new(srs, cert_vk);
                Arc::new(
                    IvcProverSetup::load(&trusted_setup_provider, &cert_provider, &ivc_provider)
                        .expect("IvcProverSetup::load should succeed"),
                )
            })
            .clone()
        }

        fn shared_asset_setup() -> &'static AssetGenerationSetup {
            static CELL: OnceLock<AssetGenerationSetup> = OnceLock::new();
            CELL.get_or_init(build_asset_generation_setup)
        }

        fn shared_verification_context() -> &'static VerificationContextAsset {
            static CELL: OnceLock<VerificationContextAsset> = OnceLock::new();
            CELL.get_or_init(|| {
                load_embedded_verification_context_asset()
                    .expect("verification context asset should load")
            })
        }

        fn build_global() -> Global {
            let asset_setup = shared_asset_setup();
            let ctx = shared_verification_context();
            build_recursive_global(
                asset_setup,
                &ctx.certificate_verifying_key,
                &ctx.recursive_verifying_key,
            )
        }

        fn wrap_snark_proof(
            certificate_proof_bytes: Vec<u8>,
        ) -> SnarkProof<MithrilMembershipDigest> {
            let ctx = shared_verification_context();
            let parameters = Parameters {
                k: QUORUM_SIZE as u64,
                m: (QUORUM_SIZE * 10) as u64,
                phi_f: 0.2,
            };
            let merkle_tree_depth = SIGNER_COUNT.next_power_of_two().trailing_zeros();
            SnarkProof::from_parts(
                certificate_proof_bytes,
                parameters,
                merkle_tree_depth,
                CircuitVerificationKey::new(ctx.certificate_verifying_key.clone()),
            )
        }

        fn wrap_avk(root: &[u8; 32]) -> AggregateVerificationKeyForSnark<MithrilMembershipDigest> {
            let mut avk_bytes = [0u8; 40];
            avk_bytes[0..32].copy_from_slice(root);
            avk_bytes[32..40].copy_from_slice(&TOTAL_STAKE.to_be_bytes());
            AggregateVerificationKeyForSnark::<MithrilMembershipDigest>::from_bytes(&avk_bytes)
                .expect("AVK should decode from bytes")
        }

        fn wrap_protocol_message_preimage(preimage: &[u8]) -> ProtocolMessagePreimage {
            use crate::circuits::halo2_ivc::PREIMAGE_SIZE;
            let preimage_array: [u8; PREIMAGE_SIZE] = preimage
                .try_into()
                .expect("preimage should be exactly PREIMAGE_SIZE bytes");
            ProtocolMessagePreimage::new(preimage_array)
        }

        fn assert_vk_consistency() {
            let ctx = shared_verification_context();
            let setup = shared_ivc_setup();
            assert_eq!(
                ctx.certificate_verifying_key.vk().transcript_repr(),
                setup.certificate_verifying_key.transcript_repr(),
                "stored verification context cert VK must match freshly generated cert VK"
            );
            assert_eq!(
                ctx.recursive_verifying_key.transcript_repr(),
                setup.ivc_verifying_key.transcript_repr(),
                "stored verification context IVC VK must match freshly generated IVC VK"
            );
        }

        #[test]
        fn prove_bootstrap_produces_epoch1_proof_and_rolling_state() {
            // Verifies the bootstrap path (rolling_state = None, genesis_bootstrap = Some):
            // prove() must internally run the genesis IVC step, then run the Epoch 1 step
            // with the supplied certificate inputs, and return a Blake2b proof plus an
            // updated rolling state.
            assert_vk_consistency();

            let ivc_setup = shared_ivc_setup();
            let asset_setup = shared_asset_setup();
            let global = build_global();
            let verifier_setup = IvcVerifierSetup::from_ivc_setup_with_srs(&ivc_setup);

            let mut cert_params = ivc_setup.srs.clone();
            cert_params.downsize(CERTIFICATE_CIRCUIT_DEGREE);
            let cert_vk = shared_verification_context().certificate_verifying_key.clone();

            // Epoch 1 certificate data (next-epoch transition from the genesis base-case state).
            // The previous state for the first real certificate is the output of the internal
            // genesis IVC step, not State::genesis() (which is the zero input state).
            let genesis_base_state = build_genesis_base_case_next_state(asset_setup, GENESIS_EPOCH);
            let (cert_proof, _, expected_epoch1_state, _) = build_next_certificate_asset_data(
                asset_setup,
                &cert_params,
                &asset_setup.certificate_relation,
                &cert_vk,
                &genesis_base_state,
                &mut OsRng,
            );
            let (_, epoch1_preimage_bytes) =
                next_message_and_preimage_for_step(asset_setup, &genesis_base_state);
            let epoch1_message_bytes = Sha256::digest(&epoch1_preimage_bytes);
            let avk_root: [u8; 32] = asset_setup
                .aggregate_verification_key
                .get_merkle_tree_commitment()
                .root
                .as_slice()
                .try_into()
                .expect("asset AVK root must be 32 bytes");
            let avk = wrap_avk(&avk_root);
            let snark_proof = wrap_snark_proof(cert_proof.as_bytes().to_vec());
            let epoch1_preimage = wrap_protocol_message_preimage(&epoch1_preimage_bytes);

            // Genesis bootstrap input.
            let genesis_preimage_bytes = build_genesis_protocol_message_preimage(asset_setup);
            let genesis_preimage = wrap_protocol_message_preimage(&genesis_preimage_bytes);
            let bootstrap = IvcGenesisBootstrapInput {
                genesis_signature: asset_setup.genesis_signature,
                genesis_protocol_message_preimage: genesis_preimage,
            };

            let mut prover = IvcProver {
                ivc_setup,
                rng: OsRng,
            };

            let (blake2b_proof, rolling) = prover
                .prove(
                    snark_proof,
                    epoch1_message_bytes.as_ref(),
                    &avk,
                    &global,
                    &epoch1_preimage,
                    None,
                    Some(bootstrap),
                )
                .expect("bootstrap prove should succeed");

            let epoch1_rolling = rolling.expect("bootstrap must return a rolling state");

            assert_eq!(
                blake2b_proof.state, expected_epoch1_state,
                "bootstrap Blake2b proof state must match Epoch 1 expected state"
            );
            assert_eq!(
                epoch1_rolling.state(),
                &expected_epoch1_state,
                "bootstrap rolling state must match Epoch 1 expected state"
            );

            blake2b_proof
                .verify(&global, &verifier_setup)
                .expect("bootstrap Blake2b proof must verify");

            IvcProof::<PoseidonState<CircuitBase>>::new(
                epoch1_rolling.ivc_proof().clone(),
                epoch1_rolling.state().clone(),
                epoch1_rolling.accumulator().clone(),
            )
            .verify(&global, &verifier_setup)
            .expect("bootstrap Poseidon proof must verify");
        }

        #[test]
        fn prove_next_epoch_produces_external_proof_and_rolling_state() {
            assert_vk_consistency();

            let ivc_setup = shared_ivc_setup();
            let asset_setup = shared_asset_setup();
            let global = build_global();
            let verifier_setup = IvcVerifierSetup::from_ivc_setup_with_srs(&ivc_setup);

            let mut cert_params = ivc_setup.srs.clone();
            cert_params.downsize(CERTIFICATE_CIRCUIT_DEGREE);
            let cert_vk = shared_verification_context().certificate_verifying_key.clone();

            let avk_root: [u8; 32] = asset_setup
                .aggregate_verification_key
                .get_merkle_tree_commitment()
                .root
                .as_slice()
                .try_into()
                .expect("asset AVK root must be 32 bytes");
            let avk = wrap_avk(&avk_root);

            // Bootstrap step — runs genesis + Epoch 1 internally; no assertions on this step.
            let epoch1_rolling = {
                let genesis_base_state =
                    build_genesis_base_case_next_state(asset_setup, GENESIS_EPOCH);
                let (cert_proof, _, _, _) = build_next_certificate_asset_data(
                    asset_setup,
                    &cert_params,
                    &asset_setup.certificate_relation,
                    &cert_vk,
                    &genesis_base_state,
                    &mut OsRng,
                );
                let (_, epoch1_preimage_bytes) =
                    next_message_and_preimage_for_step(asset_setup, &genesis_base_state);
                let epoch1_message_bytes = Sha256::digest(&epoch1_preimage_bytes);
                let snark_proof = wrap_snark_proof(cert_proof.as_bytes().to_vec());
                let epoch1_preimage = wrap_protocol_message_preimage(&epoch1_preimage_bytes);
                let genesis_preimage_bytes = build_genesis_protocol_message_preimage(asset_setup);
                let genesis_preimage = wrap_protocol_message_preimage(&genesis_preimage_bytes);
                let bootstrap = IvcGenesisBootstrapInput {
                    genesis_signature: asset_setup.genesis_signature,
                    genesis_protocol_message_preimage: genesis_preimage,
                };
                let mut prover = IvcProver {
                    ivc_setup: Arc::clone(&ivc_setup),
                    rng: OsRng,
                };
                let (_, rolling) = prover
                    .prove(
                        snark_proof,
                        epoch1_message_bytes.as_ref(),
                        &avk,
                        &global,
                        &epoch1_preimage,
                        None,
                        Some(bootstrap),
                    )
                    .expect("bootstrap prove should succeed");
                rolling.expect("bootstrap must return a rolling state")
            };

            // Next-epoch step (Epoch 2).
            let (cert_proof, _, expected_next_epoch_state, _) = build_next_certificate_asset_data(
                asset_setup,
                &cert_params,
                &asset_setup.certificate_relation,
                &cert_vk,
                epoch1_rolling.state(),
                &mut OsRng,
            );
            let (_, next_epoch_preimage_bytes) =
                next_message_and_preimage_for_step(asset_setup, epoch1_rolling.state());
            let next_epoch_message_bytes = Sha256::digest(&next_epoch_preimage_bytes);
            let snark_proof = wrap_snark_proof(cert_proof.as_bytes().to_vec());
            let preimage = wrap_protocol_message_preimage(&next_epoch_preimage_bytes);

            let mut prover = IvcProver {
                ivc_setup: Arc::clone(&ivc_setup),
                rng: OsRng,
            };
            let (blake2b_proof, rolling) = prover
                .prove(
                    snark_proof,
                    next_epoch_message_bytes.as_ref(),
                    &avk,
                    &global,
                    &preimage,
                    Some(&epoch1_rolling),
                    None,
                )
                .expect("next-epoch prove should succeed");

            let next_rolling = rolling.expect("next-epoch must return a rolling state");

            assert_eq!(
                blake2b_proof.state, expected_next_epoch_state,
                "next-epoch Blake2b proof state must match generator output"
            );
            assert_eq!(
                next_rolling.state(),
                &expected_next_epoch_state,
                "next-epoch rolling state must match generator output"
            );

            blake2b_proof
                .verify(&global, &verifier_setup)
                .expect("next-epoch Blake2b proof must verify");

            IvcProof::<PoseidonState<CircuitBase>>::new(
                next_rolling.ivc_proof().clone(),
                next_rolling.state().clone(),
                next_rolling.accumulator().clone(),
            )
            .verify(&global, &verifier_setup)
            .expect("next-epoch Poseidon proof must verify");
        }

        #[test]
        fn prove_same_epoch_produces_external_proof_only() {
            assert_vk_consistency();

            let ivc_setup = shared_ivc_setup();
            let asset_setup = shared_asset_setup();
            let global = build_global();
            let verifier_setup = IvcVerifierSetup::from_ivc_setup_with_srs(&ivc_setup);

            let mut cert_params = ivc_setup.srs.clone();
            cert_params.downsize(CERTIFICATE_CIRCUIT_DEGREE);
            let cert_vk = shared_verification_context().certificate_verifying_key.clone();

            let avk_root: [u8; 32] = asset_setup
                .aggregate_verification_key
                .get_merkle_tree_commitment()
                .root
                .as_slice()
                .try_into()
                .expect("asset AVK root must be 32 bytes");
            let avk = wrap_avk(&avk_root);

            // Bootstrap step — runs genesis + Epoch 1 internally; no assertions on this step.
            let epoch1_rolling = {
                let genesis_base_state =
                    build_genesis_base_case_next_state(asset_setup, GENESIS_EPOCH);
                let (cert_proof, _, _, _) = build_next_certificate_asset_data(
                    asset_setup,
                    &cert_params,
                    &asset_setup.certificate_relation,
                    &cert_vk,
                    &genesis_base_state,
                    &mut OsRng,
                );
                let (_, epoch1_preimage_bytes) =
                    next_message_and_preimage_for_step(asset_setup, &genesis_base_state);
                let epoch1_message_bytes = Sha256::digest(&epoch1_preimage_bytes);
                let snark_proof = wrap_snark_proof(cert_proof.as_bytes().to_vec());
                let epoch1_preimage = wrap_protocol_message_preimage(&epoch1_preimage_bytes);
                let genesis_preimage_bytes = build_genesis_protocol_message_preimage(asset_setup);
                let genesis_preimage = wrap_protocol_message_preimage(&genesis_preimage_bytes);
                let bootstrap = IvcGenesisBootstrapInput {
                    genesis_signature: asset_setup.genesis_signature,
                    genesis_protocol_message_preimage: genesis_preimage,
                };
                let mut prover = IvcProver {
                    ivc_setup: Arc::clone(&ivc_setup),
                    rng: OsRng,
                };
                let (_, rolling) = prover
                    .prove(
                        snark_proof,
                        epoch1_message_bytes.as_ref(),
                        &avk,
                        &global,
                        &epoch1_preimage,
                        None,
                        Some(bootstrap),
                    )
                    .expect("bootstrap prove should succeed");
                rolling.expect("bootstrap must return a rolling state")
            };

            // Next-epoch step (Epoch 2) — seeds rolling state for same-epoch; no assertions.
            let epoch2_rolling = {
                let (cert_proof, _, _, _) = build_next_certificate_asset_data(
                    asset_setup,
                    &cert_params,
                    &asset_setup.certificate_relation,
                    &cert_vk,
                    epoch1_rolling.state(),
                    &mut OsRng,
                );
                let (_, next_epoch_preimage_bytes) =
                    next_message_and_preimage_for_step(asset_setup, epoch1_rolling.state());
                let next_epoch_message_bytes = Sha256::digest(&next_epoch_preimage_bytes);
                let snark_proof = wrap_snark_proof(cert_proof.as_bytes().to_vec());
                let preimage = wrap_protocol_message_preimage(&next_epoch_preimage_bytes);
                let mut prover = IvcProver {
                    ivc_setup: Arc::clone(&ivc_setup),
                    rng: OsRng,
                };
                let (_, rolling) = prover
                    .prove(
                        snark_proof,
                        next_epoch_message_bytes.as_ref(),
                        &avk,
                        &global,
                        &preimage,
                        Some(&epoch1_rolling),
                        None,
                    )
                    .expect("next-epoch prove should succeed");
                rolling.expect("next-epoch must return a rolling state")
            };

            // Same-epoch step (second certificate in Epoch 2).
            let (cert_proof, _, expected_same_epoch_state, _) =
                build_same_epoch_certificate_asset_data(
                    asset_setup,
                    &cert_params,
                    &asset_setup.certificate_relation,
                    &cert_vk,
                    epoch2_rolling.state(),
                    &mut OsRng,
                );
            let (_, same_epoch_preimage_bytes) =
                same_epoch_message_and_preimage_for_step(asset_setup, epoch2_rolling.state());
            let same_epoch_message_bytes = Sha256::digest(&same_epoch_preimage_bytes);
            let snark_proof = wrap_snark_proof(cert_proof.as_bytes().to_vec());
            let preimage = wrap_protocol_message_preimage(&same_epoch_preimage_bytes);

            let mut prover = IvcProver {
                ivc_setup: Arc::clone(&ivc_setup),
                rng: OsRng,
            };
            let (blake2b_proof, rolling) = prover
                .prove(
                    snark_proof,
                    same_epoch_message_bytes.as_ref(),
                    &avk,
                    &global,
                    &preimage,
                    Some(&epoch2_rolling),
                    None,
                )
                .expect("same-epoch prove should succeed");

            assert!(
                rolling.is_none(),
                "same-epoch must not return a rolling state"
            );

            assert_eq!(
                blake2b_proof.state, expected_same_epoch_state,
                "same-epoch Blake2b proof state must match generator output"
            );

            blake2b_proof
                .verify(&global, &verifier_setup)
                .expect("same-epoch Blake2b proof must verify");
        }
    }
}
