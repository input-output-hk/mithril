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
        CircuitProvingKey, errors::IvcProofError, prover_input::IvcProverInput,
        rolling_state::IvcRollingState, setup::IvcProvingSetup, verifier_setup::IvcVerifierSetup,
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
    /// - **Genesis** (`step_counter == 0`): one Poseidon proof that seeds the rolling state.
    ///   No Blake2b external proof is returned: the chain is bootstrapping and there is
    ///   nothing to hand back to an external verifier yet.
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
    ) -> StmResult<(
        Option<IvcProof<blake2b_simd::State>>,
        Option<IvcRollingState>,
    )> {
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

        // Genesis only seeds the rolling state; no Blake2b external proof is returned.
        // Same-epoch and next-epoch steps produce a Blake2b proof for the external verifier.
        let external_proof = if is_genesis {
            None
        } else {
            let blake2b_bytes = prove_with_transcript::<blake2b_simd::State>(
                &self.ivc_setup.srs,
                &self.ivc_setup.ivc_proving_key,
                &circuit_data,
                &public_inputs,
                &mut self.rng,
            )?;
            Some(IvcProof::new(
                IvcProofBytes::new(blake2b_bytes),
                input.next_state,
                input.next_accumulator,
            ))
        };

        Ok((external_proof, next_rolling_state))
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
                    rolling_state::IvcRollingState,
                    setup::IvcProvingSetup,
                    unsafe_setup_helpers::{TempCertificateKeyProvider, TempIvcKeyProvider},
                    verifier_setup::IvcVerifierSetup,
                },
            },
        };

        use super::super::{IvcProof, IvcProver};

        fn shared_ivc_setup() -> Arc<IvcProvingSetup> {
            static CELL: OnceLock<Arc<IvcProvingSetup>> = OnceLock::new();
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
                    IvcProvingSetup::load(&trusted_setup_provider, &cert_provider, &ivc_provider)
                        .expect("IvcProvingSetup::load should succeed"),
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
        fn prove_genesis_seeds_rolling_state_with_verifiable_poseidon_proof() {
            assert_vk_consistency();

            let ivc_setup = shared_ivc_setup();
            let asset_setup = shared_asset_setup();
            let global = build_global();
            let verifier_setup = IvcVerifierSetup::from_ivc_setup_with_srs(&ivc_setup);

            let combined_names: Vec<String> =
                ivc_setup.combined_fixed_bases.keys().cloned().collect();
            let rolling_state =
                IvcRollingState::genesis(asset_setup.genesis_signature, &combined_names);
            let genesis_preimage_bytes = build_genesis_protocol_message_preimage(asset_setup);
            let preimage = wrap_protocol_message_preimage(&genesis_preimage_bytes);
            let snark_proof = wrap_snark_proof(vec![]);
            let avk = wrap_avk(&[0u8; 32]);
            let message_bytes = Sha256::digest(&genesis_preimage_bytes);

            let mut prover = IvcProver {
                ivc_setup,
                rng: OsRng,
            };

            let (external, rolling) = prover
                .prove(
                    snark_proof,
                    message_bytes.as_ref(),
                    &avk,
                    &global,
                    &preimage,
                    &rolling_state,
                )
                .expect("genesis prove should succeed");

            assert!(external.is_none(), "genesis must return no Blake2b proof");

            let new_rolling = rolling.expect("genesis must return a rolling state");

            let expected_state = build_genesis_base_case_next_state(asset_setup, GENESIS_EPOCH);
            assert_eq!(
                new_rolling.state(),
                &expected_state,
                "genesis rolling state must match expected next state"
            );

            IvcProof::<PoseidonState<CircuitBase>>::new(
                new_rolling.ivc_proof().clone(),
                new_rolling.state().clone(),
                new_rolling.accumulator().clone(),
            )
            .verify(&global, &verifier_setup)
            .expect("genesis Poseidon proof must verify");
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

            // Genesis step — bootstraps rolling state; no assertions on this step.
            let genesis_rolling = {
                let combined_names: Vec<String> =
                    ivc_setup.combined_fixed_bases.keys().cloned().collect();
                let rolling_state =
                    IvcRollingState::genesis(asset_setup.genesis_signature, &combined_names);
                let genesis_preimage_bytes = build_genesis_protocol_message_preimage(asset_setup);
                let preimage = wrap_protocol_message_preimage(&genesis_preimage_bytes);
                let snark_proof = wrap_snark_proof(vec![]);
                let avk = wrap_avk(&[0u8; 32]);
                let message_bytes = Sha256::digest(&genesis_preimage_bytes);
                let mut prover = IvcProver {
                    ivc_setup: Arc::clone(&ivc_setup),
                    rng: OsRng,
                };
                let (_, rolling) = prover
                    .prove(
                        snark_proof,
                        message_bytes.as_ref(),
                        &avk,
                        &global,
                        &preimage,
                        &rolling_state,
                    )
                    .expect("genesis prove should succeed");
                rolling.expect("genesis must return a rolling state")
            };

            // Next-epoch step.
            let (cert_proof, _, expected_next_epoch_state, _) = build_next_certificate_asset_data(
                asset_setup,
                &cert_params,
                &asset_setup.certificate_relation,
                &cert_vk,
                genesis_rolling.state(),
                &mut OsRng,
            );
            let (_, next_epoch_preimage_bytes) =
                next_message_and_preimage_for_step(asset_setup, genesis_rolling.state());
            let next_epoch_message_bytes = Sha256::digest(&next_epoch_preimage_bytes);
            let avk_root: [u8; 32] = asset_setup
                .aggregate_verification_key
                .get_merkle_tree_commitment()
                .root
                .as_slice()
                .try_into()
                .expect("asset AVK root must be 32 bytes");
            let avk = wrap_avk(&avk_root);
            let snark_proof = wrap_snark_proof(cert_proof.as_bytes().to_vec());
            let preimage = wrap_protocol_message_preimage(&next_epoch_preimage_bytes);

            let mut prover = IvcProver {
                ivc_setup: Arc::clone(&ivc_setup),
                rng: OsRng,
            };
            let (external, rolling) = prover
                .prove(
                    snark_proof,
                    next_epoch_message_bytes.as_ref(),
                    &avk,
                    &global,
                    &preimage,
                    &genesis_rolling,
                )
                .expect("next-epoch prove should succeed");

            let blake2b_proof = external.expect("next-epoch must return a Blake2b proof");
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

            // Genesis step — bootstraps rolling state; no assertions on this step.
            let genesis_rolling = {
                let combined_names: Vec<String> =
                    ivc_setup.combined_fixed_bases.keys().cloned().collect();
                let rolling_state =
                    IvcRollingState::genesis(asset_setup.genesis_signature, &combined_names);
                let genesis_preimage_bytes = build_genesis_protocol_message_preimage(asset_setup);
                let preimage = wrap_protocol_message_preimage(&genesis_preimage_bytes);
                let snark_proof = wrap_snark_proof(vec![]);
                let avk = wrap_avk(&[0u8; 32]);
                let message_bytes = Sha256::digest(&genesis_preimage_bytes);
                let mut prover = IvcProver {
                    ivc_setup: Arc::clone(&ivc_setup),
                    rng: OsRng,
                };
                let (_, rolling) = prover
                    .prove(
                        snark_proof,
                        message_bytes.as_ref(),
                        &avk,
                        &global,
                        &preimage,
                        &rolling_state,
                    )
                    .expect("genesis prove should succeed");
                rolling.expect("genesis must return a rolling state")
            };

            let avk_root: [u8; 32] = asset_setup
                .aggregate_verification_key
                .get_merkle_tree_commitment()
                .root
                .as_slice()
                .try_into()
                .expect("asset AVK root must be 32 bytes");
            let avk = wrap_avk(&avk_root);

            // Next-epoch step — bootstraps rolling state for same-epoch; no assertions on this step.
            let next_rolling = {
                let (cert_proof, _, _, _) = build_next_certificate_asset_data(
                    asset_setup,
                    &cert_params,
                    &asset_setup.certificate_relation,
                    &cert_vk,
                    genesis_rolling.state(),
                    &mut OsRng,
                );
                let (_, next_epoch_preimage_bytes) =
                    next_message_and_preimage_for_step(asset_setup, genesis_rolling.state());
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
                        &genesis_rolling,
                    )
                    .expect("next-epoch prove should succeed");
                rolling.expect("next-epoch must return a rolling state")
            };

            // Same-epoch step.
            let (cert_proof, _, expected_same_epoch_state, _) =
                build_same_epoch_certificate_asset_data(
                    asset_setup,
                    &cert_params,
                    &asset_setup.certificate_relation,
                    &cert_vk,
                    next_rolling.state(),
                    &mut OsRng,
                );
            let (_, same_epoch_preimage_bytes) =
                same_epoch_message_and_preimage_for_step(asset_setup, next_rolling.state());
            let same_epoch_message_bytes = Sha256::digest(&same_epoch_preimage_bytes);
            let snark_proof = wrap_snark_proof(cert_proof.as_bytes().to_vec());
            let preimage = wrap_protocol_message_preimage(&same_epoch_preimage_bytes);

            let mut prover = IvcProver {
                ivc_setup: Arc::clone(&ivc_setup),
                rng: OsRng,
            };
            let (external, rolling) = prover
                .prove(
                    snark_proof,
                    same_epoch_message_bytes.as_ref(),
                    &avk,
                    &global,
                    &preimage,
                    &next_rolling,
                )
                .expect("same-epoch prove should succeed");

            let blake2b_proof = external.expect("same-epoch must return a Blake2b proof");
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
