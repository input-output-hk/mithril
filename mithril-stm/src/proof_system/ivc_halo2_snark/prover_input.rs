//! Pre-circuit inputs produced by the IVC prover's preparation step.
//!
//! Holds the witness, the advanced chain state, and the folded accumulator that
//! the in-circuit construction and proof-generation steps consume next.

use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};

use crate::{
    AggregateVerificationKeyForSnark, MembershipDigest, SnarkProof, StmResult,
    circuits::halo2_ivc::{
        errors::{EpochTransitionErrorKind, IvcCircuitError},
        state::{Global, State, Witness},
        types::{
            EpochNumber, MerkleTreeCommitment, MessageHash, ProtocolMessagePreimage,
            ProtocolParametersHash, StepCounter,
        },
    },
    proof_system::{
        halo2_snark::build_snark_message,
        ivc_halo2_snark::{prover_setup::IvcProverSetup, rolling_state::IvcRollingState},
    },
};

/// Pre-circuit inputs consumed by the IVC prover's circuit-construction and proof-generation steps.
#[derive(Debug)]
pub(crate) struct IvcProverInput {
    /// In-circuit witness for the new step.
    pub(crate) witness: Witness,
    /// Chain state advanced by one step.
    pub(crate) next_state: State,
    /// Folded accumulator the new step's IVC proof commits to.
    pub(crate) next_accumulator: Accumulator<BlstrsEmulation>,
}

impl IvcProverInput {
    /// Advances the chain state by one step and bundles the in-circuit witness, the
    /// new state, and the new folded accumulator.
    ///
    /// At genesis (step counter zero) only the chain's genesis signature is verified;
    /// no certificate is processed and the rolling state's accumulator passes through.
    /// At non-genesis steps the certificate proof is verifier-prepared, the epoch
    /// transition is validated, and the certificate and previous IVC accumulators are
    /// folded into the chain's accumulator.
    pub(crate) fn prepare<D: MembershipDigest>(
        snark_proof: &SnarkProof<D>,
        message: &[u8],
        aggregate_verification_key_for_snark: &AggregateVerificationKeyForSnark<D>,
        global: &Global,
        protocol_message_preimage: &ProtocolMessagePreimage,
        rolling_state: &IvcRollingState,
        setup: &IvcProverSetup,
    ) -> StmResult<Self> {
        let chain_epoch = rolling_state.state().current_epoch;
        let certificate_epoch = protocol_message_preimage.current_epoch();

        let is_same_epoch = certificate_epoch == chain_epoch;
        let is_next_epoch =
            certificate_epoch.as_field() == chain_epoch.as_field() + EpochNumber::new(1).as_field();
        let is_genesis = rolling_state.state().step_counter == StepCounter::ZERO;

        // Genesis base case: no certificate is processed. Verify the chain's genesis
        // signature, build the base-case witness/state, and pass the rolling state's
        // trivial accumulator through unchanged.
        if is_genesis {
            rolling_state.verify_genesis_signature(global)?;

            let new_step_counter = rolling_state.new_step_counter()?;
            let next_state = State::new(
                new_step_counter,
                global.genesis_message,
                MerkleTreeCommitment::ZERO,
                protocol_message_preimage.next_merkle_tree_commitment(),
                ProtocolParametersHash::ZERO,
                protocol_message_preimage.next_protocol_parameters(),
                certificate_epoch,
            );

            let witness = Witness::new(
                rolling_state.genesis_signature(),
                MessageHash::ZERO,
                MerkleTreeCommitment::ZERO,
                protocol_message_preimage.clone(),
            );

            return Ok(IvcProverInput {
                witness,
                next_state,
                next_accumulator: rolling_state.accumulator().clone(),
            });
        }

        // Non-genesis path: Reject upfront if the proof's embedded verifying key differs
        // from `setup.certificate_verifying_key`: the off-circuit accumulator built here
        // would not match the one the in-circuit verifier produces.
        if snark_proof
            .circuit_verification_key()
            .get_midnight_vk()
            .vk()
            .transcript_repr()
            != setup.certificate_verifying_key.transcript_repr()
        {
            return Err(IvcCircuitError::CertificateVerifyingKeyMismatch.into());
        }

        let verifier_params = setup.srs.verifier_params();
        let dual_msm = snark_proof.prepare_and_check(
            message,
            aggregate_verification_key_for_snark,
            &verifier_params,
        )?;

        if !is_same_epoch && !is_next_epoch {
            return Err(IvcCircuitError::InvalidEpochTransition {
                kind: EpochTransitionErrorKind::OutOfRange {
                    certificate_epoch: certificate_epoch.as_u64(),
                },
                chain_epoch: chain_epoch.as_u64(),
            }
            .into());
        }

        if rolling_state.state().step_counter == StepCounter::new(1) && !is_next_epoch {
            return Err(IvcCircuitError::InvalidEpochTransition {
                kind: EpochTransitionErrorKind::FirstCertificateAfterGenesisMustBeNextEpoch,
                chain_epoch: chain_epoch.as_u64(),
            }
            .into());
        }

        if is_same_epoch
            && (protocol_message_preimage.next_merkle_tree_commitment()
                != rolling_state.state().next_merkle_tree_commitment
                || protocol_message_preimage.next_protocol_parameters()
                    != rolling_state.state().next_protocol_parameters)
        {
            return Err(IvcCircuitError::InvalidEpochTransition {
                kind: EpochTransitionErrorKind::SameEpochLookaheadMismatch,
                chain_epoch: chain_epoch.as_u64(),
            }
            .into());
        }
        let snark_message = build_snark_message(
            &aggregate_verification_key_for_snark.get_merkle_tree_commitment().root,
            message,
        )?;
        let certificate_message = MessageHash::from_field(snark_message[1].0);
        let certificate_merkle_tree_commitment =
            MerkleTreeCommitment::from_field(snark_message[0].0);

        let new_protocol_parameters = if is_same_epoch {
            rolling_state.state().protocol_parameters
        } else {
            rolling_state.state().next_protocol_parameters
        };
        let new_step_counter = rolling_state.new_step_counter()?;
        let next_state = State::new(
            new_step_counter,
            certificate_message,
            certificate_merkle_tree_commitment,
            protocol_message_preimage.next_merkle_tree_commitment(),
            new_protocol_parameters,
            protocol_message_preimage.next_protocol_parameters(),
            certificate_epoch,
        );

        let certificate_collapsed_accumulator = setup.certificate_collapsed_accumulator(dual_msm);
        let previous_ivc_proof_collapsed_accumulator = setup
            .previous_ivc_proof_collapsed_accumulator(
                rolling_state.ivc_proof().as_bytes(),
                &rolling_state.previous_ivc_proof_public_inputs(global),
            )?;
        let mut next_accumulator = Accumulator::accumulate(&[
            rolling_state.accumulator().clone(),
            certificate_collapsed_accumulator,
            previous_ivc_proof_collapsed_accumulator,
        ]);
        next_accumulator.collapse();

        let witness = Witness::new(
            rolling_state.genesis_signature(),
            certificate_message,
            certificate_merkle_tree_commitment,
            protocol_message_preimage.clone(),
        );

        Ok(IvcProverInput {
            witness,
            next_state,
            next_accumulator,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod slow {
        use std::sync::{Arc, OnceLock};

        use midnight_proofs::utils::SerdeFormat;
        use tempfile::tempdir;

        use crate::{
            MithrilMembershipDigest, Parameters,
            circuits::{
                halo2_ivc::{
                    K,
                    errors::{EpochTransitionErrorKind, IvcCircuitError},
                    io::Write as IvcWrite,
                    tests::common::{
                        asset_readers::{
                            VerificationContextAsset,
                            load_embedded_first_certificate_in_epoch_asset,
                            load_embedded_following_certificate_in_epoch_asset,
                            load_embedded_next_epoch_step_output_asset,
                            load_embedded_recursive_chain_state_asset,
                            load_embedded_verification_context_asset,
                        },
                        generators::{
                            build_asset_generation_setup, build_genesis_base_case_next_state,
                            build_genesis_base_case_witness,
                            build_genesis_protocol_message_preimage, build_recursive_global,
                            setup::{
                                AssetGenerationSetup, GENESIS_EPOCH, QUORUM_SIZE, SIGNER_COUNT,
                                TOTAL_STAKE,
                            },
                        },
                    },
                },
                trusted_setup::build_provider_with_unsafe_srs,
            },
            proof_system::{
                halo2_snark::CircuitVerificationKey,
                ivc_halo2_snark::unsafe_setup_helpers::{
                    TempCertificateKeyProvider, TempIvcKeyProvider,
                },
            },
            signature_scheme::{SchnorrSignatureError, StandardSchnorrSignature},
        };

        use super::*;

        fn shared_ivc_setup() -> &'static IvcProverSetup {
            static CELL: OnceLock<IvcProverSetup> = OnceLock::new();
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
                IvcProverSetup::load(&trusted_setup_provider, &cert_provider, &ivc_provider)
                    .expect("IvcProverSetup::load should succeed under the unsafe SRS")
            })
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
            let circuit_verification_key =
                CircuitVerificationKey::new(ctx.certificate_verifying_key.clone());
            SnarkProof::from_parts(
                certificate_proof_bytes,
                parameters,
                merkle_tree_depth,
                circuit_verification_key,
            )
        }

        fn wrap_avk(
            aggregate_verification_key_merkle_root: &[u8; 32],
        ) -> AggregateVerificationKeyForSnark<MithrilMembershipDigest> {
            let mut avk_bytes = [0u8; 40];
            avk_bytes[0..32].copy_from_slice(aggregate_verification_key_merkle_root);
            avk_bytes[32..40].copy_from_slice(&TOTAL_STAKE.to_be_bytes());
            AggregateVerificationKeyForSnark::<MithrilMembershipDigest>::from_bytes(&avk_bytes)
                .expect("AVK should decode from asset bytes")
        }

        fn wrap_protocol_message_preimage(preimage: &[u8]) -> ProtocolMessagePreimage {
            use crate::circuits::halo2_ivc::PREIMAGE_SIZE;
            let preimage_array: [u8; PREIMAGE_SIZE] = preimage
                .try_into()
                .expect("preimage should be exactly PREIMAGE_SIZE bytes");
            ProtocolMessagePreimage::new(preimage_array)
        }

        fn accumulator_bytes(accumulator: &Accumulator<BlstrsEmulation>) -> Vec<u8> {
            let mut bytes = Vec::new();
            accumulator
                .write(&mut bytes, SerdeFormat::RawBytesUnchecked)
                .expect("accumulator serialization should succeed");
            bytes
        }

        fn build_rolling_state(
            state: State,
            ivc_proof: crate::circuits::halo2_ivc::types::IvcProofBytes,
            accumulator: midnight_circuits::verifier::Accumulator<BlstrsEmulation>,
            genesis_signature: StandardSchnorrSignature,
        ) -> IvcRollingState {
            IvcRollingState::new(state, ivc_proof, accumulator, genesis_signature)
        }

        #[test]
        #[ignore = "slow: runs real keygen via shared OnceLock; opt-in only"]
        fn prepare_at_genesis_produces_advanced_state_and_witness() {
            let setup = shared_ivc_setup();
            let asset_setup = shared_asset_setup();
            let first_step = load_embedded_first_certificate_in_epoch_asset()
                .expect("first step cert asset should load");

            let global = build_global();
            let combined_names: Vec<String> = setup.combined_fixed_bases.keys().cloned().collect();
            let rolling_state =
                IvcRollingState::genesis(asset_setup.genesis_signature, &combined_names);

            let snark_proof = wrap_snark_proof(first_step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&first_step.aggregate_verification_key_merkle_root);
            let genesis_preimage_bytes = build_genesis_protocol_message_preimage(asset_setup);
            let protocol_message_preimage = wrap_protocol_message_preimage(&genesis_preimage_bytes);

            let input = IvcProverInput::prepare(
                &snark_proof,
                &first_step.message,
                &avk,
                &global,
                &protocol_message_preimage,
                &rolling_state,
                setup,
            )
            .expect("prepare should succeed at genesis");

            let expected_next_state =
                build_genesis_base_case_next_state(asset_setup, GENESIS_EPOCH);
            assert_eq!(input.next_state, expected_next_state);

            let expected_witness = build_genesis_base_case_witness(asset_setup);
            assert_eq!(input.witness, expected_witness);

            assert_eq!(
                accumulator_bytes(&input.next_accumulator),
                accumulator_bytes(rolling_state.accumulator()),
            );
        }

        #[test]
        #[ignore = "slow: runs real keygen via shared OnceLock; opt-in only"]
        fn prepare_at_same_epoch_advances_state_correctly() {
            let setup = shared_ivc_setup();
            let chain_state = load_embedded_recursive_chain_state_asset()
                .expect("recursive chain state asset should load");
            let step = load_embedded_following_certificate_in_epoch_asset()
                .expect("same-epoch step output asset should load");

            let global = build_global();
            let snark_proof = wrap_snark_proof(step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
            let protocol_message_preimage = wrap_protocol_message_preimage(&step.message_preimage);
            let chain_genesis_signature = chain_state.genesis_signature;
            let rolling_state = build_rolling_state(
                chain_state.state,
                chain_state.ivc_proof,
                chain_state.accumulator,
                chain_state.genesis_signature,
            );

            let input = IvcProverInput::prepare(
                &snark_proof,
                &step.message,
                &avk,
                &global,
                &protocol_message_preimage,
                &rolling_state,
                setup,
            )
            .expect("prepare should succeed at same-epoch step");

            assert_eq!(input.next_state, step.next_state);
            assert_eq!(
                accumulator_bytes(&input.next_accumulator),
                accumulator_bytes(&step.next_accumulator),
            );
            assert_eq!(input.witness.genesis_signature, chain_genesis_signature);
            assert_eq!(input.witness.message_preimage, protocol_message_preimage);
            assert_eq!(
                input.witness.certificate_merkle_tree_commitment,
                step.next_state.merkle_tree_commitment,
            );
            assert_eq!(input.witness.certificate_message, step.next_state.message);
        }

        #[test]
        #[ignore = "slow: runs real keygen via shared OnceLock; opt-in only"]
        fn prepare_at_next_epoch_carries_lookahead_protocol_parameters() {
            let setup = shared_ivc_setup();
            let chain_state = load_embedded_recursive_chain_state_asset()
                .expect("recursive chain state asset should load");
            let step = load_embedded_next_epoch_step_output_asset()
                .expect("recursive step output asset should load");

            let global = build_global();
            let snark_proof = wrap_snark_proof(step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
            let protocol_message_preimage = wrap_protocol_message_preimage(&step.message_preimage);
            let chain_genesis_signature = chain_state.genesis_signature;
            let rolling_state = build_rolling_state(
                chain_state.state,
                chain_state.ivc_proof,
                chain_state.accumulator,
                chain_state.genesis_signature,
            );

            let input = IvcProverInput::prepare(
                &snark_proof,
                &step.message,
                &avk,
                &global,
                &protocol_message_preimage,
                &rolling_state,
                setup,
            )
            .expect("prepare should succeed at next-epoch step");

            assert_eq!(input.next_state, step.next_state);
            assert_eq!(
                accumulator_bytes(&input.next_accumulator),
                accumulator_bytes(&step.next_accumulator),
            );
            assert_eq!(input.witness.genesis_signature, chain_genesis_signature);
            assert_eq!(input.witness.message_preimage, protocol_message_preimage);
            assert_eq!(
                input.witness.certificate_merkle_tree_commitment,
                step.next_state.merkle_tree_commitment,
            );
            assert_eq!(input.witness.certificate_message, step.next_state.message);
        }

        #[test]
        #[ignore = "slow: runs real keygen via shared OnceLock; opt-in only"]
        fn prepare_rejects_invalid_snark_proof() {
            let setup = shared_ivc_setup();
            let chain_state = load_embedded_recursive_chain_state_asset()
                .expect("recursive chain state asset should load");
            let step = load_embedded_following_certificate_in_epoch_asset()
                .expect("same-epoch step output asset should load");

            let global = build_global();
            let mut corrupted = step.certificate_proof.clone().into_vec();
            corrupted[0] ^= 0xFF;

            let snark_proof = wrap_snark_proof(corrupted);
            let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
            let protocol_message_preimage = wrap_protocol_message_preimage(&step.message_preimage);
            let rolling_state = build_rolling_state(
                chain_state.state,
                chain_state.ivc_proof,
                chain_state.accumulator,
                chain_state.genesis_signature,
            );

            let result = IvcProverInput::prepare(
                &snark_proof,
                &step.message,
                &avk,
                &global,
                &protocol_message_preimage,
                &rolling_state,
                setup,
            );

            let err = result
                .expect_err("prepare should reject a corrupted certificate proof")
                .downcast::<IvcCircuitError>()
                .expect("error should downcast to IvcCircuitError");
            assert_eq!(err, IvcCircuitError::CertificateProofRejected);
        }

        #[test]
        #[ignore = "slow: runs real keygen via shared OnceLock; opt-in only"]
        fn prepare_rejects_invalid_genesis_signature() {
            let setup = shared_ivc_setup();
            let asset_setup = shared_asset_setup();
            let first_step = load_embedded_first_certificate_in_epoch_asset()
                .expect("first step cert asset should load");

            let global = build_global();
            let mut sig_bytes = asset_setup.genesis_signature.to_bytes();
            sig_bytes[32] ^= 0x01;
            let bad_signature = StandardSchnorrSignature::from_bytes(&sig_bytes)
                .expect("mutated signature should still deserialize");

            let combined_names: Vec<String> = setup.combined_fixed_bases.keys().cloned().collect();
            let rolling_state = IvcRollingState::genesis(bad_signature, &combined_names);

            let snark_proof = wrap_snark_proof(first_step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&first_step.aggregate_verification_key_merkle_root);
            let genesis_preimage_bytes = build_genesis_protocol_message_preimage(asset_setup);
            let protocol_message_preimage = wrap_protocol_message_preimage(&genesis_preimage_bytes);

            let result = IvcProverInput::prepare(
                &snark_proof,
                &first_step.message,
                &avk,
                &global,
                &protocol_message_preimage,
                &rolling_state,
                setup,
            );

            let err = result
                .expect_err("prepare should reject an invalid genesis signature")
                .downcast::<SchnorrSignatureError>()
                .expect("error should downcast to SchnorrSignatureError");
            assert!(
                matches!(err, SchnorrSignatureError::StandardSignatureInvalid(_)),
                "expected StandardSignatureInvalid, got {err:?}"
            );
        }

        #[test]
        #[ignore = "slow: runs real keygen via shared OnceLock; opt-in only"]
        fn prepare_rejects_invalid_epoch_transition() {
            let setup = shared_ivc_setup();
            let chain_state = load_embedded_recursive_chain_state_asset()
                .expect("recursive chain state asset should load");
            let step = load_embedded_following_certificate_in_epoch_asset()
                .expect("same-epoch step output asset should load");

            let modified_state = State::new(
                chain_state.state.step_counter,
                chain_state.state.message,
                chain_state.state.merkle_tree_commitment,
                chain_state.state.next_merkle_tree_commitment,
                chain_state.state.protocol_parameters,
                chain_state.state.next_protocol_parameters,
                EpochNumber::new(u64::MAX - 100),
            );
            let rolling_state = build_rolling_state(
                modified_state,
                chain_state.ivc_proof,
                chain_state.accumulator,
                chain_state.genesis_signature,
            );

            let global = build_global();
            let snark_proof = wrap_snark_proof(step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
            let protocol_message_preimage = wrap_protocol_message_preimage(&step.message_preimage);

            let result = IvcProverInput::prepare(
                &snark_proof,
                &step.message,
                &avk,
                &global,
                &protocol_message_preimage,
                &rolling_state,
                setup,
            );

            let err = result
                .expect_err("prepare should reject a bad epoch transition")
                .downcast::<IvcCircuitError>()
                .expect("error should downcast to IvcCircuitError");
            assert!(
                matches!(
                    err,
                    IvcCircuitError::InvalidEpochTransition {
                        kind: EpochTransitionErrorKind::OutOfRange { .. },
                        ..
                    }
                ),
                "expected InvalidEpochTransition with OutOfRange kind, got {err:?}"
            );
        }

        #[test]
        #[ignore = "slow: runs real keygen via shared OnceLock; opt-in only"]
        fn prepare_rejects_step_counter_overflow() {
            let setup = shared_ivc_setup();
            let chain_state = load_embedded_recursive_chain_state_asset()
                .expect("recursive chain state asset should load");
            let step = load_embedded_following_certificate_in_epoch_asset()
                .expect("same-epoch step output asset should load");

            let modified_state = State::new(
                StepCounter::new(u64::MAX),
                chain_state.state.message,
                chain_state.state.merkle_tree_commitment,
                chain_state.state.next_merkle_tree_commitment,
                chain_state.state.protocol_parameters,
                chain_state.state.next_protocol_parameters,
                chain_state.state.current_epoch,
            );
            let rolling_state = build_rolling_state(
                modified_state,
                chain_state.ivc_proof,
                chain_state.accumulator,
                chain_state.genesis_signature,
            );

            let global = build_global();
            let snark_proof = wrap_snark_proof(step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
            let protocol_message_preimage = wrap_protocol_message_preimage(&step.message_preimage);

            let result = IvcProverInput::prepare(
                &snark_proof,
                &step.message,
                &avk,
                &global,
                &protocol_message_preimage,
                &rolling_state,
                setup,
            );

            let err = result
                .expect_err("prepare should reject step counter overflow")
                .downcast::<IvcCircuitError>()
                .expect("error should downcast to IvcCircuitError");
            assert!(
                matches!(err, IvcCircuitError::StepCounterOverflow { .. }),
                "expected StepCounterOverflow, got {err:?}"
            );
        }
    }
}
