use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};
use midnight_curves::Bls12;
use midnight_proofs::poly::kzg::msm::DualMSM;

use crate::{
    AggregateVerificationKeyForSnark, MembershipDigest, SnarkProof, StmResult,
    circuits::halo2_ivc::{
        errors::{EpochTransitionErrorKind, IvcCircuitError},
        state::{Global, State},
        types::{
            EpochNumber, MerkleTreeCommitment, MessageHash, ProtocolMessagePreimage, StepCounter,
        },
    },
    proof_system::{
        halo2_snark::build_snark_message,
        ivc_halo2_snark::{rolling_state::IvcRollingState, setup::IvcSetup},
    },
};

/// Categorizes the requested step relative to the rolling chain state: either the first
/// step (genesis), or a non-genesis step at the same or next epoch.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TransitionType {
    /// The first step of the chain. The rolling state's step counter is zero, no
    /// certificate is processed, and the rolling state's accumulator passes through.
    Genesis,
    /// Non-genesis step where the incoming certificate's epoch matches the last
    /// committed epoch.
    SameEpoch,
    /// Non-genesis step where the incoming certificate's epoch is one greater than the
    /// last committed epoch.
    NextEpoch,
}

/// Categorizes the requested step as genesis, same epoch, or next epoch, and validates
/// the epoch advance against the rolling chain state.
///
/// Returns the matching `TransitionType` when the step is valid. Returns
/// `IvcCircuitError::InvalidEpochTransition` with the specific
/// `EpochTransitionErrorKind` when the incoming certificate's epoch is out of range,
/// when the first certificate after genesis is not a next-epoch certificate, or when
/// a same-epoch certificate's lookahead does not match the rolling state.
pub(crate) fn determine_transition(
    rolling_state: &IvcRollingState,
    protocol_message_preimage: &ProtocolMessagePreimage,
) -> StmResult<TransitionType> {
    let last_committed_epoch = rolling_state.state().current_epoch;
    let incoming_certificate_epoch = protocol_message_preimage.current_epoch();

    if rolling_state.state().step_counter == StepCounter::ZERO {
        return Ok(TransitionType::Genesis);
    }

    let is_same_epoch = incoming_certificate_epoch == last_committed_epoch;
    let is_next_epoch = incoming_certificate_epoch.as_field()
        == last_committed_epoch.as_field() + EpochNumber::new(1).as_field();

    if !is_same_epoch && !is_next_epoch {
        return Err(IvcCircuitError::InvalidEpochTransition {
            kind: EpochTransitionErrorKind::OutOfRange {
                incoming_certificate_epoch: incoming_certificate_epoch.as_u64(),
            },
            last_committed_epoch: last_committed_epoch.as_u64(),
        }
        .into());
    }

    if rolling_state.state().step_counter == StepCounter::new(1) && !is_next_epoch {
        return Err(IvcCircuitError::InvalidEpochTransition {
            kind: EpochTransitionErrorKind::FirstCertificateAfterGenesisMustBeNextEpoch,
            last_committed_epoch: last_committed_epoch.as_u64(),
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
            last_committed_epoch: last_committed_epoch.as_u64(),
        }
        .into());
    }

    let transition_type = if is_same_epoch {
        TransitionType::SameEpoch
    } else {
        TransitionType::NextEpoch
    };

    Ok(transition_type)
}

/// Rejects the certificate proof if its embedded verifying key differs from
/// `setup.certificate_verifying_key`, then runs the off-circuit verifier and
/// returns the prepared `DualMSM`.
///
/// The verifying-key match is required so the off-circuit accumulator built from
/// `prepare_and_check` agrees with the one the in-circuit IVC verifier gadget
/// produces on the same proof.
pub(crate) fn verify_certificate_proof<D: MembershipDigest>(
    certificate_proof: &SnarkProof<D>,
    certificate_message_bytes: &[u8],
    aggregate_verification_key_for_snark: &AggregateVerificationKeyForSnark<D>,
    setup: &IvcSetup,
) -> StmResult<DualMSM<Bls12>> {
    if certificate_proof
        .circuit_verification_key()
        .get_midnight_vk()
        .vk()
        .transcript_repr()
        != setup.certificate_verifying_key.transcript_repr()
    {
        return Err(IvcCircuitError::CertificateVerifyingKeyMismatch.into());
    }

    certificate_proof.prepare_and_check(
        certificate_message_bytes,
        aggregate_verification_key_for_snark,
        &setup.srs_verifier_params,
    )
}

/// Builds the certificate's two-element SNARK public-input message from the AVK Merkle root
/// and the certificate's message bytes, then decodes it into the typed certificate message
/// hash and Merkle tree commitment.
pub(crate) fn build_snark_message_and_decode_fields<D: MembershipDigest>(
    aggregate_verification_key_for_snark: &AggregateVerificationKeyForSnark<D>,
    certificate_message_bytes: &[u8],
) -> StmResult<(MessageHash, MerkleTreeCommitment)> {
    let snark_message = build_snark_message(
        &aggregate_verification_key_for_snark.get_merkle_tree_commitment().root,
        certificate_message_bytes,
    )?;
    let certificate_message_hash = MessageHash::from_field(snark_message[1].0);
    let certificate_merkle_tree_commitment = MerkleTreeCommitment::from_field(snark_message[0].0);
    Ok((certificate_message_hash, certificate_merkle_tree_commitment))
}

/// Builds the non-genesis `State` for the next step. Advances the step counter
/// (overflow-checked), selects the next state's protocol parameters from the rolling
/// state (current for same-epoch, next-epoch lookahead promoted for next-epoch
/// transitions), and carries the lookahead fields from the protocol message preimage.
/// The certificate's typed message hash and Merkle tree commitment are passed in by the
/// caller (decoded upstream by `build_snark_message_and_decode_fields`).
pub(crate) fn build_next_state(
    transition_type: TransitionType,
    rolling_state: &IvcRollingState,
    certificate_message_hash: MessageHash,
    certificate_merkle_tree_commitment: MerkleTreeCommitment,
    protocol_message_preimage: &ProtocolMessagePreimage,
) -> StmResult<State> {
    let new_protocol_parameters = if matches!(transition_type, TransitionType::SameEpoch) {
        rolling_state.state().protocol_parameters
    } else {
        rolling_state.state().next_protocol_parameters
    };
    let new_step_counter = rolling_state.new_step_counter()?;
    Ok(State::new(
        new_step_counter,
        certificate_message_hash,
        certificate_merkle_tree_commitment,
        protocol_message_preimage.next_merkle_tree_commitment(),
        new_protocol_parameters,
        protocol_message_preimage.next_protocol_parameters(),
        protocol_message_preimage.current_epoch(),
    ))
}

/// Folds the certificate proof's accumulator and the previous IVC proof's accumulator
/// into the rolling state's accumulator, then collapses the result.
///
/// The returned accumulator is the off-circuit twin of the one the in-circuit IVC
/// verifier gadget computes from the same inputs; the new IVC proof commits to it.
pub(crate) fn build_next_accumulator(
    certificate_dual_msm: DualMSM<Bls12>,
    rolling_state: &IvcRollingState,
    setup: &IvcSetup,
    global: &Global,
) -> StmResult<Accumulator<BlstrsEmulation>> {
    let certificate_collapsed_accumulator =
        setup.certificate_collapsed_accumulator(certificate_dual_msm);
    let previous_ivc_proof_collapsed_accumulator = setup.previous_ivc_proof_collapsed_accumulator(
        rolling_state.ivc_proof().as_bytes(),
        &rolling_state.previous_ivc_proof_public_inputs(global),
    )?;
    let mut next_accumulator = Accumulator::accumulate(&[
        rolling_state.accumulator().clone(),
        certificate_collapsed_accumulator,
        previous_ivc_proof_collapsed_accumulator,
    ]);
    next_accumulator.collapse();
    Ok(next_accumulator)
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use super::*;
    use crate::{
        circuits::halo2_ivc::{
            PREIMAGE_CURRENT_EPOCH_BYTES, PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES,
            PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES, PREIMAGE_SIZE,
            state::trivial_acc,
            types::{IvcProofBytes, ProtocolParametersHash},
        },
        signature_scheme::{BaseFieldElement, SchnorrSigningKey, StandardSchnorrSignature},
    };

    fn build_signature() -> StandardSchnorrSignature {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let signing_key = SchnorrSigningKey::generate(&mut rng);
        signing_key
            .sign_standard(&[BaseFieldElement::from(1u64)], &mut rng)
            .expect("standard schnorr signing should succeed for a synthetic message")
    }

    fn build_rolling_state(
        step_counter: StepCounter,
        current_epoch: EpochNumber,
        next_merkle_tree_commitment: MerkleTreeCommitment,
        protocol_parameters: ProtocolParametersHash,
        next_protocol_parameters: ProtocolParametersHash,
    ) -> IvcRollingState {
        IvcRollingState::new(
            State::new(
                step_counter,
                MessageHash::ZERO,
                MerkleTreeCommitment::ZERO,
                next_merkle_tree_commitment,
                protocol_parameters,
                next_protocol_parameters,
                current_epoch,
            ),
            IvcProofBytes::empty(),
            trivial_acc(&[]),
            build_signature(),
        )
    }

    fn build_standard_rolling_state(
        step_counter: StepCounter,
        current_epoch: EpochNumber,
    ) -> IvcRollingState {
        build_rolling_state(
            step_counter,
            current_epoch,
            MerkleTreeCommitment::ZERO,
            ProtocolParametersHash::ZERO,
            ProtocolParametersHash::ZERO,
        )
    }

    fn build_rolling_state_with_protocol_parameters(
        step_counter: StepCounter,
        current_epoch: EpochNumber,
        current_pp: ProtocolParametersHash,
        next_pp: ProtocolParametersHash,
    ) -> IvcRollingState {
        build_rolling_state(
            step_counter,
            current_epoch,
            MerkleTreeCommitment::ZERO,
            current_pp,
            next_pp,
        )
    }

    fn build_preimage(
        current_epoch: EpochNumber,
        next_merkle_tree_commitment_bytes: [u8; 32],
        next_protocol_parameters_bytes: [u8; 32],
    ) -> ProtocolMessagePreimage {
        let mut bytes = [0u8; PREIMAGE_SIZE];
        bytes[PREIMAGE_CURRENT_EPOCH_BYTES].copy_from_slice(&current_epoch.as_u64().to_le_bytes());
        bytes[PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES]
            .copy_from_slice(&next_merkle_tree_commitment_bytes);
        bytes[PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES]
            .copy_from_slice(&next_protocol_parameters_bytes);
        ProtocolMessagePreimage::new(bytes)
    }

    fn build_standard_preimage(current_epoch: EpochNumber) -> ProtocolMessagePreimage {
        build_preimage(current_epoch, [0u8; 32], [0u8; 32])
    }

    fn protocol_parameters_from_u64(value: u64) -> ProtocolParametersHash {
        ProtocolParametersHash::from_field(BaseFieldElement::from(value).0)
    }

    fn merkle_tree_commitment_from_u64(value: u64) -> MerkleTreeCommitment {
        MerkleTreeCommitment::from_field(BaseFieldElement::from(value).0)
    }

    fn merkle_tree_commitment_from_bytes(bytes: [u8; 32]) -> MerkleTreeCommitment {
        MerkleTreeCommitment::from_field(
            BaseFieldElement::from_raw(&bytes)
                .expect("from_raw applies modulus reduction")
                .0,
        )
    }

    fn message_hash_from_u64(value: u64) -> MessageHash {
        MessageHash::from_field(BaseFieldElement::from(value).0)
    }

    mod build_next_state {

        use super::*;

        #[test]
        fn same_epoch_keeps_current_protocol_parameters() {
            let pp_current = protocol_parameters_from_u64(1);
            let pp_next = protocol_parameters_from_u64(2);
            let rolling_state = build_rolling_state_with_protocol_parameters(
                StepCounter::new(5),
                EpochNumber::new(3),
                pp_current,
                pp_next,
            );
            let preimage = build_standard_preimage(EpochNumber::new(3));
            let next_state = build_next_state(
                TransitionType::SameEpoch,
                &rolling_state,
                MessageHash::ZERO,
                MerkleTreeCommitment::ZERO,
                &preimage,
            )
            .unwrap();
            assert_eq!(next_state.protocol_parameters, pp_current);
        }

        #[test]
        fn next_epoch_promotes_lookahead_protocol_parameters() {
            let pp_current = protocol_parameters_from_u64(1);
            let pp_next = protocol_parameters_from_u64(2);
            let rolling_state = build_rolling_state_with_protocol_parameters(
                StepCounter::new(5),
                EpochNumber::new(3),
                pp_current,
                pp_next,
            );
            let preimage = build_standard_preimage(EpochNumber::new(4));
            let next_state = build_next_state(
                TransitionType::NextEpoch,
                &rolling_state,
                MessageHash::ZERO,
                MerkleTreeCommitment::ZERO,
                &preimage,
            )
            .unwrap();
            assert_eq!(next_state.protocol_parameters, pp_next);
        }

        #[test]
        fn advances_step_counter_by_one() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(7), EpochNumber::new(3));
            let preimage = build_standard_preimage(EpochNumber::new(3));
            let next_state = build_next_state(
                TransitionType::SameEpoch,
                &rolling_state,
                MessageHash::ZERO,
                MerkleTreeCommitment::ZERO,
                &preimage,
            )
            .unwrap();
            assert_eq!(next_state.step_counter, StepCounter::new(8));
        }

        #[test]
        fn produces_state_with_expected_field_plumbing() {
            let pp_current = protocol_parameters_from_u64(7);
            let pp_next = protocol_parameters_from_u64(8);
            let rolling_state = build_rolling_state(
                StepCounter::new(5),
                EpochNumber::new(3),
                merkle_tree_commitment_from_bytes([0x33; 32]),
                pp_current,
                pp_next,
            );

            let cert_message = message_hash_from_u64(99);
            let cert_merkle_tree_commitment = merkle_tree_commitment_from_u64(98);
            let preimage_cert_epoch = EpochNumber::new(4);
            let preimage = build_preimage(preimage_cert_epoch, [0x44; 32], [0x55; 32]);

            let next_state = build_next_state(
                TransitionType::NextEpoch,
                &rolling_state,
                cert_message,
                cert_merkle_tree_commitment,
                &preimage,
            )
            .unwrap();

            assert_eq!(next_state.step_counter, StepCounter::new(6));
            assert_eq!(next_state.message, cert_message);
            assert_eq!(
                next_state.merkle_tree_commitment,
                cert_merkle_tree_commitment
            );
            assert_eq!(
                next_state.next_merkle_tree_commitment,
                preimage.next_merkle_tree_commitment()
            );
            assert_eq!(next_state.protocol_parameters, pp_next);
            assert_eq!(
                next_state.next_protocol_parameters,
                preimage.next_protocol_parameters()
            );
            assert_eq!(next_state.current_epoch, preimage_cert_epoch);
        }

        #[test]
        fn rejects_step_counter_overflow() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(u64::MAX), EpochNumber::new(3));
            let preimage = build_standard_preimage(EpochNumber::new(3));
            let err = build_next_state(
                TransitionType::SameEpoch,
                &rolling_state,
                MessageHash::ZERO,
                MerkleTreeCommitment::ZERO,
                &preimage,
            )
            .unwrap_err();
            let circuit_error = err
                .downcast_ref::<IvcCircuitError>()
                .expect("error chain should carry IvcCircuitError");
            assert!(matches!(
                circuit_error,
                IvcCircuitError::StepCounterOverflow { .. }
            ));
        }
    }

    mod determine_transition {

        use super::*;

        #[test]
        fn returns_genesis_at_step_zero() {
            let rolling_state = build_standard_rolling_state(StepCounter::ZERO, EpochNumber::ZERO);
            let preimage = build_standard_preimage(EpochNumber::ZERO);
            let transition = determine_transition(&rolling_state, &preimage).unwrap();
            assert!(matches!(transition, TransitionType::Genesis));
        }

        #[test]
        fn returns_same_epoch_for_matching_cert_epoch() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_standard_preimage(EpochNumber::new(3));
            let transition = determine_transition(&rolling_state, &preimage).unwrap();
            assert!(matches!(transition, TransitionType::SameEpoch));
        }

        #[test]
        fn returns_next_epoch_for_advanced_cert_epoch() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_standard_preimage(EpochNumber::new(4));
            let transition = determine_transition(&rolling_state, &preimage).unwrap();
            assert!(matches!(transition, TransitionType::NextEpoch));
        }

        #[test]
        fn rejects_out_of_range_cert_epoch() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_standard_preimage(EpochNumber::new(10));
            let err = determine_transition(&rolling_state, &preimage).unwrap_err();
            let circuit_error = err
                .downcast_ref::<IvcCircuitError>()
                .expect("error chain should carry IvcCircuitError");
            assert!(matches!(
                circuit_error,
                IvcCircuitError::InvalidEpochTransition {
                    kind: EpochTransitionErrorKind::OutOfRange {
                        incoming_certificate_epoch: 10,
                    },
                    ..
                }
            ));
        }

        #[test]
        fn rejects_same_epoch_after_genesis_step() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(1), EpochNumber::ZERO);
            let preimage = build_standard_preimage(EpochNumber::ZERO);
            let err = determine_transition(&rolling_state, &preimage).unwrap_err();
            let circuit_error = err
                .downcast_ref::<IvcCircuitError>()
                .expect("error chain should carry IvcCircuitError");
            assert!(matches!(
                circuit_error,
                IvcCircuitError::InvalidEpochTransition {
                    kind: EpochTransitionErrorKind::FirstCertificateAfterGenesisMustBeNextEpoch,
                    ..
                }
            ));
        }

        #[test]
        fn rejects_same_epoch_with_mismatched_lookahead_commitment() {
            // Rolling state has ZERO next_merkle_tree_commitment; preimage decodes a non-zero one.
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_preimage(EpochNumber::new(3), [0x11; 32], [0u8; 32]);
            let err = determine_transition(&rolling_state, &preimage).unwrap_err();
            let circuit_error = err
                .downcast_ref::<IvcCircuitError>()
                .expect("error chain should carry IvcCircuitError");
            assert!(matches!(
                circuit_error,
                IvcCircuitError::InvalidEpochTransition {
                    kind: EpochTransitionErrorKind::SameEpochLookaheadMismatch,
                    ..
                }
            ));
        }

        #[test]
        fn rejects_same_epoch_with_mismatched_lookahead_parameters() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_preimage(EpochNumber::new(3), [0u8; 32], [0x22; 32]);
            let err = determine_transition(&rolling_state, &preimage).unwrap_err();
            let circuit_error = err
                .downcast_ref::<IvcCircuitError>()
                .expect("error chain should carry IvcCircuitError");
            assert!(matches!(
                circuit_error,
                IvcCircuitError::InvalidEpochTransition {
                    kind: EpochTransitionErrorKind::SameEpochLookaheadMismatch,
                    ..
                }
            ));
        }
    }

    mod slow {
        use std::sync::Arc;

        use tempfile::tempdir;

        use super::*;
        use crate::{
            MithrilMembershipDigest, Parameters,
            circuits::{
                halo2_ivc::{
                    K,
                    tests::common::{
                        asset_readers::{
                            VerificationContextAsset,
                            load_embedded_following_certificate_in_epoch_asset,
                            load_embedded_verification_context_asset,
                        },
                        generators::setup::{QUORUM_SIZE, SIGNER_COUNT, TOTAL_STAKE},
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
        };

        fn build_setup() -> IvcSetup {
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
            let cert_provider =
                TempCertificateKeyProvider::new(Arc::clone(&srs), parameters, merkle_tree_depth);
            let cert_vk = cert_provider
                .get_verifying_key()
                .expect("certificate verifying key keygen should succeed");
            let ivc_provider = TempIvcKeyProvider::new(srs, cert_vk);
            IvcSetup::load(&trusted_setup_provider, &cert_provider, &ivc_provider)
                .expect("IvcSetup::load should succeed under the unsafe SRS")
        }

        fn wrap_snark_proof(
            verification_context: &VerificationContextAsset,
            certificate_proof_bytes: Vec<u8>,
        ) -> SnarkProof<MithrilMembershipDigest> {
            let parameters = Parameters {
                k: QUORUM_SIZE as u64,
                m: (QUORUM_SIZE * 10) as u64,
                phi_f: 0.2,
            };
            let merkle_tree_depth = SIGNER_COUNT.next_power_of_two().trailing_zeros();
            let circuit_verification_key =
                CircuitVerificationKey::new(verification_context.certificate_verifying_key.clone());
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

        mod build_next_accumulator {
            use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};
            use midnight_proofs::utils::SerdeFormat;

            use super::*;
            use crate::circuits::halo2_ivc::tests::common::asset_readers::{
                load_embedded_next_epoch_step_output_asset,
                load_embedded_recursive_chain_state_asset,
            };
            use crate::circuits::halo2_ivc::{
                io::Write as IvcWrite,
                tests::common::generators::{
                    build_asset_generation_setup, build_recursive_global,
                    setup::AssetGenerationSetup,
                },
            };

            fn build_global(
                asset_setup: &AssetGenerationSetup,
                verification_context: &VerificationContextAsset,
            ) -> Global {
                build_recursive_global(
                    asset_setup,
                    &verification_context.certificate_verifying_key,
                    &verification_context.recursive_verifying_key,
                )
            }

            fn accumulator_bytes(accumulator: &Accumulator<BlstrsEmulation>) -> Vec<u8> {
                let mut bytes = Vec::new();
                accumulator
                    .write(&mut bytes, SerdeFormat::RawBytesUnchecked)
                    .expect("accumulator serialization should succeed");
                bytes
            }

            #[test]
            fn folds_correctly_for_same_epoch_step() {
                let setup = build_setup();
                let verification_context = load_embedded_verification_context_asset()
                    .expect("verification context asset should load");
                let asset_setup = build_asset_generation_setup();
                let global = build_global(&asset_setup, &verification_context);

                let chain_state = load_embedded_recursive_chain_state_asset()
                    .expect("recursive chain state asset should load");
                let step = load_embedded_following_certificate_in_epoch_asset()
                    .expect("same-epoch step output asset should load");

                let certificate_proof = wrap_snark_proof(
                    &verification_context,
                    step.certificate_proof.clone().into_vec(),
                );
                let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
                let certificate_dual_msm =
                    verify_certificate_proof(&certificate_proof, &step.message, &avk, &setup)
                        .expect("verify_certificate_proof should succeed for valid asset");

                let rolling_state = IvcRollingState::new(
                    chain_state.state,
                    chain_state.ivc_proof,
                    chain_state.accumulator,
                    chain_state.genesis_signature,
                );

                let next_accumulator =
                    build_next_accumulator(certificate_dual_msm, &rolling_state, &setup, &global)
                        .expect("build_next_accumulator should succeed");

                assert_eq!(
                    accumulator_bytes(&next_accumulator),
                    accumulator_bytes(&step.next_accumulator),
                );
            }

            #[test]
            fn folds_correctly_for_next_epoch_step() {
                let setup = build_setup();
                let verification_context = load_embedded_verification_context_asset()
                    .expect("verification context asset should load");
                let asset_setup = build_asset_generation_setup();
                let global = build_global(&asset_setup, &verification_context);

                let chain_state = load_embedded_recursive_chain_state_asset()
                    .expect("recursive chain state asset should load");
                let step = load_embedded_next_epoch_step_output_asset()
                    .expect("next-epoch step output asset should load");

                let certificate_proof = wrap_snark_proof(
                    &verification_context,
                    step.certificate_proof.clone().into_vec(),
                );
                let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
                let certificate_dual_msm =
                    verify_certificate_proof(&certificate_proof, &step.message, &avk, &setup)
                        .expect("verify_certificate_proof should succeed for valid asset");

                let rolling_state = IvcRollingState::new(
                    chain_state.state,
                    chain_state.ivc_proof,
                    chain_state.accumulator,
                    chain_state.genesis_signature,
                );

                let next_accumulator =
                    build_next_accumulator(certificate_dual_msm, &rolling_state, &setup, &global)
                        .expect("build_next_accumulator should succeed");

                assert_eq!(
                    accumulator_bytes(&next_accumulator),
                    accumulator_bytes(&step.next_accumulator),
                );
            }

            #[test]
            fn rejects_corrupted_previous_ivc_proof() {
                let setup = build_setup();
                let verification_context = load_embedded_verification_context_asset()
                    .expect("verification context asset should load");
                let asset_setup = build_asset_generation_setup();
                let global = build_global(&asset_setup, &verification_context);

                let chain_state = load_embedded_recursive_chain_state_asset()
                    .expect("recursive chain state asset should load");
                let step = load_embedded_following_certificate_in_epoch_asset()
                    .expect("same-epoch step output asset should load");

                let certificate_proof = wrap_snark_proof(
                    &verification_context,
                    step.certificate_proof.clone().into_vec(),
                );
                let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
                let certificate_dual_msm =
                    verify_certificate_proof(&certificate_proof, &step.message, &avk, &setup)
                        .expect("verify_certificate_proof should succeed for valid asset");

                let mut corrupted = chain_state.ivc_proof.into_vec();
                corrupted[0] ^= 0xFF;
                let rolling_state = IvcRollingState::new(
                    chain_state.state,
                    IvcProofBytes::new(corrupted),
                    chain_state.accumulator,
                    chain_state.genesis_signature,
                );

                let result =
                    build_next_accumulator(certificate_dual_msm, &rolling_state, &setup, &global);
                assert!(
                    result.is_err(),
                    "build_next_accumulator should reject a corrupted previous IVC proof",
                );
            }

            #[test]
            fn rejects_mismatched_global() {
                let setup = build_setup();
                let verification_context = load_embedded_verification_context_asset()
                    .expect("verification context asset should load");
                let asset_setup = build_asset_generation_setup();
                let correct_global = build_global(&asset_setup, &verification_context);

                let chain_state = load_embedded_recursive_chain_state_asset()
                    .expect("recursive chain state asset should load");
                let step = load_embedded_following_certificate_in_epoch_asset()
                    .expect("same-epoch step output asset should load");

                let certificate_proof = wrap_snark_proof(
                    &verification_context,
                    step.certificate_proof.clone().into_vec(),
                );
                let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
                let certificate_dual_msm =
                    verify_certificate_proof(&certificate_proof, &step.message, &avk, &setup)
                        .expect("verify_certificate_proof should succeed for valid asset");

                let rolling_state = IvcRollingState::new(
                    chain_state.state,
                    chain_state.ivc_proof,
                    chain_state.accumulator,
                    chain_state.genesis_signature,
                );

                let mut wrong_global = correct_global.clone();
                wrong_global.genesis_message = message_hash_from_u64(0xDEAD_BEEF);

                let result = build_next_accumulator(
                    certificate_dual_msm,
                    &rolling_state,
                    &setup,
                    &wrong_global,
                );
                assert!(
                    result.is_err(),
                    "build_next_accumulator should reject a global whose public inputs do not match \
                     the previous IVC proof",
                );
            }
        }

        mod verify_certificate_proof {
            use super::*;

            // Note: a dedicated `rejects_mismatched_verifying_key` test is not provided.
            // Constructing a `CircuitVerificationKey` whose inner `MidnightVK.vk().transcript_repr()`
            // differs from `setup.certificate_verifying_key.transcript_repr()` requires either a
            // pre-generated wrong-VK asset or a second cert-circuit keygen (via `setup_vk`), since
            // `MidnightVK` has no in-process constructor from a raw `VerifyingKey`. The early-return
            // VK match check in `verify_certificate_proof` is a single equality on `transcript_repr`
            // and is exercised indirectly through the matching-VK path in every other slow test.

            #[test]
            fn accepts_valid_proof() {
                let setup = build_setup();
                let verification_context = load_embedded_verification_context_asset()
                    .expect("verification context asset should load");
                let step = load_embedded_following_certificate_in_epoch_asset()
                    .expect("same-epoch step output asset should load");

                let certificate_proof = wrap_snark_proof(
                    &verification_context,
                    step.certificate_proof.clone().into_vec(),
                );
                let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);

                let dual_msm =
                    verify_certificate_proof(&certificate_proof, &step.message, &avk, &setup)
                        .expect("verify_certificate_proof should succeed for valid asset");
                assert!(
                    dual_msm.check(&setup.srs_verifier_params),
                    "returned DualMSM should pass its pairing check",
                );
            }

            #[test]
            fn rejects_corrupted_proof_bytes() {
                let setup = build_setup();
                let verification_context = load_embedded_verification_context_asset()
                    .expect("verification context asset should load");
                let step = load_embedded_following_certificate_in_epoch_asset()
                    .expect("same-epoch step output asset should load");

                let mut corrupted = step.certificate_proof.clone().into_vec();
                corrupted[0] ^= 0xFF;
                let certificate_proof = wrap_snark_proof(&verification_context, corrupted);
                let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);

                let result =
                    verify_certificate_proof(&certificate_proof, &step.message, &avk, &setup);
                assert!(
                    result.is_err(),
                    "verify_certificate_proof should reject a corrupted certificate proof",
                );
            }

            #[test]
            fn rejects_mismatched_message() {
                let setup = build_setup();
                let verification_context = load_embedded_verification_context_asset()
                    .expect("verification context asset should load");
                let step = load_embedded_following_certificate_in_epoch_asset()
                    .expect("same-epoch step output asset should load");

                let certificate_proof = wrap_snark_proof(
                    &verification_context,
                    step.certificate_proof.clone().into_vec(),
                );
                let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);

                let mut wrong_message = step.message;
                wrong_message[0] ^= 0xFF;

                let result =
                    verify_certificate_proof(&certificate_proof, &wrong_message, &avk, &setup);
                assert!(
                    result.is_err(),
                    "verify_certificate_proof should reject a message that does not match the \
                     one the proof committed to",
                );
            }

            #[test]
            fn rejects_mismatched_avk() {
                let setup = build_setup();
                let verification_context = load_embedded_verification_context_asset()
                    .expect("verification context asset should load");
                let step = load_embedded_following_certificate_in_epoch_asset()
                    .expect("same-epoch step output asset should load");

                let certificate_proof = wrap_snark_proof(
                    &verification_context,
                    step.certificate_proof.clone().into_vec(),
                );
                let mut tampered_root = step.aggregate_verification_key_merkle_root;
                tampered_root[0] ^= 0xFF;
                let wrong_avk = wrap_avk(&tampered_root);

                let result =
                    verify_certificate_proof(&certificate_proof, &step.message, &wrong_avk, &setup);
                assert!(
                    result.is_err(),
                    "verify_certificate_proof should reject an AVK that does not match the one \
                     the proof committed to",
                );
            }
        }
    }
}
