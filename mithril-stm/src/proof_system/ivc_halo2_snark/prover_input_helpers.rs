use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};
use midnight_curves::Bls12;
use midnight_proofs::poly::kzg::msm::DualMSM;

use crate::{
    AggregateVerificationKeyForSnark, MembershipDigest, SnarkProof, StmResult,
    circuits::halo2_ivc::{
        errors::{EpochTransitionErrorKind, IvcCircuitError},
        state::{Global, State},
        types::{MerkleTreeCommitment, MessageHash, ProtocolMessagePreimage},
    },
    proof_system::{
        halo2_snark::build_snark_message,
        ivc_halo2_snark::{prover_setup::IvcSnarkProverSetup, rolling_state::IvcRollingState},
    },
};

/// Classifies how the incoming certificate advances the chain.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IvcTransitionType {
    /// First step of the chain; no certificate is processed.
    Genesis,
    /// Certificate extends the current epoch.
    SameEpoch,
    /// Certificate starts a new epoch.
    NextEpoch,
}

impl IvcTransitionType {
    /// Categorizes the requested step as genesis, same epoch, or next epoch, and
    /// validates the epoch advance against the rolling chain state.
    /// Returns the matching `IvcTransitionType` when the step is valid.
    ///
    /// Returns `IvcCircuitError::InvalidEpochTransition` with the specific
    /// `EpochTransitionErrorKind` when the incoming certificate's epoch is out of
    /// range.
    pub(crate) fn try_compute_transition_type(
        rolling_state: &IvcRollingState,
        protocol_message_preimage: &ProtocolMessagePreimage,
    ) -> StmResult<Self> {
        let last_committed_epoch = rolling_state.state().current_epoch;
        let incoming_certificate_epoch = protocol_message_preimage.current_epoch();

        let transition_type = if incoming_certificate_epoch == last_committed_epoch {
            Self::SameEpoch
        } else if incoming_certificate_epoch == last_committed_epoch.add(1) {
            Self::NextEpoch
        } else {
            return Err(IvcCircuitError::InvalidEpochTransition {
                kind: EpochTransitionErrorKind::EpochGap {
                    incoming_certificate_epoch: incoming_certificate_epoch.as_u64(),
                    last_committed_epoch: last_committed_epoch.as_u64(),
                },
                last_committed_epoch: last_committed_epoch.as_u64(),
            }
            .into());
        };

        Ok(transition_type)
    }
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
    setup: &IvcSnarkProverSetup,
) -> StmResult<DualMSM<Bls12>> {
    if certificate_proof
        .circuit_verification_key()
        .midnight_vk()
        .vk()
        .transcript_repr()
        != setup.certificate_verifying_key.midnight_vk().vk().transcript_repr()
    {
        return Err(IvcCircuitError::CertificateVerifyingKeyMismatch.into());
    }

    certificate_proof.prepare_and_check(
        certificate_message_bytes,
        aggregate_verification_key_for_snark,
        &setup.srs.verifier_params(),
    )
}

/// Builds the certificate's two-element SNARK public-input message from the AVK Merkle root
/// and the certificate's message bytes and returns typed versions that can be used to build
/// a circuit `State`
pub(crate) fn create_snark_message_for_next_state<D: MembershipDigest>(
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
    transition_type: IvcTransitionType,
    rolling_state: &IvcRollingState,
    certificate_message_hash: MessageHash,
    certificate_merkle_tree_commitment: MerkleTreeCommitment,
    protocol_message_preimage: &ProtocolMessagePreimage,
) -> StmResult<State> {
    let new_protocol_parameters = if matches!(transition_type, IvcTransitionType::SameEpoch) {
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
    setup: &IvcSnarkProverSetup,
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

/// Asserts that the parameters in the rolling state matches the ones in
/// the protocol message depending on the epoch transition type.
/// This is done mainly to avoid computing a proof that will not verify.
pub(crate) fn assert_correct_parameters<D: MembershipDigest>(
    rolling_state: &IvcRollingState,
    protocol_message_preimage: &ProtocolMessagePreimage,
    aggregate_verification_key: &AggregateVerificationKeyForSnark<D>,
    message: &[u8],
    transition_type: IvcTransitionType,
) -> StmResult<()> {
    let (_, merkle_tree_commitement) =
        create_snark_message_for_next_state(aggregate_verification_key, message)?;
    let merkle_tree_commitment_matches =
        rolling_state.state().merkle_tree_commitment == merkle_tree_commitement;

    let next_merkle_tree_commitment_matches = rolling_state.state().next_merkle_tree_commitment
        == protocol_message_preimage.next_merkle_tree_commitment();

    let next_protocol_parameters_matches = rolling_state.state().next_protocol_parameters
        == protocol_message_preimage.next_protocol_parameters();

    let result = match transition_type {
        IvcTransitionType::SameEpoch => {
            merkle_tree_commitment_matches
                && next_merkle_tree_commitment_matches
                && next_protocol_parameters_matches
                && rolling_state.state().step_counter.as_u64() > 1
        }
        IvcTransitionType::NextEpoch => next_merkle_tree_commitment_matches,
        IvcTransitionType::Genesis => true,
    };

    if !result {
        return Err(IvcCircuitError::InvalidEpochTransition {
            kind: EpochTransitionErrorKind::RollingStateParametersDoesNotMatchProtocolMessage,
            last_committed_epoch: protocol_message_preimage.current_epoch().as_u64(),
        }
        .into());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        circuits::halo2_ivc::{
            PREIMAGE_CURRENT_EPOCH_BYTES, PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES,
            PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES, PREIMAGE_SIZE,
            state::trivial_acc,
            types::{EpochNumber, IvcProofBytes, ProtocolParametersHash, StepCounter},
        },
        signature_scheme::{BaseFieldElement, SchnorrSigningKey, StandardSchnorrSignature},
    };

    use super::*;

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
                IvcTransitionType::SameEpoch,
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
                IvcTransitionType::NextEpoch,
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
                IvcTransitionType::SameEpoch,
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
                IvcTransitionType::NextEpoch,
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
                IvcTransitionType::SameEpoch,
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

    mod try_compute_transition {

        use super::*;

        #[test]
        fn returns_same_epoch_for_matching_cert_epoch() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_standard_preimage(EpochNumber::new(3));
            let transition =
                IvcTransitionType::try_compute_transition_type(&rolling_state, &preimage).unwrap();
            assert!(matches!(transition, IvcTransitionType::SameEpoch));
        }

        #[test]
        fn returns_next_epoch_for_advanced_cert_epoch() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_standard_preimage(EpochNumber::new(4));
            let transition =
                IvcTransitionType::try_compute_transition_type(&rolling_state, &preimage).unwrap();
            assert!(matches!(transition, IvcTransitionType::NextEpoch));
        }

        #[test]
        fn rejects_out_of_range_cert_epoch() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_standard_preimage(EpochNumber::new(10));
            let err = IvcTransitionType::try_compute_transition_type(&rolling_state, &preimage)
                .unwrap_err();
            let circuit_error = err
                .downcast_ref::<IvcCircuitError>()
                .expect("error chain should carry IvcCircuitError");
            assert!(matches!(
                circuit_error,
                IvcCircuitError::InvalidEpochTransition {
                    kind: EpochTransitionErrorKind::EpochGap {
                        incoming_certificate_epoch: 10,
                        ..
                    },
                    ..
                }
            ));
        }
    }

    mod assert_correct_parameters {
        use crate::{MithrilMembershipDigest, proof_system::AggregateVerificationKeyForSnark};

        use super::*;

        // Creates an avk with a zero root
        fn avk_with_zero_root() -> AggregateVerificationKeyForSnark<MithrilMembershipDigest> {
            AggregateVerificationKeyForSnark::from_bytes(&[0u8; 40]).unwrap()
        }

        // Creates an avk with non zero root
        fn avk_with_nonzero_root() -> AggregateVerificationKeyForSnark<MithrilMembershipDigest> {
            let mut bytes = [0u8; 40];
            bytes[0] = 0x02;
            AggregateVerificationKeyForSnark::from_bytes(&bytes).unwrap()
        }

        #[test]
        fn same_epoch_passes_with_valid_parameters() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_standard_preimage(EpochNumber::new(3));

            let result = assert_correct_parameters(
                &rolling_state,
                &preimage,
                &avk_with_zero_root(),
                &[0u8; 32],
                IvcTransitionType::SameEpoch,
            );

            assert!(result.is_ok());
        }

        #[test]
        fn rejects_same_epoch_after_genesis_step() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(1), EpochNumber::ZERO);
            let preimage = build_standard_preimage(EpochNumber::ZERO);

            let err = assert_correct_parameters(
                &rolling_state,
                &preimage,
                &avk_with_zero_root(),
                &[0u8; 32],
                IvcTransitionType::SameEpoch,
            )
            .unwrap_err();

            let circuit_error = err
                .downcast_ref::<IvcCircuitError>()
                .expect("error chain should carry IvcCircuitError");
            assert!(matches!(
                circuit_error,
                IvcCircuitError::InvalidEpochTransition {
                    kind:
                        EpochTransitionErrorKind::RollingStateParametersDoesNotMatchProtocolMessage,
                    ..
                }
            ));
        }

        #[test]
        fn rejects_same_epoch_with_mismatched_lookahead_commitment() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_preimage(EpochNumber::new(3), [0x11; 32], [0u8; 32]);

            let err = assert_correct_parameters(
                &rolling_state,
                &preimage,
                &avk_with_zero_root(),
                &[0u8; 32],
                IvcTransitionType::SameEpoch,
            )
            .unwrap_err();

            let circuit_error = err
                .downcast_ref::<IvcCircuitError>()
                .expect("error chain should carry IvcCircuitError");
            assert!(matches!(
                circuit_error,
                IvcCircuitError::InvalidEpochTransition {
                    kind:
                        EpochTransitionErrorKind::RollingStateParametersDoesNotMatchProtocolMessage,
                    ..
                }
            ));
        }

        #[test]
        fn rejects_same_epoch_with_mismatched_lookahead_parameters() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_preimage(EpochNumber::new(3), [0u8; 32], [0x22; 32]);

            let err = assert_correct_parameters(
                &rolling_state,
                &preimage,
                &avk_with_zero_root(),
                &[0u8; 32],
                IvcTransitionType::SameEpoch,
            )
            .unwrap_err();

            let circuit_error = err
                .downcast_ref::<IvcCircuitError>()
                .expect("error chain should carry IvcCircuitError");
            assert!(matches!(
                circuit_error,
                IvcCircuitError::InvalidEpochTransition {
                    kind:
                        EpochTransitionErrorKind::RollingStateParametersDoesNotMatchProtocolMessage,
                    ..
                }
            ));
        }

        #[test]
        fn next_epoch_passes_when_merkle_commitment_matches() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_standard_preimage(EpochNumber::new(4));

            let result = assert_correct_parameters(
                &rolling_state,
                &preimage,
                &avk_with_zero_root(),
                &[0u8; 32],
                IvcTransitionType::NextEpoch,
            );

            assert!(result.is_ok());
        }

        #[test]
        fn rejects_next_epoch_when_merkle_commitment_does_not_match() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(5), EpochNumber::new(3));
            let preimage = build_standard_preimage(EpochNumber::new(4));

            let err = assert_correct_parameters(
                &rolling_state,
                &preimage,
                &avk_with_nonzero_root(),
                &[0u8; 32],
                IvcTransitionType::NextEpoch,
            )
            .unwrap_err();

            let circuit_error = err
                .downcast_ref::<IvcCircuitError>()
                .expect("error chain should carry IvcCircuitError");
            assert!(matches!(
                circuit_error,
                IvcCircuitError::InvalidEpochTransition {
                    kind:
                        EpochTransitionErrorKind::RollingStateParametersDoesNotMatchProtocolMessage,
                    ..
                }
            ));
        }

        #[test]
        fn genesis_always_passes() {
            let rolling_state =
                build_standard_rolling_state(StepCounter::new(1), EpochNumber::ZERO);
            let preimage = build_standard_preimage(EpochNumber::ZERO);

            let result = assert_correct_parameters(
                &rolling_state,
                &preimage,
                &avk_with_zero_root(),
                &[0u8; 32],
                IvcTransitionType::Genesis,
            );

            assert!(result.is_ok());
        }
    }
}
