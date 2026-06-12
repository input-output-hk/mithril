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

    fn build_state(
        step_counter: StepCounter,
        current_epoch: EpochNumber,
        next_merkle_tree_commitment: MerkleTreeCommitment,
        protocol_parameters: ProtocolParametersHash,
        next_protocol_parameters: ProtocolParametersHash,
    ) -> State {
        State::new(
            step_counter,
            MessageHash::ZERO,
            MerkleTreeCommitment::ZERO,
            next_merkle_tree_commitment,
            protocol_parameters,
            next_protocol_parameters,
            current_epoch,
        )
    }

    fn build_rolling_state(state: State) -> IvcRollingState {
        IvcRollingState::new(
            state,
            IvcProofBytes::empty(),
            trivial_acc(&[]),
            build_signature(),
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

    fn protocol_parameters_from_u64(value: u64) -> ProtocolParametersHash {
        ProtocolParametersHash::from_field(BaseFieldElement::from(value).0)
    }

    #[test]
    fn determine_transition_returns_genesis_at_step_zero() {
        let rolling_state = build_rolling_state(build_state(
            StepCounter::ZERO,
            EpochNumber::ZERO,
            MerkleTreeCommitment::ZERO,
            ProtocolParametersHash::ZERO,
            ProtocolParametersHash::ZERO,
        ));
        let preimage = build_preimage(EpochNumber::ZERO, [0u8; 32], [0u8; 32]);
        let transition = determine_transition(&rolling_state, &preimage).unwrap();
        assert!(matches!(transition, TransitionType::Genesis));
    }

    #[test]
    fn determine_transition_returns_same_epoch_for_matching_cert_epoch() {
        let rolling_state = build_rolling_state(build_state(
            StepCounter::new(5),
            EpochNumber::new(3),
            MerkleTreeCommitment::ZERO,
            ProtocolParametersHash::ZERO,
            ProtocolParametersHash::ZERO,
        ));
        let preimage = build_preimage(EpochNumber::new(3), [0u8; 32], [0u8; 32]);
        let transition = determine_transition(&rolling_state, &preimage).unwrap();
        assert!(matches!(transition, TransitionType::SameEpoch));
    }

    #[test]
    fn determine_transition_returns_next_epoch_for_advanced_cert_epoch() {
        let rolling_state = build_rolling_state(build_state(
            StepCounter::new(5),
            EpochNumber::new(3),
            MerkleTreeCommitment::ZERO,
            ProtocolParametersHash::ZERO,
            ProtocolParametersHash::ZERO,
        ));
        let preimage = build_preimage(EpochNumber::new(4), [0u8; 32], [0u8; 32]);
        let transition = determine_transition(&rolling_state, &preimage).unwrap();
        assert!(matches!(transition, TransitionType::NextEpoch));
    }

    #[test]
    fn determine_transition_rejects_out_of_range_cert_epoch() {
        let rolling_state = build_rolling_state(build_state(
            StepCounter::new(5),
            EpochNumber::new(3),
            MerkleTreeCommitment::ZERO,
            ProtocolParametersHash::ZERO,
            ProtocolParametersHash::ZERO,
        ));
        let preimage = build_preimage(EpochNumber::new(10), [0u8; 32], [0u8; 32]);
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
    fn determine_transition_rejects_same_epoch_after_genesis_step() {
        let rolling_state = build_rolling_state(build_state(
            StepCounter::new(1),
            EpochNumber::ZERO,
            MerkleTreeCommitment::ZERO,
            ProtocolParametersHash::ZERO,
            ProtocolParametersHash::ZERO,
        ));
        let preimage = build_preimage(EpochNumber::ZERO, [0u8; 32], [0u8; 32]);
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
    fn determine_transition_rejects_same_epoch_with_mismatched_lookahead_commitment() {
        // Rolling state has ZERO next_merkle_tree_commitment; preimage decodes a non-zero one.
        let rolling_state = build_rolling_state(build_state(
            StepCounter::new(5),
            EpochNumber::new(3),
            MerkleTreeCommitment::ZERO,
            ProtocolParametersHash::ZERO,
            ProtocolParametersHash::ZERO,
        ));
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
    fn determine_transition_rejects_same_epoch_with_mismatched_lookahead_parameters() {
        let rolling_state = build_rolling_state(build_state(
            StepCounter::new(5),
            EpochNumber::new(3),
            MerkleTreeCommitment::ZERO,
            ProtocolParametersHash::ZERO,
            ProtocolParametersHash::ZERO,
        ));
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

    #[test]
    fn build_next_state_same_epoch_keeps_current_protocol_parameters() {
        let pp_current = protocol_parameters_from_u64(1);
        let pp_next = protocol_parameters_from_u64(2);
        let rolling_state = build_rolling_state(build_state(
            StepCounter::new(5),
            EpochNumber::new(3),
            MerkleTreeCommitment::ZERO,
            pp_current,
            pp_next,
        ));
        let preimage = build_preimage(EpochNumber::new(3), [0u8; 32], [0u8; 32]);
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
    fn build_next_state_next_epoch_promotes_lookahead_protocol_parameters() {
        let pp_current = protocol_parameters_from_u64(1);
        let pp_next = protocol_parameters_from_u64(2);
        let rolling_state = build_rolling_state(build_state(
            StepCounter::new(5),
            EpochNumber::new(3),
            MerkleTreeCommitment::ZERO,
            pp_current,
            pp_next,
        ));
        let preimage = build_preimage(EpochNumber::new(4), [0u8; 32], [0u8; 32]);
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
    fn build_next_state_advances_step_counter_by_one() {
        let rolling_state = build_rolling_state(build_state(
            StepCounter::new(7),
            EpochNumber::new(3),
            MerkleTreeCommitment::ZERO,
            ProtocolParametersHash::ZERO,
            ProtocolParametersHash::ZERO,
        ));
        let preimage = build_preimage(EpochNumber::new(3), [0u8; 32], [0u8; 32]);
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
    fn build_next_state_produces_state_with_expected_field_plumbing() {
        let pp_current = protocol_parameters_from_u64(7);
        let pp_next = protocol_parameters_from_u64(8);
        let rolling_state = build_rolling_state(build_state(
            StepCounter::new(5),
            EpochNumber::new(3),
            MerkleTreeCommitment::from_field(
                BaseFieldElement::from_raw(&[0x33; 32])
                    .expect("from_raw applies modulus reduction")
                    .0,
            ),
            pp_current,
            pp_next,
        ));

        let cert_message = MessageHash::from_field(BaseFieldElement::from(99u64).0);
        let cert_merkle_tree_commitment =
            MerkleTreeCommitment::from_field(BaseFieldElement::from(98u64).0);
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
    fn build_next_state_rejects_step_counter_overflow() {
        let rolling_state = build_rolling_state(build_state(
            StepCounter::new(u64::MAX),
            EpochNumber::new(3),
            MerkleTreeCommitment::ZERO,
            ProtocolParametersHash::ZERO,
            ProtocolParametersHash::ZERO,
        ));
        let preimage = build_preimage(EpochNumber::new(3), [0u8; 32], [0u8; 32]);
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
