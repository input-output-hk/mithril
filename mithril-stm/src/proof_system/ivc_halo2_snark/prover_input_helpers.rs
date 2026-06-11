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

/// Categorizes the type of epoch transition represented by a given certificate and
/// protocol message preimage, relative to the current rolling chain state.
pub(crate) enum TransitionType {
    /// The certificate and protocol message preimage represent the genesis epoch (epoch 0).
    Genesis,
    /// The certificate and protocol message preimage represent a transition within the same epoch.
    SameEpoch,
    /// The certificate and protocol message preimage represent a transition to the next epoch.
    NextEpoch,
}

/// Categorizes the requested step as genesis, same epoch, or next epoch, and validates
/// the epoch advance against the rolling chain state.
///
/// Returns the matching `TransitionType` when the step is valid. Returns
/// `IvcCircuitError::InvalidEpochTransition` with the specific
/// `EpochTransitionErrorKind` when the certificate's epoch is out of range, when
/// the first certificate after genesis is not a next-epoch certificate, or when a
/// same-epoch certificate's lookahead does not match the rolling state.
pub(crate) fn determine_transition(
    rolling_state: &IvcRollingState,
    protocol_message_preimage: &ProtocolMessagePreimage,
) -> StmResult<TransitionType> {
    let chain_epoch = rolling_state.state().current_epoch;
    let certificate_epoch = protocol_message_preimage.current_epoch();

    if rolling_state.state().step_counter == StepCounter::ZERO {
        return Ok(TransitionType::Genesis);
    }

    let is_same_epoch = certificate_epoch == chain_epoch;
    let is_next_epoch =
        certificate_epoch.as_field() == chain_epoch.as_field() + EpochNumber::new(1).as_field();

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
    snark_proof: &SnarkProof<D>,
    message: &[u8],
    aggregate_verification_key_for_snark: &AggregateVerificationKeyForSnark<D>,
    setup: &IvcSetup,
) -> StmResult<DualMSM<Bls12>> {
    if snark_proof
        .circuit_verification_key()
        .get_midnight_vk()
        .vk()
        .transcript_repr()
        != setup.certificate_verifying_key.transcript_repr()
    {
        return Err(IvcCircuitError::CertificateVerifyingKeyMismatch.into());
    }

    snark_proof.prepare_and_check(
        message,
        aggregate_verification_key_for_snark,
        &setup.srs_verifier_params,
    )
}

/// Builds the certificate's two-element SNARK public-input message from the AVK Merkle root
/// and the user message, then decodes it into the typed certificate message hash and Merkle
/// tree commitment.
pub(crate) fn decode_snark_message_fields<D: MembershipDigest>(
    aggregate_verification_key_for_snark: &AggregateVerificationKeyForSnark<D>,
    message: &[u8],
) -> StmResult<(MessageHash, MerkleTreeCommitment)> {
    let snark_message = build_snark_message(
        &aggregate_verification_key_for_snark.get_merkle_tree_commitment().root,
        message,
    )?;
    let certificate_message = MessageHash::from_field(snark_message[1].0);
    let certificate_merkle_tree_commitment = MerkleTreeCommitment::from_field(snark_message[0].0);
    Ok((certificate_message, certificate_merkle_tree_commitment))
}

/// Builds the non-genesis new chain `State`. Advances the step counter (overflow-checked),
/// selects the new state's protocol parameters from the rolling state (current for same-epoch,
/// next-epoch lookahead promoted for next-epoch transitions), and carries the lookahead fields
/// from the protocol message preimage.
pub(crate) fn build_new_chain_state(
    transition_type: TransitionType,
    rolling_state: &IvcRollingState,
    certificate_message: MessageHash,
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
        certificate_message,
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
    dual_msm: DualMSM<Bls12>,
    rolling_state: &IvcRollingState,
    setup: &IvcSetup,
    global: &Global,
) -> StmResult<Accumulator<BlstrsEmulation>> {
    let certificate_collapsed_accumulator = setup.certificate_collapsed_accumulator(dual_msm);
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
