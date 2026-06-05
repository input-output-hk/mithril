//! Pre-circuit inputs produced by the IVC prover's preparation step.
//!
//! Holds the witness, the advanced chain state, and the folded accumulator that
//! the in-circuit construction and proof-generation steps consume next.

use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};

use crate::{
    AggregateVerificationKeyForSnark, MembershipDigest, SnarkProof, StmResult,
    circuits::halo2_ivc::{
        errors::IvcCircuitError,
        state::{Global, State, Witness},
        types::{
            EpochNumber, MerkleTreeCommitment, MessageHash, ProtocolMessagePreimage,
            ProtocolParametersHash, StepCounter,
        },
    },
    proof_system::{
        halo2_snark::build_snark_message,
        ivc_halo2_snark::{epoch_data::EpochData, rolling_state::IvcRollingState, setup::IvcSetup},
    },
};

/// Pre-circuit inputs consumed by the IVC prover's circuit-construction and proof-generation steps.
// TODO: remove this allow dead_code directive when the IVC prover consumes this input
#[allow(dead_code)]
pub(crate) struct IvcProverInput {
    /// In-circuit witness for the new step.
    pub(crate) witness: Witness,
    /// Chain state advanced by one step.
    pub(crate) next_state: State,
    /// Folded accumulator the new step's IVC proof commits to.
    pub(crate) next_accumulator: Accumulator<BlstrsEmulation>,
}

// TODO: remove this allow dead_code directive when the IVC prover consumes this input
#[allow(dead_code)]
impl IvcProverInput {
    pub(crate) fn prepare<D: MembershipDigest>(
        snark_proof: SnarkProof<D>,
        message: &[u8],
        aggregate_verification_key_for_snark: &AggregateVerificationKeyForSnark<D>,
        global: &Global,
        epoch_data: &EpochData,
        rolling_state: &IvcRollingState,
        setup: &IvcSetup,
    ) -> StmResult<Self> {
        // Verify the SNARK proof and prepare the dual MSM for the certificate accumulator.
        let dual_msm = snark_proof.prepare_and_check(
            message,
            aggregate_verification_key_for_snark,
            &setup.srs.verifier_params(),
        )?;

        let chain_epoch = rolling_state.state().current_epoch.as_field();
        let certificate_epoch = epoch_data.current_epoch().0;

        let is_same_epoch = certificate_epoch == chain_epoch;
        let is_next_epoch = certificate_epoch == chain_epoch + EpochNumber::new(1).as_field();
        let is_genesis = rolling_state.state().step_counter == StepCounter::ZERO;

        // Verify the genesis signature if at genesis.
        if is_genesis {
            rolling_state.verify_genesis_signature(global)?;
        }

        // Non-genesis steps must advance by zero or one epoch. Genesis bypasses this check.
        if !is_genesis && !is_same_epoch && !is_next_epoch {
            return Err(IvcCircuitError::InvalidEpochTransition {
                certificate_epoch: EpochNumber::from_field(certificate_epoch).as_u64(),
                chain_epoch: rolling_state.state().current_epoch.as_u64(),
            }
            .into());
        }

        // Build the SNARK message and extract the certificate message and Merkle tree commitment.
        let snark_message = build_snark_message(
            &aggregate_verification_key_for_snark.get_merkle_tree_commitment().root,
            message,
        )?;
        let certificate_message = MessageHash::from_field(snark_message[1].0);
        let certificate_merkle_tree_commitment =
            MerkleTreeCommitment::from_field(snark_message[0].0);

        // Compute the new chain state the IVC proof will commit to, based on the epoch transition type.
        let (new_message, new_merkle_tree_commitment, new_protocol_parameters) = if is_genesis {
            (
                global.genesis_message,
                MerkleTreeCommitment::ZERO,
                ProtocolParametersHash::ZERO,
            )
        } else {
            let new_protocol_parameters = if is_same_epoch {
                rolling_state.state().protocol_parameters
            } else {
                rolling_state.state().next_protocol_parameters
            };
            (
                certificate_message,
                certificate_merkle_tree_commitment,
                new_protocol_parameters,
            )
        };
        let new_step_counter = rolling_state.new_step_counter()?;
        let next_state = State::new(
            new_step_counter,
            new_message,
            new_merkle_tree_commitment,
            MerkleTreeCommitment::from_field(epoch_data.next_merkle_tree_commitment().0),
            new_protocol_parameters,
            ProtocolParametersHash::from_field(epoch_data.next_protocol_parameters().0),
            EpochNumber::from_field(certificate_epoch),
        );

        // Compute the new folded accumulator the IVC proof will commit to, by accumulating the
        // previous folded accumulator with the certificate proof and the previous IVC proof
        // (unless at genesis, where we skip the previous IVC proof and use a trivial accumulator instead).
        let certificate_accumulator = setup.certificate_accumulator(dual_msm);
        let previous_ivc_proof_accumulator = if is_genesis {
            setup.trivial_previous_ivc_proof_accumulator()
        } else {
            setup.previous_ivc_proof_accumulator(
                rolling_state.ivc_proof().as_bytes(),
                &rolling_state.previous_ivc_proof_public_inputs(global),
            )?
        };
        let mut next_accumulator = Accumulator::accumulate(&[
            rolling_state.accumulator().clone(),
            certificate_accumulator,
            previous_ivc_proof_accumulator,
        ]);
        next_accumulator.collapse();

        // Construct the in-circuit witness for the new step.
        let witness = Witness::new(
            rolling_state.genesis_signature(),
            certificate_message,
            certificate_merkle_tree_commitment,
            ProtocolMessagePreimage::from(*epoch_data.message_preimage()),
        );

        Ok(IvcProverInput {
            witness,
            next_state,
            next_accumulator,
        })
    }
}
