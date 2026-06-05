//! Pre-circuit inputs produced by the IVC prover's preparation step.
//!
//! Holds the witness, the advanced chain state, and the folded accumulator that
//! the in-circuit construction and proof-generation steps consume next.

use midnight_circuits::{
    types::Instantiable,
    verifier::{Accumulator, BlstrsEmulation},
};

use crate::{
    AggregateVerificationKeyForSnark, MembershipDigest, SnarkProof, StmResult,
    circuits::{
        halo2::types::CircuitBase,
        halo2_ivc::{
            AssignedAccumulator,
            certificate_proof::verify_and_prepare_accumulator,
            errors::IvcCircuitError,
            state::{Global, State, Witness, trivial_acc},
            types::{
                EpochNumber, MerkleTreeCommitment, MessageHash, ProtocolMessagePreimage,
                ProtocolParametersHash, StepCounter,
            },
        },
    },
    proof_system::{
        halo2_snark::build_snark_message,
        ivc_halo2_snark::{epoch_data::EpochData, rolling_state::IvcRollingState, setup::IvcSetup},
    },
    signature_scheme::BaseFieldElement,
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

        if !is_genesis && !is_same_epoch && !is_next_epoch {
            return Err(IvcCircuitError::InvalidEpochTransition {
                certificate_epoch: EpochNumber::from_field(certificate_epoch).as_u64(),
                chain_epoch: rolling_state.state().current_epoch.as_u64(),
            }
            .into());
        }

        if is_genesis {
            rolling_state.genesis_signature().verify(
                &[BaseFieldElement::from(global.genesis_message.as_field())],
                &global.genesis_verification_key,
            )?;
        }

        let snark_message = build_snark_message(
            &aggregate_verification_key_for_snark.get_merkle_tree_commitment().root,
            message,
        )?;
        let certificate_message = MessageHash::from_field(snark_message[1].0);
        let certificate_merkle_tree_commitment =
            MerkleTreeCommitment::from_field(snark_message[0].0);

        let (
            new_message,
            new_merkle_tree_commitment,
            new_protocol_parameters,
            previous_ivc_proof_accumulator,
        ) = if is_genesis {
            let combined_fixed_base_names: Vec<String> =
                setup.combined_fixed_bases.keys().cloned().collect();
            (
                global.genesis_message,
                MerkleTreeCommitment::ZERO,
                ProtocolParametersHash::ZERO,
                trivial_acc(&combined_fixed_base_names),
            )
        } else {
            let new_protocol_parameters = if is_same_epoch {
                rolling_state.state().protocol_parameters
            } else {
                rolling_state.state().next_protocol_parameters
            };
            let previous_ivc_proof_public_inputs: Vec<CircuitBase> = [
                global.as_public_input(),
                rolling_state.state().as_public_input(),
                AssignedAccumulator::as_public_input(rolling_state.accumulator()),
            ]
            .concat();

            let previous_ivc_proof_dual_msm = verify_and_prepare_accumulator(
                rolling_state.ivc_proof().as_bytes(),
                &previous_ivc_proof_public_inputs,
                &setup.ivc_verifying_key,
                &setup.srs.verifier_params(),
            )?;
            let mut accumulator: Accumulator<BlstrsEmulation> = previous_ivc_proof_dual_msm.into();
            accumulator.extract_fixed_bases(&setup.ivc_fixed_bases);
            accumulator.collapse();
            (
                certificate_message,
                certificate_merkle_tree_commitment,
                new_protocol_parameters,
                accumulator,
            )
        };

        let new_step_counter_value =
            rolling_state.state().step_counter.as_u64().checked_add(1).ok_or(
                IvcCircuitError::StepCounterOverflow {
                    current: rolling_state.state().step_counter.as_u64(),
                },
            )?;
        let new_step_counter = StepCounter::new(new_step_counter_value);

        let mut certificate_accumulator: Accumulator<BlstrsEmulation> = dual_msm.into();
        certificate_accumulator.extract_fixed_bases(&setup.certificate_fixed_bases);
        certificate_accumulator.collapse();

        let witness = Witness::new(
            rolling_state.genesis_signature(),
            certificate_message,
            certificate_merkle_tree_commitment,
            ProtocolMessagePreimage::from(*epoch_data.message_preimage()),
        );

        let next_state = State::new(
            new_step_counter,
            new_message,
            new_merkle_tree_commitment,
            MerkleTreeCommitment::from_field(epoch_data.next_merkle_tree_commitment().0),
            new_protocol_parameters,
            ProtocolParametersHash::from_field(epoch_data.next_protocol_parameters().0),
            EpochNumber::from_field(certificate_epoch),
        );

        let mut next_accumulator = Accumulator::accumulate(&[
            rolling_state.accumulator().clone(),
            certificate_accumulator,
            previous_ivc_proof_accumulator,
        ]);
        next_accumulator.collapse();

        Ok(IvcProverInput {
            witness,
            next_state,
            next_accumulator,
        })
    }
}
