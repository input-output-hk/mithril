use std::collections::BTreeMap;

use ff::Field;
use group::Group;

use crate::signature_scheme::{SchnorrVerificationKey, StandardSchnorrSignature};

use super::{
    Accumulator, AssignedByte, AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve,
    AssignedVk, C, ConstraintSystem, E, F, Instantiable, Jubjub, KZGCommitmentScheme, Msm, S,
    VerifyingKey,
    types::{
        CertificateCircuitVerificationKeyRepresentation, EpochNumber,
        IvcCircuitVerificationKeyRepresentation, MerkleTreeCommitment, MessageHash,
        ProtocolMessagePreimage, ProtocolParametersHash, StepCounter,
    },
    verifier,
};

#[derive(Clone, Debug)]
pub(crate) struct State {
    pub(crate) step_counter: StepCounter,
    pub(crate) message: MessageHash,
    pub(crate) merkle_tree_commitment: MerkleTreeCommitment,
    pub(crate) next_merkle_tree_commitment: MerkleTreeCommitment,
    pub(crate) protocol_parameters: ProtocolParametersHash,
    pub(crate) next_protocol_parameters: ProtocolParametersHash,
    pub(crate) current_epoch: EpochNumber,
}

impl State {
    #[allow(dead_code)]
    pub(crate) fn new(
        step_counter: StepCounter,
        message: MessageHash,
        merkle_tree_commitment: MerkleTreeCommitment,
        next_merkle_tree_commitment: MerkleTreeCommitment,
        protocol_parameters: ProtocolParametersHash,
        next_protocol_parameters: ProtocolParametersHash,
        current_epoch: EpochNumber,
    ) -> Self {
        State {
            step_counter,
            message,
            merkle_tree_commitment,
            next_merkle_tree_commitment,
            protocol_parameters,
            next_protocol_parameters,
            current_epoch,
        }
    }

    pub(crate) fn genesis() -> Self {
        State {
            step_counter: StepCounter::ZERO,
            message: MessageHash::ZERO,
            merkle_tree_commitment: MerkleTreeCommitment::ZERO,
            next_merkle_tree_commitment: MerkleTreeCommitment::ZERO,
            protocol_parameters: ProtocolParametersHash::ZERO,
            next_protocol_parameters: ProtocolParametersHash::ZERO,
            current_epoch: EpochNumber::ZERO,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn as_public_input(&self) -> Vec<F> {
        let state = self.clone();
        // Public-input order is part of the recursive circuit statement contract:
        // [step_counter, message, merkle_tree_commitment, next_merkle_tree_commitment, protocol_parameters, next_protocol_parameters, current_epoch].
        vec![
            state.step_counter.as_field(),
            state.message.as_field(),
            state.merkle_tree_commitment.as_field(),
            state.next_merkle_tree_commitment.as_field(),
            state.protocol_parameters.as_field(),
            state.next_protocol_parameters.as_field(),
            state.current_epoch.as_field(),
        ]
    }
}

/// In-circuit counterpart of [`State`].
#[derive(Clone, Debug)]
pub(crate) struct AssignedState {
    pub(crate) step_counter: AssignedNative<F>,
    pub(crate) message: AssignedNative<F>,
    pub(crate) merkle_tree_commitment: AssignedNative<F>,
    pub(crate) next_merkle_tree_commitment: AssignedNative<F>,
    pub(crate) protocol_parameters: AssignedNative<F>,
    pub(crate) next_protocol_parameters: AssignedNative<F>,
    pub(crate) current_epoch: AssignedNative<F>,
}

impl AssignedState {
    pub(crate) fn as_public_input(&self) -> Vec<AssignedNative<F>> {
        let state = self.clone();
        vec![
            state.step_counter,
            state.message,
            state.merkle_tree_commitment,
            state.next_merkle_tree_commitment,
            state.protocol_parameters,
            state.next_protocol_parameters,
            state.current_epoch,
        ]
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Global {
    // Persistent values that do not change through an ivc stream
    pub(crate) genesis_message: MessageHash,
    pub(crate) genesis_vk: SchnorrVerificationKey,
    // cert_vk hash
    pub(crate) cert_vk_repr: CertificateCircuitVerificationKeyRepresentation,
    // ivc_vk hash
    pub(crate) self_vk_repr: IvcCircuitVerificationKeyRepresentation,
}

impl Global {
    #[allow(dead_code)]
    pub(crate) fn new(
        genesis_message: MessageHash,
        genesis_vk: SchnorrVerificationKey,
        cert_vk: &VerifyingKey<F, KZGCommitmentScheme<E>>,
        self_vk: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    ) -> Self {
        Global {
            genesis_message,
            genesis_vk,
            cert_vk_repr: CertificateCircuitVerificationKeyRepresentation::from_field(
                cert_vk.transcript_repr(),
            ),
            self_vk_repr: IvcCircuitVerificationKeyRepresentation::from_field(
                self_vk.transcript_repr(),
            ),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn as_public_input(&self) -> Vec<F> {
        [
            vec![self.genesis_message.as_field()],
            AssignedNativePoint::<Jubjub>::as_public_input(self.genesis_vk.as_jubjub_subgroup()),
            vec![self.cert_vk_repr.as_field(), self.self_vk_repr.as_field()],
        ]
        .concat()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct AssignedGlobal {
    //Persistent values that do not change through an ivc stream
    pub(crate) genesis_message: AssignedNative<F>,
    pub(crate) genesis_vk: AssignedNativePoint<Jubjub>,
    pub(crate) cert_vk: AssignedVk<S>,
    pub(crate) self_vk: AssignedVk<S>,
    // Combined fixed base names for cert_vk and ivc_vk
    pub(crate) fixed_base_names: Vec<String>,
}

#[derive(Clone, Debug)]
pub(crate) struct Witness {
    pub(crate) genesis_sig: StandardSchnorrSignature,
    pub(crate) certificate_message: MessageHash,
    pub(crate) certificate_merkle_tree_commitment: MerkleTreeCommitment,
    // Protocol message preimage bytes
    pub(crate) message_preimage: ProtocolMessagePreimage,
}

impl Witness {
    #[allow(dead_code)]
    pub(crate) fn new(
        genesis_sig: StandardSchnorrSignature,
        certificate_merkle_tree_commitment: MerkleTreeCommitment,
        certificate_message: MessageHash,
        message_preimage: ProtocolMessagePreimage,
    ) -> Self {
        Witness {
            genesis_sig,
            certificate_merkle_tree_commitment,
            certificate_message,
            message_preimage,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct AssignedWitness {
    pub(crate) genesis_sig: (AssignedScalarOfNativeCurve<Jubjub>, AssignedNative<F>),
    pub(crate) certificate_merkle_tree_commitment: AssignedNative<F>,
    pub(crate) certificate_message: AssignedNative<F>,
    // Protocol message preimage bytes
    pub(crate) message_preimage: Vec<AssignedByte<F>>,
}

pub(crate) fn trivial_acc(fixed_base_names: &[String]) -> Accumulator<S> {
    Accumulator::<S>::new(
        Msm::new(&[C::default()], &[F::ONE], &BTreeMap::new()),
        Msm::new(
            &[C::default()],
            &[F::ONE],
            &fixed_base_names.iter().map(|name| (name.clone(), F::ZERO)).collect(),
        ),
    )
}

pub(crate) fn fixed_bases_and_names(
    vk_name: &str,
    vk: &VerifyingKey<F, KZGCommitmentScheme<E>>,
) -> (BTreeMap<String, C>, Vec<String>) {
    let mut fixed_bases = BTreeMap::new();
    fixed_bases.insert(String::from("com_instance"), C::identity());
    fixed_bases.extend(verifier::fixed_bases::<S>(vk_name, vk));
    let fixed_base_names = fixed_bases.keys().cloned().collect::<Vec<_>>();
    (fixed_bases, fixed_base_names)
}

pub(crate) fn fixed_base_names(vk_name: &str, cs: &ConstraintSystem<F>) -> Vec<String> {
    let mut fixed_base_names = vec![String::from("com_instance")];
    fixed_base_names.extend(verifier::fixed_base_names::<S>(
        vk_name,
        cs.num_fixed_columns() + cs.num_selectors(),
        cs.permutation().columns.len(),
    ));
    fixed_base_names
}
