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
pub struct State {
    pub(crate) counter: StepCounter,
    pub(crate) msg: MessageHash,
    pub(crate) merkle_root: MerkleTreeCommitment,
    pub(crate) next_merkle_root: MerkleTreeCommitment,
    pub(crate) protocol_params: ProtocolParametersHash,
    pub(crate) next_protocol_params: ProtocolParametersHash,
    pub(crate) current_epoch: EpochNumber,
}

impl State {
    #[allow(dead_code)]
    pub(crate) fn new(
        counter: StepCounter,
        msg: MessageHash,
        merkle_root: MerkleTreeCommitment,
        next_merkle_root: MerkleTreeCommitment,
        protocol_params: ProtocolParametersHash,
        next_protocol_params: ProtocolParametersHash,
        current_epoch: EpochNumber,
    ) -> Self {
        State {
            counter,
            msg,
            merkle_root,
            next_merkle_root,
            protocol_params,
            next_protocol_params,
            current_epoch,
        }
    }

    pub(crate) fn genesis() -> Self {
        State {
            counter: StepCounter::ZERO,
            msg: MessageHash::ZERO,
            merkle_root: MerkleTreeCommitment::ZERO,
            next_merkle_root: MerkleTreeCommitment::ZERO,
            protocol_params: ProtocolParametersHash::ZERO,
            next_protocol_params: ProtocolParametersHash::ZERO,
            current_epoch: EpochNumber::ZERO,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn as_public_input(&self) -> Vec<F> {
        let state = self.clone();
        // Public-input order is part of the recursive circuit statement contract:
        // [counter, msg, merkle_root, next_merkle_root, protocol_params, next_protocol_params, current_epoch].
        vec![
            state.counter.as_field(),
            state.msg.as_field(),
            state.merkle_root.as_field(),
            state.next_merkle_root.as_field(),
            state.protocol_params.as_field(),
            state.next_protocol_params.as_field(),
            state.current_epoch.as_field(),
        ]
    }
}

/// In-circuit counterpart of [`State`].
#[derive(Clone, Debug)]
pub struct AssignedState {
    pub(crate) counter: AssignedNative<F>,
    pub(crate) msg: AssignedNative<F>,
    pub(crate) merkle_root: AssignedNative<F>,
    pub(crate) next_merkle_root: AssignedNative<F>,
    pub(crate) protocol_params: AssignedNative<F>,
    pub(crate) next_protocol_params: AssignedNative<F>,
    pub(crate) current_epoch: AssignedNative<F>,
}

impl AssignedState {
    pub fn as_public_input(&self) -> Vec<AssignedNative<F>> {
        let state = self.clone();
        vec![
            state.counter,
            state.msg,
            state.merkle_root,
            state.next_merkle_root,
            state.protocol_params,
            state.next_protocol_params,
            state.current_epoch,
        ]
    }
}

#[derive(Clone, Debug)]
pub struct Global {
    // Persistent values that do not change through an ivc stream
    pub(crate) genesis_msg: MessageHash,
    pub(crate) genesis_vk: SchnorrVerificationKey,
    // cert_vk hash
    pub(crate) cert_vk_repr: CertificateCircuitVerificationKeyRepresentation,
    // ivc_vk hash
    pub(crate) self_vk_repr: IvcCircuitVerificationKeyRepresentation,
}

impl Global {
    #[allow(dead_code)]
    pub(crate) fn new(
        genesis_msg: MessageHash,
        genesis_vk: SchnorrVerificationKey,
        cert_vk: &VerifyingKey<F, KZGCommitmentScheme<E>>,
        self_vk: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    ) -> Self {
        Global {
            genesis_msg,
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
            vec![self.genesis_msg.as_field()],
            AssignedNativePoint::<Jubjub>::as_public_input(self.genesis_vk.as_jubjub_subgroup()),
            vec![self.cert_vk_repr.as_field(), self.self_vk_repr.as_field()],
        ]
        .concat()
    }
}

#[derive(Clone, Debug)]
pub struct AssignedGlobal {
    //Persistent values that do not change through an ivc stream
    pub(crate) genesis_msg: AssignedNative<F>,
    pub(crate) genesis_vk: AssignedNativePoint<Jubjub>,
    pub(crate) cert_vk: AssignedVk<S>,
    pub(crate) self_vk: AssignedVk<S>,
    // Combined fixed base names for cert_vk and ivc_vk
    pub(crate) fixed_base_names: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct Witness {
    pub(crate) genesis_sig: StandardSchnorrSignature,
    pub(crate) cert_msg: MessageHash,
    pub(crate) cert_merkle_root: MerkleTreeCommitment,
    // Protocol msg preimage bytes
    pub(crate) msg_preimage: ProtocolMessagePreimage,
}

impl Witness {
    #[allow(dead_code)]
    pub(crate) fn new(
        genesis_sig: StandardSchnorrSignature,
        cert_merkle_root: MerkleTreeCommitment,
        cert_msg: MessageHash,
        msg_preimage: ProtocolMessagePreimage,
    ) -> Self {
        Witness {
            genesis_sig,
            cert_merkle_root,
            cert_msg,
            msg_preimage,
        }
    }
}

#[derive(Clone, Debug)]
pub struct AssignedWitness {
    pub(crate) genesis_sig: (AssignedScalarOfNativeCurve<Jubjub>, AssignedNative<F>),
    pub(crate) cert_merkle_root: AssignedNative<F>,
    pub(crate) cert_msg: AssignedNative<F>,
    // Protocol msg preimage bytes
    pub(crate) msg_preimage: Vec<AssignedByte<F>>,
}

pub fn trivial_acc(fixed_base_names: &[String]) -> Accumulator<S> {
    Accumulator::<S>::new(
        Msm::new(&[C::default()], &[F::ONE], &BTreeMap::new()),
        Msm::new(
            &[C::default()],
            &[F::ONE],
            &fixed_base_names.iter().map(|name| (name.clone(), F::ZERO)).collect(),
        ),
    )
}

pub fn fixed_bases_and_names(
    vk_name: &str,
    vk: &VerifyingKey<F, KZGCommitmentScheme<E>>,
) -> (BTreeMap<String, C>, Vec<String>) {
    let mut fixed_bases = BTreeMap::new();
    fixed_bases.insert(String::from("com_instance"), C::identity());
    fixed_bases.extend(verifier::fixed_bases::<S>(vk_name, vk));
    let fixed_base_names = fixed_bases.keys().cloned().collect::<Vec<_>>();
    (fixed_bases, fixed_base_names)
}

pub fn fixed_base_names(vk_name: &str, cs: &ConstraintSystem<F>) -> Vec<String> {
    let mut fixed_base_names = vec![String::from("com_instance")];
    fixed_base_names.extend(verifier::fixed_base_names::<S>(
        vk_name,
        cs.num_fixed_columns() + cs.num_selectors(),
        cs.permutation().columns.len(),
    ));
    fixed_base_names
}
