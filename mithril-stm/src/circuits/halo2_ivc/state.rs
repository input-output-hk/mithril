use super::{
    Accumulator, AssignedByte, AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve,
    AssignedVk, C, ConstraintSystem, E, F, Instantiable, Jubjub, KZGCommitmentScheme, Msm,
    PREIMAGE_SIZE, S, VerifyingKey, verifier,
};
use crate::circuits::halo2_ivc::helpers::signatures::schnorr_signature::{
    Signature as SchnorrSignature, VerificationKey as SchnorrVerificationKey,
};
use ff::Field;
use group::Group;
use std::collections::BTreeMap;

#[derive(Clone, Debug)]
pub struct State {
    pub(crate) counter: F,
    pub(crate) msg: F,
    pub(crate) merkle_root: F,
    pub(crate) next_merkle_root: F,
    pub(crate) protocol_params: F,
    pub(crate) next_protocol_params: F,
    pub(crate) current_epoch: F,
}

impl State {
    pub fn new(
        counter: F,
        msg: F,
        merkle_root: F,
        next_merkle_root: F,
        protocol_params: F,
        next_protocol_params: F,
        current_epoch: F,
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

    pub fn genesis() -> Self {
        State {
            counter: F::ZERO,
            msg: F::ZERO,
            merkle_root: F::ZERO,
            next_merkle_root: F::ZERO,
            protocol_params: F::ZERO,
            next_protocol_params: F::ZERO,
            current_epoch: F::ZERO,
        }
    }

    pub fn as_public_input(&self) -> Vec<F> {
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
    pub(crate) genesis_msg: F,
    pub(crate) genesis_vk: SchnorrVerificationKey,
    // cert_vk hash
    pub(crate) cert_vk_repr: F,
    // ivc_vk hash
    pub(crate) self_vk_repr: F,
}

impl Global {
    pub fn new(
        genesis_msg: F,
        genesis_vk: SchnorrVerificationKey,
        cert_vk: &VerifyingKey<F, KZGCommitmentScheme<E>>,
        self_vk: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    ) -> Self {
        Global {
            genesis_msg,
            genesis_vk,
            cert_vk_repr: cert_vk.transcript_repr(),
            self_vk_repr: self_vk.transcript_repr(),
        }
    }

    pub fn as_public_input(&self) -> Vec<F> {
        [
            vec![self.genesis_msg],
            AssignedNativePoint::<Jubjub>::as_public_input(&self.genesis_vk.0),
            vec![self.cert_vk_repr, self.self_vk_repr],
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
    pub(crate) genesis_sig: SchnorrSignature,
    pub(crate) cert_msg: F,
    pub(crate) cert_merkle_root: F,
    // Protocol msg preimage bytes
    pub(crate) msg_preimage: [u8; PREIMAGE_SIZE],
}

impl Witness {
    pub fn new(
        genesis_sig: SchnorrSignature,
        cert_merkle_root: F,
        cert_msg: F,
        msg_preimage: [u8; PREIMAGE_SIZE],
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
