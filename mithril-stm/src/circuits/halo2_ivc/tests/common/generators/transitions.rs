use ff::Field;
use midnight_circuits::hash::poseidon::PoseidonState;
use midnight_curves::Bls12;
use midnight_proofs::poly::kzg::params::ParamsKZG;
use midnight_zk_stdlib as zk_lib;
use midnight_zk_stdlib::{MidnightVK, Relation};
use rand_core::{CryptoRng, RngCore};
use sha2::{Digest as Sha2Digest, Sha256};

use crate::MithrilMembershipDigest;
use crate::circuits::common::merkle::MerklePath;
use crate::circuits::halo2::circuit::StmCertificateCircuit;
use crate::circuits::halo2::types::CircuitBaseField;
use crate::circuits::halo2::witness::{CircuitMerkleTreeLeaf, CircuitWitnessEntry};
use crate::circuits::halo2_ivc::protocol_message::{ProtocolMessage, ProtocolMessagePartKey};
use crate::circuits::halo2_ivc::state::{State, Witness, fixed_bases_and_names};
use crate::circuits::halo2_ivc::{Accumulator, CERT_VK_NAME, F, PREIMAGE_SIZE, S};
use crate::signature_scheme::{
    BaseFieldElement, SchnorrVerificationKey as StmSchnorrVerificationKey,
};

use super::super::field_encoding::jubjub_base_from_raw_le_bytes;
use super::proofs::verify_prepare_poseidon_ivc;
use super::setup::{AssetGenerationSetup, GENESIS_EPOCH, QUORUM_SIZE};

/// Builds the genesis protocol message whose hash becomes `setup.genesis_message`.
///
/// This is the same genesis message shape used by the asset generators and the
/// recursive base-case path, so future tests can reuse it without re-encoding
/// the protocol-message layout by hand.
pub(super) fn build_genesis_protocol_message(
    avk_hex: &str,
    params_hex: &str,
    genesis_epoch: u64,
) -> ProtocolMessage {
    let mut protocol_message = ProtocolMessage::new();
    protocol_message.set_message_part(
        ProtocolMessagePartKey::SnapshotDigest,
        hex::encode([2u8; 32]),
    );
    protocol_message.set_message_part(
        ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
        avk_hex.to_owned(),
    );
    protocol_message.set_message_part(
        ProtocolMessagePartKey::NextProtocolParameters,
        params_hex.to_owned(),
    );
    protocol_message.set_message_part(
        ProtocolMessagePartKey::CurrentEpoch,
        genesis_epoch.to_string(),
    );
    protocol_message
}

/// Returns the raw genesis protocol-message preimage bytes produced by the serializer.
pub(crate) fn build_genesis_protocol_message_preimage(setup: &AssetGenerationSetup) -> Vec<u8> {
    let params_hex = hex::encode(setup.genesis_next_protocol_params.to_bytes_le());
    build_genesis_protocol_message(&setup.avk_hex, &params_hex, GENESIS_EPOCH)
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("genesis protocol message preimage should succeed")
        .to_vec()
}

/// Builds the witness used by the recursive genesis/base-case branch.
pub(crate) fn build_genesis_base_case_witness(setup: &AssetGenerationSetup) -> Witness {
    let preimage: [u8; PREIMAGE_SIZE] = build_genesis_protocol_message_preimage(setup)
        .try_into()
        .expect("genesis protocol message preimage should be PREIMAGE_SIZE bytes");
    Witness::new(setup.genesis_signature.clone(), F::ZERO, F::ZERO, preimage)
}

/// Builds the first next-state public output produced by the recursive base case.
pub(crate) fn build_genesis_base_case_next_state(
    setup: &AssetGenerationSetup,
    genesis_epoch: u64,
) -> State {
    State::new(
        F::ONE,
        setup.genesis_message,
        F::ZERO,
        setup.genesis_next_merkle_root,
        F::ZERO,
        setup.genesis_next_protocol_params,
        F::from(genesis_epoch),
    )
}

/// Builds the fresh certificate-side data for one normal non-genesis recursive step.
pub(crate) fn build_next_certificate_asset_data(
    setup: &AssetGenerationSetup,
    certificate_commitment_parameters: &ParamsKZG<Bls12>,
    certificate_relation: &StmCertificateCircuit,
    certificate_verifying_key: &MidnightVK,
    recursive_chain_state: &State,
    random_generator: &mut (impl RngCore + CryptoRng),
) -> (Vec<u8>, Accumulator<S>, State, Witness) {
    let merkle_root = recursive_chain_state.next_merkle_root;
    let (message, message_preimage) =
        next_message_and_preimage_for_step(setup, recursive_chain_state);
    let next_state = next_state_for_step(recursive_chain_state, message);
    build_certificate_asset_data_inner(
        setup,
        certificate_commitment_parameters,
        certificate_relation,
        certificate_verifying_key,
        merkle_root,
        message,
        message_preimage,
        next_state,
        random_generator,
    )
}

/// Builds the fresh certificate-side data for one same-epoch recursive step.
pub(crate) fn build_same_epoch_certificate_asset_data(
    setup: &AssetGenerationSetup,
    certificate_commitment_parameters: &ParamsKZG<Bls12>,
    certificate_relation: &StmCertificateCircuit,
    certificate_verifying_key: &MidnightVK,
    recursive_chain_state: &State,
    random_generator: &mut (impl RngCore + CryptoRng),
) -> (Vec<u8>, Accumulator<S>, State, Witness) {
    let merkle_root = recursive_chain_state.merkle_root;
    let (message, message_preimage) =
        same_epoch_message_and_preimage_for_step(setup, recursive_chain_state);
    let next_state = same_epoch_next_state_for_step(recursive_chain_state, message);
    build_certificate_asset_data_inner(
        setup,
        certificate_commitment_parameters,
        certificate_relation,
        certificate_verifying_key,
        merkle_root,
        message,
        message_preimage,
        next_state,
        random_generator,
    )
}

/// Shared inner implementation for building certificate asset data.
///
/// `merkle_root`, `message`, `message_preimage`, and `next_state` are
/// pre-computed by the caller according to the transition type. The signing
/// loop and proof generation are identical for next-epoch and same-epoch steps.
#[allow(clippy::too_many_arguments)]
fn build_certificate_asset_data_inner(
    setup: &AssetGenerationSetup,
    certificate_commitment_parameters: &ParamsKZG<Bls12>,
    certificate_relation: &StmCertificateCircuit,
    certificate_verifying_key: &MidnightVK,
    merkle_root: F,
    message: F,
    message_preimage: Vec<u8>,
    next_state: State,
    random_generator: &mut (impl RngCore + CryptoRng),
) -> (Vec<u8>, Accumulator<S>, State, Witness) {
    let certificate_proving_key = zk_lib::setup_pk(certificate_relation, certificate_verifying_key);
    let (certificate_fixed_bases, _) =
        fixed_bases_and_names(CERT_VK_NAME, certificate_verifying_key.vk());

    assert_eq!(
        merkle_root, setup.genesis_next_merkle_root,
        "merkle_root does not match deterministic setup root"
    );

    let mut certificate_witness_entries: Vec<CircuitWitnessEntry> = vec![];
    for j in 0..QUORUM_SIZE as usize {
        let unique_schnorr_signature = setup.signing_keys[j]
            .sign_unique(
                &[BaseFieldElement::from(merkle_root), BaseFieldElement::from(message)],
                random_generator,
            )
            .expect("certificate witness signature should not fail");
        let leaf = setup.merkle_tree_leaves[j];
        let stm_path = setup
            .merkle_tree
            .compute_merkle_tree_path_fixed_length(j, certificate_relation.merkle_tree_depth());
        setup
            .merkle_tree
            .to_merkle_tree_commitment()
            .verify_leaf_membership_from_path(&leaf, &stm_path)
            .unwrap_or_else(|_| panic!("STM Merkle path should verify for signer index {j}"));
        let schnorr_vk =
            StmSchnorrVerificationKey::new_from_signing_key(setup.signing_keys[j].clone());
        assert_eq!(
            schnorr_vk, leaf.0,
            "STM leaf verification key mismatch for signer index {j}"
        );
        unique_schnorr_signature
            .verify(
                &[BaseFieldElement::from(merkle_root), BaseFieldElement::from(message)],
                &schnorr_vk,
            )
            .expect("fresh certificate signature should verify");
        let circuit_merkle_path = MerklePath::try_from(&stm_path)
            .expect("STM Merkle path should adapt to the circuit witness path");
        let circuit_leaf = CircuitMerkleTreeLeaf(schnorr_vk, CircuitBaseField::from(leaf.1));
        certificate_witness_entries.push(CircuitWitnessEntry {
            leaf: circuit_leaf,
            merkle_path: circuit_merkle_path,
            unique_schnorr_signature,
            lottery_index: (j + 1) as u64,
        });
    }

    let certificate_instance = certificate_public_inputs(merkle_root, next_state.msg);

    let certificate_proof = zk_lib::prove::<StmCertificateCircuit, PoseidonState<F>>(
        certificate_commitment_parameters,
        &certificate_proving_key,
        certificate_relation,
        &(
            CircuitBaseField::from(certificate_instance[0]),
            CircuitBaseField::from(certificate_instance[1]),
        ),
        certificate_witness_entries,
        random_generator,
    )
    .expect("Certificate proof generation should not fail");

    let certificate_dual_msm = verify_prepare_poseidon_ivc(
        certificate_verifying_key.vk(),
        &certificate_proof,
        &certificate_instance,
    );
    assert!(
        certificate_dual_msm
            .clone()
            .check(&certificate_commitment_parameters.verifier_params())
    );
    let mut certificate_accumulator: Accumulator<S> = certificate_dual_msm.into();
    certificate_accumulator.extract_fixed_bases(&certificate_fixed_bases);
    certificate_accumulator.collapse();

    let ivc_witness = Witness::new(
        setup.genesis_signature.clone(),
        merkle_root,
        message,
        message_preimage.try_into().unwrap(),
    );

    (
        certificate_proof,
        certificate_accumulator,
        next_state,
        ivc_witness,
    )
}

/// Formats a `(merkle_root, message)` pair as certificate public inputs.
pub(super) fn certificate_public_inputs(merkle_root: F, message: F) -> Vec<F> {
    StmCertificateCircuit::format_instance(&(
        CircuitBaseField::from(merkle_root),
        CircuitBaseField::from(message),
    ))
    .unwrap()
}

/// Returns the certificate public inputs for one recursive-step transition.
pub(crate) fn certificate_public_inputs_for_step(
    previous_state: &State,
    next_state: &State,
) -> Vec<F> {
    certificate_public_inputs(previous_state.next_merkle_root, next_state.msg)
}

/// Returns the deterministic next certificate message and preimage for one
/// non-genesis recursive step.
pub(crate) fn next_message_and_preimage_for_step(
    setup: &AssetGenerationSetup,
    previous_state: &State,
) -> (F, Vec<u8>) {
    let current_epoch = current_epoch_from_state(previous_state);
    let step = step_index_from_state(previous_state);
    let params_hex = hex::encode(setup.genesis_next_protocol_params.to_bytes_le());

    let mut protocol_message = ProtocolMessage::new();
    protocol_message.set_message_part(
        ProtocolMessagePartKey::SnapshotDigest,
        hex::encode(vec![(step as u8) + 2; 32]),
    );
    protocol_message.set_message_part(
        ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
        setup.avk_hex.clone(),
    );
    protocol_message.set_message_part(ProtocolMessagePartKey::NextProtocolParameters, params_hex);
    protocol_message.set_message_part(
        ProtocolMessagePartKey::CurrentEpoch,
        (current_epoch + 1).to_string(),
    );

    let preimage = protocol_message
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("protocol message preimage should succeed");
    let message_hash = Sha256::digest(&preimage);
    (
        jubjub_base_from_raw_le_bytes(message_hash.as_ref()),
        preimage.to_vec(),
    )
}

/// Returns the deterministic certificate message and preimage for one
/// same-epoch recursive step.
pub(crate) fn same_epoch_message_and_preimage_for_step(
    setup: &AssetGenerationSetup,
    previous_state: &State,
) -> (F, Vec<u8>) {
    let current_epoch = current_epoch_from_state(previous_state);
    let step = step_index_from_state(previous_state);
    let params_hex = hex::encode(setup.genesis_next_protocol_params.to_bytes_le());

    let mut protocol_message = ProtocolMessage::new();
    protocol_message.set_message_part(
        ProtocolMessagePartKey::SnapshotDigest,
        hex::encode(vec![(step as u8) + 2; 32]),
    );
    protocol_message.set_message_part(
        ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
        setup.avk_hex.clone(),
    );
    protocol_message.set_message_part(ProtocolMessagePartKey::NextProtocolParameters, params_hex);
    protocol_message.set_message_part(
        ProtocolMessagePartKey::CurrentEpoch,
        current_epoch.to_string(),
    );

    let preimage = protocol_message
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("protocol message preimage should succeed");
    let message_hash = Sha256::digest(&preimage);
    (
        jubjub_base_from_raw_le_bytes(message_hash.as_ref()),
        preimage.to_vec(),
    )
}

/// Returns the recursive next state for a non-genesis step once the next
/// certificate message has been fixed.
pub(crate) fn next_state_for_step(previous_state: &State, message: F) -> State {
    let current_epoch = current_epoch_from_state(previous_state);
    let step = step_index_from_state(previous_state);

    State::new(
        F::from((step + 1) as u64),
        message,
        previous_state.next_merkle_root,
        previous_state.next_merkle_root,
        previous_state.next_protocol_params,
        previous_state.next_protocol_params,
        F::from(current_epoch + 1),
    )
}

/// Returns the recursive next state for a same-epoch step.
pub(crate) fn same_epoch_next_state_for_step(previous_state: &State, message: F) -> State {
    let current_epoch = current_epoch_from_state(previous_state);
    let step = step_index_from_state(previous_state);

    State::new(
        F::from((step + 1) as u64),
        message,
        previous_state.merkle_root,
        previous_state.next_merkle_root,
        previous_state.protocol_params,
        previous_state.next_protocol_params,
        F::from(current_epoch),
    )
}

fn current_epoch_from_state(previous_state: &State) -> u64 {
    let bytes = previous_state.current_epoch.to_bytes_le();
    u64::from_le_bytes(bytes[0..8].try_into().unwrap())
}

fn step_index_from_state(previous_state: &State) -> usize {
    let bytes = previous_state.counter.to_bytes_le();
    u64::from_le_bytes(bytes[0..8].try_into().unwrap()) as usize
}
