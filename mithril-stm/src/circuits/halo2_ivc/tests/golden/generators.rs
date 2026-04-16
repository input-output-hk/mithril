use std::{
    fs::{self, File},
    io::{BufWriter, Write as IoWrite},
    path::PathBuf,
    time::Instant,
};

use ff::Field;
use group::Group;
use midnight_circuits::{hash::poseidon::PoseidonState, types::Instantiable};
use midnight_curves::Bls12;
use midnight_proofs::{
    plonk::{ProvingKey, VerifyingKey, create_proof, keygen_pk, keygen_vk_with_k, prepare},
    poly::kzg::{
        KZGCommitmentScheme,
        msm::DualMSM,
        params::{ParamsKZG, ParamsVerifierKZG},
    },
    transcript::{CircuitTranscript, Transcript},
    utils::{SerdeFormat, helpers::ProcessedSerdeObject},
};
use midnight_zk_stdlib as zk_lib;
use midnight_zk_stdlib::{MidnightVK, Relation};
use rand_chacha::ChaCha20Rng;
use rand_core::{CryptoRng, OsRng, RngCore, SeedableRng};

use crate::circuits::halo2_ivc::helpers::{
    merkle_tree::{MTLeaf as MerkleTreeLeaf, MerklePath, MerkleTree},
    protocol_message::{AggregateVerificationKey, ProtocolMessage, ProtocolMessagePartKey},
    signatures::{
        schnorr_signature::{
            Signature as SchnorrSignature, SigningKey as SchnorrSigningKey,
            VerificationKey as SchnorrVerificationKey,
        },
        unique_signature::{Signature, SigningKey, VerificationKey},
    },
    utils::jubjub_base_from_le_bytes,
};
use crate::circuits::halo2_ivc::state::{State, Witness, fixed_bases_and_names, trivial_acc};
use crate::circuits::halo2_ivc::tests::{
    golden::asset_readers::load_recursive_chain_state_asset, test_certificate::Certificate,
};
use crate::circuits::halo2_ivc::{
    Accumulator, AssignedAccumulator, C, CERT_VK_NAME, E, F, IVC_ONE_NAME, PREIMAGE_SIZE,
    circuit::IvcCircuit, io::Write, state::Global,
};

/// Fixed seed used for the deterministic universal KZG setup.
const ASSET_SEED: u64 = 42;
const CERTIFICATE_CIRCUIT_DEGREE: u32 = 13;
const RECURSIVE_CIRCUIT_DEGREE: u32 = 19;
const INITIAL_CHAIN_LENGTH: usize = 3;
const SIGNER_COUNT: usize = 3000;
const QUORUM_SIZE: u32 = 2;

type CertificateWitnessEntry = (MerkleTreeLeaf, MerklePath, Signature, u32);

/// Paths for the minimal stored asset set used by asset-based golden tests.
#[derive(Debug, Clone)]
pub(crate) struct AssetPaths {
    pub(crate) recursive_chain_state: PathBuf,
    pub(crate) verification_context: PathBuf,
    pub(crate) recursive_step_output: PathBuf,
}

impl AssetPaths {
    pub(crate) fn new(base_dir: PathBuf) -> Self {
        Self {
            recursive_chain_state: base_dir.join("recursive_chain_state.bin"),
            verification_context: base_dir.join("verification_context.bin"),
            recursive_step_output: base_dir.join("recursive_step_output.bin"),
        }
    }
}

pub(crate) fn default_asset_paths() -> AssetPaths {
    AssetPaths::new(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src/circuits/halo2_ivc/tests/golden/assets"),
    )
}

/// Deterministic setup data for asset generation.
///
/// This carries the shared `halo2_ivc` context needed to reproduce the same
/// stored asset contents across runs.
#[derive(Debug)]
pub(crate) struct AssetGenerationSetup {
    pub(crate) certificate_relation: Certificate,
    pub(crate) genesis_verification_key: SchnorrVerificationKey,
    pub(crate) genesis_message: F,
    pub(crate) genesis_signature: SchnorrSignature,
    pub(crate) merkle_tree: MerkleTree,
    pub(crate) merkle_tree_leaves: Vec<MerkleTreeLeaf>,
    pub(crate) signing_keys: Vec<SigningKey>,
    pub(crate) aggregate_verification_key: AggregateVerificationKey,
    pub(crate) genesis_next_merkle_root: F,
    pub(crate) genesis_next_protocol_params: F,
}

fn write_shared_verifier_params(
    writer: &mut impl IoWrite,
    verifier_params: &ParamsVerifierKZG<Bls12>,
) {
    verifier_params
        .write(writer, SerdeFormat::RawBytesUnchecked)
        .expect("failed to write verifier-side SRS data");
}

/// Wraps the protocol-message API so the call sites stay explicit about which
/// fields define a generated asset.
fn insert_protocol_message_part(
    protocol_message: &mut ProtocolMessage,
    part_key: ProtocolMessagePartKey,
    encoded_value: Vec<u8>,
) {
    // Keep the protocol-message boundary explicit at the call site.
    protocol_message.set_message_part(part_key, encoded_value);
}

/// Builds the genesis protocol message whose hash becomes `setup.genesis_message`.
///
/// This is the same genesis message shape used by the asset generators and the
/// recursive base-case path, so future tests can reuse it without re-encoding
/// the protocol-message layout by hand.
fn build_genesis_protocol_message(
    aggregate_verification_key: &AggregateVerificationKey,
    genesis_next_protocol_params: F,
    genesis_epoch: u64,
) -> ProtocolMessage {
    let mut protocol_message = ProtocolMessage::new();
    insert_protocol_message_part(
        &mut protocol_message,
        ProtocolMessagePartKey::Digest,
        vec![2u8; 32],
    );
    insert_protocol_message_part(
        &mut protocol_message,
        ProtocolMessagePartKey::NextAggregateVerificationKey,
        aggregate_verification_key.clone().into(),
    );
    insert_protocol_message_part(
        &mut protocol_message,
        ProtocolMessagePartKey::NextProtocolParameters,
        genesis_next_protocol_params.to_bytes_le().to_vec(),
    );
    insert_protocol_message_part(
        &mut protocol_message,
        ProtocolMessagePartKey::CurrentEpoch,
        genesis_epoch.to_le_bytes().into(),
    );
    protocol_message
}

/// Builds the genesis protocol-message preimage consumed by the recursive base case.
///
/// The circuit hashes these bytes and compares the result with
/// `global.genesis_msg`, so this helper keeps the preimage aligned with the
/// genesis message construction used during asset generation.
pub(crate) fn build_genesis_protocol_message_preimage(
    setup: &AssetGenerationSetup,
) -> [u8; PREIMAGE_SIZE] {
    build_genesis_protocol_message(
        &setup.aggregate_verification_key,
        setup.genesis_next_protocol_params,
        5u64,
    )
    .get_preimage()
    .try_into()
    .expect("genesis protocol message preimage should match PREIMAGE_SIZE")
}

/// Builds the witness used by the recursive genesis/base-case branch.
///
/// The genesis branch selects `global.genesis_msg` and a zero merkle root in
/// circuit, so the certificate-specific witness fields remain zero here while
/// the genesis signature and preimage carry the real trusted input.
pub(crate) fn build_genesis_base_case_witness(setup: &AssetGenerationSetup) -> Witness {
    Witness::new(
        setup.genesis_signature.clone(),
        F::ZERO,
        F::ZERO,
        build_genesis_protocol_message_preimage(setup),
    )
}

/// Builds the first next-state public output produced by the recursive base case.
///
/// This matches the transition logic in the circuit when the previous state is
/// `State::genesis()`: the message and next-merkle-root come from the genesis
/// setup, while the current merkle root and protocol parameters remain zero.
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

fn build_merkle_tree(
    random_generator: &mut (impl RngCore + CryptoRng),
    signer_count: usize,
) -> (Vec<SigningKey>, Vec<MerkleTreeLeaf>, MerkleTree) {
    let mut signing_keys = Vec::with_capacity(signer_count);
    let mut merkle_tree_leaves = Vec::with_capacity(signer_count);
    for _ in 0..signer_count {
        let signing_key = SigningKey::generate(random_generator);
        let verification_key = VerificationKey::from(&signing_key);
        merkle_tree_leaves.push(MerkleTreeLeaf(verification_key, -F::ONE));
        signing_keys.push(signing_key);
    }

    let merkle_tree = MerkleTree::create(&merkle_tree_leaves);
    (signing_keys, merkle_tree_leaves, merkle_tree)
}

/// Builds the shared universal KZG parameters that both circuits derive from.
fn build_deterministic_params(circuit_degree: u32) -> ParamsKZG<Bls12> {
    ParamsKZG::<Bls12>::unsafe_setup(circuit_degree, ChaCha20Rng::seed_from_u64(ASSET_SEED))
}

/// Generates a recursive proof using the Poseidon transcript.
fn prove_poseidon_ivc(
    commitment_parameters: &ParamsKZG<Bls12>,
    proving_key: &ProvingKey<F, KZGCommitmentScheme<E>>,
    circuit: &IvcCircuit,
    public_inputs: &[F],
    random_generator: impl RngCore + rand_core::CryptoRng,
) -> Vec<u8> {
    let mut transcript = CircuitTranscript::<PoseidonState<F>>::init();
    create_proof::<F, KZGCommitmentScheme<E>, CircuitTranscript<PoseidonState<F>>, IvcCircuit>(
        commitment_parameters,
        proving_key,
        &[circuit.clone()],
        1,
        &[&[&[], public_inputs]],
        random_generator,
        &mut transcript,
    )
    .expect("IVC proof generation should not fail");
    transcript.finalize()
}

/// Verifies a recursive proof using the Poseidon transcript and returns the prepared MSM.
fn verify_and_prepare_poseidon_ivc(
    verifying_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    proof: &[u8],
    public_inputs: &[F],
) -> DualMSM<E> {
    let mut transcript = CircuitTranscript::<PoseidonState<F>>::init_from_bytes(proof);
    let dual_msm = prepare::<F, KZGCommitmentScheme<E>, CircuitTranscript<PoseidonState<F>>>(
        verifying_key,
        &[&[C::identity()]],
        &[&[public_inputs]],
        &mut transcript,
    )
    .expect("Verification failed");
    transcript.assert_empty().expect("Transcript should be empty");
    dual_msm
}

/// Generates the final recursive proof using the Blake2b transcript.
fn prove_blake2b_ivc(
    commitment_parameters: &ParamsKZG<Bls12>,
    proving_key: &ProvingKey<F, KZGCommitmentScheme<E>>,
    circuit: &IvcCircuit,
    public_inputs: &[F],
    random_generator: impl RngCore + rand_core::CryptoRng,
) -> Vec<u8> {
    let mut transcript = CircuitTranscript::<blake2b_simd::State>::init();
    create_proof::<F, KZGCommitmentScheme<E>, CircuitTranscript<blake2b_simd::State>, IvcCircuit>(
        commitment_parameters,
        proving_key,
        &[circuit.clone()],
        1,
        &[&[&[], public_inputs]],
        random_generator,
        &mut transcript,
    )
    .expect("IVC Blake2b proof generation should not fail");
    transcript.finalize()
}

/// Verifies the final recursive proof using the Blake2b transcript.
fn verify_and_prepare_blake2b_ivc(
    verifying_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    proof: &[u8],
    public_inputs: &[F],
) -> DualMSM<E> {
    let mut transcript = CircuitTranscript::<blake2b_simd::State>::init_from_bytes(proof);
    let dual_msm = prepare::<F, KZGCommitmentScheme<E>, CircuitTranscript<blake2b_simd::State>>(
        verifying_key,
        &[&[C::identity()]],
        &[&[public_inputs]],
        &mut transcript,
    )
    .expect("Verification failed");
    transcript.assert_empty().expect("Transcript should be empty");
    dual_msm
}

/// Builds the fresh certificate-side data for one normal non-genesis recursive step.
///
/// This reuses the same flow as the recursive-step asset generator:
/// - derive the next protocol message from the stored recursive state
/// - generate one fresh certificate proof for that message
/// - derive the certificate accumulator contribution
/// - return the next recursive witness and next state expected by the circuit
///
/// Golden tests reuse this helper so the "normal recursive-step" scenario stays
/// aligned with the existing asset-generation path rather than rebuilding the
/// message/proof/state logic independently.
pub(crate) fn build_next_certificate_asset_data(
    setup: &AssetGenerationSetup,
    certificate_commitment_parameters: &ParamsKZG<Bls12>,
    certificate_relation: &Certificate,
    certificate_verifying_key: &MidnightVK,
    recursive_chain_state: &State,
    random_generator: &mut (impl RngCore + CryptoRng),
) -> (
    Vec<u8>,
    Accumulator<crate::circuits::halo2_ivc::S>,
    State,
    Witness,
) {
    let certificate_proving_key = zk_lib::setup_pk(certificate_relation, certificate_verifying_key);
    let (certificate_fixed_bases, _) =
        fixed_bases_and_names(CERT_VK_NAME, certificate_verifying_key.vk());

    let merkle_root = recursive_chain_state.next_merkle_root;
    let (message, message_preimage) =
        next_message_and_preimage_for_step(setup, recursive_chain_state);
    let mut certificate_witness_entries: Vec<CertificateWitnessEntry> = vec![];
    assert_eq!(
        merkle_root, setup.genesis_next_merkle_root,
        "recursive_chain_state next_merkle_root does not match deterministic setup root"
    );
    for j in 0..QUORUM_SIZE as usize {
        let signature = setup.signing_keys[j].sign(&[merkle_root, message], random_generator);
        let merkle_path = setup.merkle_tree.get_path(j);
        let computed_root = merkle_path.compute_root(setup.merkle_tree_leaves[j]);
        assert_eq!(
            merkle_root, computed_root,
            "merkle path root mismatch for signer index {j}"
        );
        signature
            .verify(&[merkle_root, message], &setup.merkle_tree_leaves[j].0)
            .expect("fresh certificate signature should verify");
        certificate_witness_entries.push((
            setup.merkle_tree_leaves[j],
            merkle_path,
            signature,
            (j + 1) as u32,
        ));
    }

    let next_state = next_state_for_step(recursive_chain_state, message);
    let certificate_instance =
        certificate_public_inputs_for_step(recursive_chain_state, &next_state);

    let certificate_proof = zk_lib::prove::<Certificate, PoseidonState<F>>(
        certificate_commitment_parameters,
        &certificate_proving_key,
        certificate_relation,
        &(certificate_instance[0], certificate_instance[1]),
        certificate_witness_entries,
        random_generator,
    )
    .expect("Certificate proof generation should not fail");

    let certificate_dual_msm = verify_and_prepare_poseidon_ivc(
        certificate_verifying_key.vk(),
        &certificate_proof,
        &certificate_instance,
    );
    assert!(
        certificate_dual_msm
            .clone()
            .check(&certificate_commitment_parameters.verifier_params())
    );
    let mut certificate_accumulator: Accumulator<crate::circuits::halo2_ivc::S> =
        certificate_dual_msm.into();
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

/// Returns the certificate public inputs for one recursive-step transition.
///
/// The certificate relation for a non-genesis step is parameterized by:
/// - the previous state's `next_merkle_root`
/// - the next state's `msg`
///
/// The generator path and the chained-flow golden test both use this helper so
/// they prepare the certificate proof against the same public-input contract.
pub(crate) fn certificate_public_inputs_for_step(
    previous_state: &State,
    next_state: &State,
) -> Vec<F> {
    Certificate::format_instance(&(previous_state.next_merkle_root, next_state.msg)).unwrap()
}

/// Returns the deterministic next certificate message and preimage for one
/// non-genesis recursive step.
pub(crate) fn next_message_and_preimage_for_step(
    setup: &AssetGenerationSetup,
    previous_state: &State,
) -> (F, Vec<u8>) {
    let current_epoch = {
        let bytes = previous_state.current_epoch.to_bytes_le();
        u64::from_le_bytes(bytes[0..8].try_into().unwrap())
    };
    let step = {
        let bytes = previous_state.counter.to_bytes_le();
        u64::from_le_bytes(bytes[0..8].try_into().unwrap()) as usize
    };

    let mut protocol_message = ProtocolMessage::new();
    // These entries define the certificate message consumed by the recursive step.
    insert_protocol_message_part(
        &mut protocol_message,
        ProtocolMessagePartKey::Digest,
        vec![(step as u8) + 2; 32],
    );
    insert_protocol_message_part(
        &mut protocol_message,
        ProtocolMessagePartKey::NextAggregateVerificationKey,
        setup.aggregate_verification_key.clone().into(),
    );
    insert_protocol_message_part(
        &mut protocol_message,
        ProtocolMessagePartKey::NextProtocolParameters,
        setup.genesis_next_protocol_params.to_bytes_le().to_vec(),
    );
    insert_protocol_message_part(
        &mut protocol_message,
        ProtocolMessagePartKey::CurrentEpoch,
        (current_epoch + 1).to_le_bytes().into(),
    );

    let message_preimage = protocol_message.get_preimage();
    let message_hash = protocol_message.compute_hash();
    (jubjub_base_from_le_bytes(&message_hash), message_preimage)
}

/// Returns the recursive next state for a non-genesis step once the next
/// certificate message has been fixed.
pub(crate) fn next_state_for_step(previous_state: &State, message: F) -> State {
    let current_epoch = {
        let bytes = previous_state.current_epoch.to_bytes_le();
        u64::from_le_bytes(bytes[0..8].try_into().unwrap())
    };
    let step = {
        let bytes = previous_state.counter.to_bytes_le();
        u64::from_le_bytes(bytes[0..8].try_into().unwrap()) as usize
    };

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

/// Builds the deterministic shared setup used by all asset generators.
pub(crate) fn build_asset_generation_setup() -> AssetGenerationSetup {
    let mut rng = ChaCha20Rng::seed_from_u64(ASSET_SEED);

    let depth = SIGNER_COUNT.next_power_of_two().trailing_zeros();
    let number_of_lotteries = QUORUM_SIZE * 10;
    let total_stake = 1_000_000u64;

    let certificate_relation = Certificate::new(QUORUM_SIZE, number_of_lotteries, depth);
    let (signing_keys, merkle_tree_leaves, merkle_tree) = build_merkle_tree(&mut rng, SIGNER_COUNT);
    let aggregate_verification_key =
        AggregateVerificationKey::new(merkle_tree.to_merkle_tree_commitment(), total_stake);

    let genesis_signing_key = SchnorrSigningKey::generate(&mut rng);
    let genesis_verification_key = SchnorrVerificationKey::from(&genesis_signing_key);
    let genesis_epoch = 5u64;
    let genesis_next_merkle_root = merkle_tree.root();
    let genesis_next_protocol_params = F::from(7u64);

    let genesis_message = {
        let protocol_message = build_genesis_protocol_message(
            &aggregate_verification_key,
            genesis_next_protocol_params,
            genesis_epoch,
        );
        let message_hash = protocol_message.compute_hash();
        jubjub_base_from_le_bytes(&message_hash)
    };

    let genesis_signature = genesis_signing_key.sign(&[genesis_message], &mut rng);
    genesis_signature
        .verify(&[genesis_message], &genesis_verification_key)
        .expect("deterministic genesis signature should verify");

    AssetGenerationSetup {
        certificate_relation,
        genesis_verification_key,
        genesis_message,
        genesis_signature,
        merkle_tree,
        merkle_tree_leaves,
        signing_keys,
        aggregate_verification_key,
        genesis_next_merkle_root,
        genesis_next_protocol_params,
    }
}

/// Generates and writes the stored recursive chain snapshot asset.
pub(crate) fn generate_recursive_chain_state_asset(
    setup: &AssetGenerationSetup,
    paths: &AssetPaths,
) {
    println!(
        "generate_recursive_chain_state: start -> {}",
        paths.recursive_chain_state.display()
    );
    let total_start = Instant::now();

    let shared_srs_degree = RECURSIVE_CIRCUIT_DEGREE.max(CERTIFICATE_CIRCUIT_DEGREE);
    let universal_kzg_parameters = build_deterministic_params(shared_srs_degree);
    let universal_verifier_params = universal_kzg_parameters.verifier_params();

    let certificate_commitment_parameters = {
        let mut certificate_commitment_parameters = universal_kzg_parameters.clone();
        if CERTIFICATE_CIRCUIT_DEGREE < shared_srs_degree {
            certificate_commitment_parameters.downsize(CERTIFICATE_CIRCUIT_DEGREE);
        }
        certificate_commitment_parameters
    };

    let recursive_commitment_parameters = {
        let mut recursive_commitment_parameters = universal_kzg_parameters.clone();
        if RECURSIVE_CIRCUIT_DEGREE < shared_srs_degree {
            recursive_commitment_parameters.downsize(RECURSIVE_CIRCUIT_DEGREE);
        }
        recursive_commitment_parameters
    };

    let certificate_verifying_key = zk_lib::setup_vk(
        &certificate_commitment_parameters,
        &setup.certificate_relation,
    );
    let certificate_proving_key =
        zk_lib::setup_pk(&setup.certificate_relation, &certificate_verifying_key);
    println!("generate_recursive_chain_state: certificate verifying/proving keys ready");

    let (certificate_fixed_bases, certificate_fixed_base_names) =
        fixed_bases_and_names(CERT_VK_NAME, certificate_verifying_key.vk());
    let mut certificate_proofs = vec![vec![]];
    let mut certificate_accumulators = vec![trivial_acc(&certificate_fixed_base_names)];

    let default_ivc_circuit = IvcCircuit::unknown(certificate_verifying_key.vk());
    let recursive_verifying_key = keygen_vk_with_k(
        &recursive_commitment_parameters,
        &default_ivc_circuit,
        RECURSIVE_CIRCUIT_DEGREE,
    )
    .expect("IVC verifying key generation should not fail");
    let recursive_proving_key = keygen_pk(recursive_verifying_key.clone(), &default_ivc_circuit)
        .expect("IVC proving key generation should not fail");
    println!("generate_recursive_chain_state: recursive verifying/proving keys ready");

    let (recursive_fixed_bases, _) = fixed_bases_and_names(IVC_ONE_NAME, &recursive_verifying_key);
    let mut combined_fixed_bases = certificate_fixed_bases.clone();
    combined_fixed_bases.extend(recursive_fixed_bases.clone());
    let combined_fixed_base_names = combined_fixed_bases.keys().cloned().collect::<Vec<_>>();

    let global = Global::new(
        setup.genesis_message,
        setup.genesis_verification_key.clone(),
        certificate_verifying_key.vk(),
        &recursive_verifying_key,
    );

    let mut certificate_random_generator = OsRng;
    let mut current_epoch = 5u64;
    let mut recursive_next_states = vec![State::new(
        F::ONE,
        setup.genesis_message,
        F::ZERO,
        setup.genesis_next_merkle_root,
        F::ZERO,
        setup.genesis_next_protocol_params,
        F::from(current_epoch),
    )];
    let mut recursive_witnesses =
        vec![Witness::new(setup.genesis_signature.clone(), F::ZERO, F::ZERO, {
            let mut protocol_message = ProtocolMessage::new();
            insert_protocol_message_part(
                &mut protocol_message,
                ProtocolMessagePartKey::Digest,
                vec![2u8; 32],
            );
            insert_protocol_message_part(
                &mut protocol_message,
                ProtocolMessagePartKey::NextAggregateVerificationKey,
                setup.aggregate_verification_key.clone().into(),
            );
            insert_protocol_message_part(
                &mut protocol_message,
                ProtocolMessagePartKey::NextProtocolParameters,
                setup.genesis_next_protocol_params.to_bytes_le().to_vec(),
            );
            insert_protocol_message_part(
                &mut protocol_message,
                ProtocolMessagePartKey::CurrentEpoch,
                current_epoch.to_le_bytes().into(),
            );
            protocol_message.get_preimage().try_into().unwrap()
        })];

    for step in 1..=INITIAL_CHAIN_LENGTH {
        println!(
            "generate_recursive_chain_state: certificate proof {step}/{INITIAL_CHAIN_LENGTH} starting"
        );
        let certificate_step_start = Instant::now();
        current_epoch += 1;
        let (message, message_preimage) = {
            let mut protocol_message = ProtocolMessage::new();
            insert_protocol_message_part(
                &mut protocol_message,
                ProtocolMessagePartKey::Digest,
                vec![(step as u8) + 2; 32],
            );
            insert_protocol_message_part(
                &mut protocol_message,
                ProtocolMessagePartKey::NextAggregateVerificationKey,
                setup.aggregate_verification_key.clone().into(),
            );
            insert_protocol_message_part(
                &mut protocol_message,
                ProtocolMessagePartKey::NextProtocolParameters,
                setup.genesis_next_protocol_params.to_bytes_le().to_vec(),
            );
            insert_protocol_message_part(
                &mut protocol_message,
                ProtocolMessagePartKey::CurrentEpoch,
                current_epoch.to_le_bytes().into(),
            );
            let message_preimage = protocol_message.get_preimage();
            let message_hash = protocol_message.compute_hash();
            (jubjub_base_from_le_bytes(&message_hash), message_preimage)
        };

        recursive_next_states.push(State::new(
            F::from((step + 1) as u64),
            message,
            setup.genesis_next_merkle_root,
            setup.genesis_next_merkle_root,
            setup.genesis_next_protocol_params,
            setup.genesis_next_protocol_params,
            F::from(current_epoch),
        ));
        recursive_witnesses.push(Witness::new(
            setup.genesis_signature.clone(),
            setup.genesis_next_merkle_root,
            message,
            message_preimage.try_into().unwrap(),
        ));

        let certificate_instance =
            Certificate::format_instance(&(setup.genesis_next_merkle_root, message)).unwrap();
        let mut certificate_witness_entries: Vec<CertificateWitnessEntry> = vec![];
        for j in 0..QUORUM_SIZE as usize {
            let signature = setup.signing_keys[j].sign(
                &[setup.genesis_next_merkle_root, message],
                &mut certificate_random_generator,
            );
            let merkle_path = setup.merkle_tree.get_path(j);
            certificate_witness_entries.push((
                setup.merkle_tree_leaves[j],
                merkle_path,
                signature,
                (j + 1) as u32,
            ));
        }

        let certificate_proof = zk_lib::prove::<Certificate, PoseidonState<F>>(
            &certificate_commitment_parameters,
            &certificate_proving_key,
            &setup.certificate_relation,
            &(certificate_instance[0], certificate_instance[1]),
            certificate_witness_entries,
            &mut certificate_random_generator,
        )
        .expect("Certificate proof generation should not fail");

        let certificate_dual_msm = verify_and_prepare_poseidon_ivc(
            certificate_verifying_key.vk(),
            &certificate_proof,
            &certificate_instance,
        );
        assert!(
            certificate_dual_msm
                .clone()
                .check(&certificate_commitment_parameters.verifier_params())
        );
        let mut certificate_accumulator: Accumulator<crate::circuits::halo2_ivc::S> =
            certificate_dual_msm.into();
        certificate_accumulator.extract_fixed_bases(&certificate_fixed_bases);
        certificate_accumulator.collapse();

        println!(
            "generate_recursive_chain_state: certificate proof {step}/{INITIAL_CHAIN_LENGTH} done in {:?}",
            certificate_step_start.elapsed()
        );

        certificate_proofs.push(certificate_proof);
        certificate_accumulators.push(certificate_accumulator);
    }

    let mut current_state = State::genesis();
    let mut recursive_proof = vec![];
    let mut current_accumulator = trivial_acc(&combined_fixed_base_names);
    let mut next_accumulator = current_accumulator.clone();
    let mut recursive_random_generator = OsRng;

    for i in 0..=INITIAL_CHAIN_LENGTH {
        println!(
            "generate_recursive_chain_state: recursive proof {}/{} starting",
            i + 1,
            INITIAL_CHAIN_LENGTH + 1
        );
        let recursive_step_start = Instant::now();
        let circuit = IvcCircuit::new(
            global.clone(),
            current_state.clone(),
            recursive_witnesses[i].clone(),
            certificate_proofs[i].clone(),
            recursive_proof.clone(),
            current_accumulator.clone(),
            certificate_verifying_key.vk(),
            &recursive_verifying_key,
        );

        let public_inputs = [
            global.as_public_input(),
            recursive_next_states[i].as_public_input(),
            AssignedAccumulator::as_public_input(&next_accumulator),
        ]
        .concat();

        let proof = prove_poseidon_ivc(
            &recursive_commitment_parameters,
            &recursive_proving_key,
            &circuit,
            &public_inputs,
            &mut recursive_random_generator,
        );
        let dual_msm =
            verify_and_prepare_poseidon_ivc(&recursive_verifying_key, &proof, &public_inputs);
        assert!(dual_msm.clone().check(&universal_verifier_params));

        let mut proof_accumulator: Accumulator<crate::circuits::halo2_ivc::S> = dual_msm.into();
        proof_accumulator.extract_fixed_bases(&recursive_fixed_bases);
        proof_accumulator.collapse();

        println!(
            "generate_recursive_chain_state: recursive proof {}/{} done in {:?}",
            i + 1,
            INITIAL_CHAIN_LENGTH + 1,
            recursive_step_start.elapsed()
        );

        current_state = recursive_next_states[i].clone();
        current_accumulator = next_accumulator.clone();
        recursive_proof = proof;

        if i < INITIAL_CHAIN_LENGTH {
            let mut accumulated_accumulator = Accumulator::accumulate(&[
                next_accumulator.clone(),
                certificate_accumulators[i + 1].clone(),
                proof_accumulator,
            ]);
            accumulated_accumulator.collapse();
            assert!(
                accumulated_accumulator.check(
                    &universal_kzg_parameters.s_g2().into(),
                    &combined_fixed_bases,
                ),
                "recursive accumulator verification failed at step {i}"
            );
            next_accumulator = accumulated_accumulator;
        }
    }

    if let Some(parent) = paths.recursive_chain_state.parent() {
        fs::create_dir_all(parent).expect("failed to create recursive_chain_state asset directory");
    }
    assert_eq!(
        current_state.next_merkle_root, setup.genesis_next_merkle_root,
        "recursive_chain_state writer is about to persist a next_merkle_root that does not match setup"
    );
    println!(
        "generate_recursive_chain_state: writing asset -> {}",
        paths.recursive_chain_state.display()
    );
    {
        let mut writer = BufWriter::new(
            File::create(&paths.recursive_chain_state)
                .expect("failed to create recursive_chain_state"),
        );

        for value in global.as_public_input() {
            writer.write_all(&value.to_bytes_le()).unwrap();
        }
        for value in current_state.as_public_input() {
            writer.write_all(&value.to_bytes_le()).unwrap();
        }
        writer
            .write_all(&(recursive_proof.len() as u32).to_le_bytes())
            .unwrap();
        writer.write_all(&recursive_proof).unwrap();
        current_accumulator
            .write(&mut writer, SerdeFormat::RawBytesUnchecked)
            .unwrap();
        writer.flush().expect("failed to flush recursive_chain_state asset");
    }

    let reloaded = load_recursive_chain_state_asset(&paths.recursive_chain_state)
        .expect("failed to reload recursive_chain_state asset after writing");
    assert_eq!(
        reloaded.state.next_merkle_root, setup.genesis_next_merkle_root,
        "reloaded recursive_chain_state next_merkle_root does not match setup"
    );
    println!(
        "generate_recursive_chain_state: done in {:?}",
        total_start.elapsed()
    );
}

/// Generates and writes the static verifier-side asset bundle.
pub(crate) fn generate_verification_context_asset(
    setup: &AssetGenerationSetup,
    paths: &AssetPaths,
) {
    println!(
        "generate_verification_context: start -> {}",
        paths.verification_context.display()
    );
    let total_start = Instant::now();
    let shared_srs_degree = RECURSIVE_CIRCUIT_DEGREE.max(CERTIFICATE_CIRCUIT_DEGREE);
    let universal_kzg_parameters = build_deterministic_params(shared_srs_degree);
    let verifier_params = universal_kzg_parameters.verifier_params();

    let certificate_commitment_parameters = {
        let mut certificate_commitment_parameters = universal_kzg_parameters.clone();
        if CERTIFICATE_CIRCUIT_DEGREE < shared_srs_degree {
            certificate_commitment_parameters.downsize(CERTIFICATE_CIRCUIT_DEGREE);
        }
        certificate_commitment_parameters
    };

    let recursive_commitment_parameters = {
        let mut recursive_commitment_parameters = universal_kzg_parameters.clone();
        if RECURSIVE_CIRCUIT_DEGREE < shared_srs_degree {
            recursive_commitment_parameters.downsize(RECURSIVE_CIRCUIT_DEGREE);
        }
        recursive_commitment_parameters
    };

    let certificate_verifying_key = zk_lib::setup_vk(
        &certificate_commitment_parameters,
        &setup.certificate_relation,
    );
    let default_ivc_circuit = IvcCircuit::unknown(certificate_verifying_key.vk());
    let recursive_verifying_key = keygen_vk_with_k(
        &recursive_commitment_parameters,
        &default_ivc_circuit,
        RECURSIVE_CIRCUIT_DEGREE,
    )
    .expect("IVC verifying key generation should not fail");
    println!("generate_verification_context: certificate and recursive verifying keys ready");

    let (certificate_fixed_bases, _) =
        fixed_bases_and_names(CERT_VK_NAME, certificate_verifying_key.vk());
    let (recursive_fixed_bases, _) = fixed_bases_and_names(IVC_ONE_NAME, &recursive_verifying_key);
    let mut combined_fixed_bases = certificate_fixed_bases;
    combined_fixed_bases.extend(recursive_fixed_bases);

    let global = Global::new(
        setup.genesis_message,
        setup.genesis_verification_key.clone(),
        certificate_verifying_key.vk(),
        &recursive_verifying_key,
    );

    if let Some(parent) = paths.verification_context.parent() {
        fs::create_dir_all(parent).expect("failed to create verification_context asset directory");
    }
    let mut writer = BufWriter::new(
        File::create(&paths.verification_context).expect("failed to create verification_context"),
    );

    for value in global.as_public_input() {
        writer.write_all(&value.to_bytes_le()).unwrap();
    }

    recursive_verifying_key
        .write(&mut writer, SerdeFormat::RawBytesUnchecked)
        .unwrap();

    writer
        .write_all(&(combined_fixed_bases.len() as u32).to_le_bytes())
        .unwrap();
    for (name, point) in &combined_fixed_bases {
        let name_bytes = name.as_bytes();
        writer.write_all(&(name_bytes.len() as u32).to_le_bytes()).unwrap();
        writer.write_all(name_bytes).unwrap();
        point.write(&mut writer, SerdeFormat::RawBytesUnchecked).unwrap();
    }
    write_shared_verifier_params(&mut writer, &verifier_params);
    println!(
        "generate_verification_context: done in {:?}",
        total_start.elapsed()
    );
}

/// Generates and writes the asset produced by one more recursive step.
pub(crate) fn generate_recursive_step_output_asset(
    setup: &AssetGenerationSetup,
    paths: &AssetPaths,
) {
    println!(
        "generate_recursive_step_output: start -> {}",
        paths.recursive_step_output.display()
    );
    let total_start = Instant::now();
    println!(
        "generate_recursive_step_output: loading recursive chain state <- {}",
        paths.recursive_chain_state.display()
    );
    let recursive_chain_state = load_recursive_chain_state_asset(&paths.recursive_chain_state)
        .expect("failed to load recursive_chain_state asset");

    let shared_srs_degree = RECURSIVE_CIRCUIT_DEGREE.max(CERTIFICATE_CIRCUIT_DEGREE);
    let universal_kzg_parameters = build_deterministic_params(shared_srs_degree);
    let universal_verifier_params = universal_kzg_parameters.verifier_params();

    let certificate_commitment_parameters = {
        let mut certificate_commitment_parameters = universal_kzg_parameters.clone();
        if CERTIFICATE_CIRCUIT_DEGREE < shared_srs_degree {
            certificate_commitment_parameters.downsize(CERTIFICATE_CIRCUIT_DEGREE);
        }
        certificate_commitment_parameters
    };

    let recursive_commitment_parameters = {
        let mut recursive_commitment_parameters = universal_kzg_parameters.clone();
        if RECURSIVE_CIRCUIT_DEGREE < shared_srs_degree {
            recursive_commitment_parameters.downsize(RECURSIVE_CIRCUIT_DEGREE);
        }
        recursive_commitment_parameters
    };

    let certificate_verifying_key = zk_lib::setup_vk(
        &certificate_commitment_parameters,
        &setup.certificate_relation,
    );
    let default_ivc_circuit = IvcCircuit::unknown(certificate_verifying_key.vk());
    let recursive_verifying_key = keygen_vk_with_k(
        &recursive_commitment_parameters,
        &default_ivc_circuit,
        RECURSIVE_CIRCUIT_DEGREE,
    )
    .expect("IVC verifying key generation should not fail");
    let recursive_proving_key = keygen_pk(recursive_verifying_key.clone(), &default_ivc_circuit)
        .expect("IVC proving key generation should not fail");
    println!("generate_recursive_step_output: certificate and recursive keys ready");

    let (recursive_fixed_bases, _) = fixed_bases_and_names(IVC_ONE_NAME, &recursive_verifying_key);
    let (certificate_fixed_bases, _) =
        fixed_bases_and_names(CERT_VK_NAME, certificate_verifying_key.vk());
    let mut combined_fixed_bases = certificate_fixed_bases;
    combined_fixed_bases.extend(recursive_fixed_bases.clone());

    let global = Global::new(
        setup.genesis_message,
        setup.genesis_verification_key.clone(),
        certificate_verifying_key.vk(),
        &recursive_verifying_key,
    );

    let mut recursive_step_output_random_generator = OsRng;
    println!("generate_recursive_step_output: building next certificate");
    let certificate_start = Instant::now();
    let (certificate_proof, certificate_accumulator, next_state, recursive_witness) =
        build_next_certificate_asset_data(
            setup,
            &certificate_commitment_parameters,
            &setup.certificate_relation,
            &certificate_verifying_key,
            &recursive_chain_state.state,
            &mut recursive_step_output_random_generator,
        );
    println!(
        "generate_recursive_step_output: next certificate done in {:?}",
        certificate_start.elapsed()
    );

    let previous_public_inputs = [
        global.as_public_input(),
        recursive_chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_chain_state.accumulator),
    ]
    .concat();
    let previous_dual_msm = verify_and_prepare_poseidon_ivc(
        &recursive_verifying_key,
        &recursive_chain_state.proof,
        &previous_public_inputs,
    );
    assert!(previous_dual_msm.clone().check(&universal_verifier_params));
    let mut previous_proof_accumulator: Accumulator<crate::circuits::halo2_ivc::S> =
        previous_dual_msm.into();
    previous_proof_accumulator.extract_fixed_bases(&recursive_fixed_bases);
    previous_proof_accumulator.collapse();

    let mut next_accumulator = Accumulator::accumulate(&[
        recursive_chain_state.accumulator.clone(),
        certificate_accumulator,
        previous_proof_accumulator,
    ]);
    next_accumulator.collapse();
    assert!(
        next_accumulator.check(
            &universal_kzg_parameters.s_g2().into(),
            &combined_fixed_bases,
        ),
        "next accumulator check failed"
    );
    println!("generate_recursive_step_output: next accumulator computed");

    let circuit = IvcCircuit::new(
        global.clone(),
        recursive_chain_state.state.clone(),
        recursive_witness,
        certificate_proof.clone(),
        recursive_chain_state.proof.clone(),
        recursive_chain_state.accumulator.clone(),
        certificate_verifying_key.vk(),
        &recursive_verifying_key,
    );
    let public_inputs = [
        global.as_public_input(),
        next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_accumulator),
    ]
    .concat();

    println!("generate_recursive_step_output: final blake2b recursive proof starting");
    let final_proof_start = Instant::now();
    let final_proof = prove_blake2b_ivc(
        &recursive_commitment_parameters,
        &recursive_proving_key,
        &circuit,
        &public_inputs,
        &mut recursive_step_output_random_generator,
    );
    let final_dual_msm =
        verify_and_prepare_blake2b_ivc(&recursive_verifying_key, &final_proof, &public_inputs);
    assert!(final_dual_msm.check(&universal_verifier_params));
    println!(
        "generate_recursive_step_output: final blake2b recursive proof done in {:?}",
        final_proof_start.elapsed()
    );

    if let Some(parent) = paths.recursive_step_output.parent() {
        fs::create_dir_all(parent).expect("failed to create recursive_step_output asset directory");
    }
    println!(
        "generate_recursive_step_output: writing asset -> {}",
        paths.recursive_step_output.display()
    );
    let mut writer = BufWriter::new(
        File::create(&paths.recursive_step_output).expect("failed to create recursive_step_output"),
    );
    writer.write_all(&(final_proof.len() as u32).to_le_bytes()).unwrap();
    writer.write_all(&final_proof).unwrap();
    next_accumulator
        .write(&mut writer, SerdeFormat::RawBytesUnchecked)
        .unwrap();
    for value in next_state.as_public_input() {
        writer.write_all(&value.to_bytes_le()).unwrap();
    }
    writer
        .write_all(&(certificate_proof.len() as u32).to_le_bytes())
        .unwrap();
    writer.write_all(&certificate_proof).unwrap();
    println!(
        "generate_recursive_step_output: done in {:?}",
        total_start.elapsed()
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    // These ignored tests are manual asset-generation entrypoints.
    #[test]
    #[ignore]
    fn generate_verification_context_only() {
        let setup = build_asset_generation_setup();
        let paths = default_asset_paths();

        generate_verification_context_asset(&setup, &paths);
    }

    #[test]
    #[ignore]
    fn generate_recursive_chain_state_only() {
        let setup = build_asset_generation_setup();
        let paths = default_asset_paths();

        generate_recursive_chain_state_asset(&setup, &paths);
    }

    #[test]
    #[ignore]
    fn generate_recursive_step_output_only() {
        let setup = build_asset_generation_setup();
        let paths = default_asset_paths();

        generate_recursive_step_output_asset(&setup, &paths);
    }
}
