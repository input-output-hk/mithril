use std::{collections::BTreeMap, path::PathBuf};

use ff::Field;
use midnight_curves::Bls12;
use midnight_proofs::{
    plonk::{ProvingKey, VerifyingKey, keygen_pk, keygen_vk_with_k},
    poly::kzg::{
        KZGCommitmentScheme,
        params::{ParamsKZG, ParamsVerifierKZG},
    },
};
use midnight_zk_stdlib as zk_lib;
use midnight_zk_stdlib::MidnightVK;
use rand_chacha::ChaCha20Rng;
use rand_core::{CryptoRng, RngCore, SeedableRng};

use crate::circuits::halo2_ivc::helpers::{
    merkle_tree::{MTLeaf as MerkleTreeLeaf, MerkleTree},
    protocol_message::AggregateVerificationKey,
    signatures::{
        schnorr_signature::{
            Signature as SchnorrSignature, SigningKey as SchnorrSigningKey,
            VerificationKey as SchnorrVerificationKey,
        },
        unique_signature::{SigningKey, VerificationKey},
    },
    utils::jubjub_base_from_le_bytes,
};
use crate::circuits::halo2_ivc::state::fixed_bases_and_names;
use crate::circuits::halo2_ivc::tests::test_certificate::Certificate;
use crate::circuits::halo2_ivc::{
    C, CERT_VK_NAME, E, F, IVC_ONE_NAME, circuit::IvcCircuit, state::Global,
};

use super::super::{ASSET_SEED, CERTIFICATE_CIRCUIT_DEGREE, RECURSIVE_CIRCUIT_DEGREE};
use super::transitions::build_genesis_protocol_message;

pub(crate) const INITIAL_CHAIN_LENGTH: usize = 3;
pub(crate) const GENESIS_EPOCH: u64 = 5;
pub(super) const QUORUM_SIZE: u32 = 2;
const SIGNER_COUNT: usize = 3000;

/// Paths for the minimal stored asset set used by asset-based golden tests.
#[derive(Debug, Clone)]
pub(crate) struct AssetPaths {
    /// Path to the stored recursive chain checkpoint asset.
    pub(crate) recursive_chain_state: PathBuf,
    /// Path to the stored verification-context asset.
    pub(crate) verification_context: PathBuf,
    /// Path to the stored one-step recursive output asset.
    pub(crate) recursive_step_output: PathBuf,
}

impl AssetPaths {
    /// Builds the three committed asset paths rooted at `base_dir`.
    pub(crate) fn new(base_dir: PathBuf) -> Self {
        Self {
            recursive_chain_state: base_dir.join("recursive_chain_state.bin"),
            verification_context: base_dir.join("verification_context.bin"),
            recursive_step_output: base_dir.join("recursive_step_output.bin"),
        }
    }
}

impl Default for AssetPaths {
    fn default() -> Self {
        Self::new(
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/circuits/halo2_ivc/tests/assets"),
        )
    }
}

/// Deterministic setup data for asset generation.
///
/// This carries the shared `halo2_ivc` context needed to reproduce the same
/// stored asset contents across runs.
#[derive(Debug)]
pub(crate) struct AssetGenerationSetup {
    /// Deterministic certificate relation used by the golden generators.
    pub(crate) certificate_relation: Certificate,
    /// Verification key for the trusted genesis signature.
    pub(crate) genesis_verification_key: SchnorrVerificationKey,
    /// Hash of the deterministic genesis protocol message.
    pub(crate) genesis_message: F,
    /// Deterministic trusted genesis signature.
    pub(crate) genesis_signature: SchnorrSignature,
    /// Deterministic signer-membership Merkle tree.
    pub(crate) merkle_tree: MerkleTree,
    /// Leaves committed into the deterministic signer-membership tree.
    pub(crate) merkle_tree_leaves: Vec<MerkleTreeLeaf>,
    /// Deterministic signing keys used to build certificate witnesses.
    pub(crate) signing_keys: Vec<SigningKey>,
    /// Aggregate verification key committed into the generated protocol messages.
    pub(crate) aggregate_verification_key: AggregateVerificationKey,
    /// Deterministic next Merkle root committed by the genesis message.
    pub(crate) genesis_next_merkle_root: F,
    /// Deterministic next protocol parameters committed by the genesis message.
    pub(crate) genesis_next_protocol_params: F,
}

/// Shared recursive verifier-side setup reused by generators and golden helpers.
pub(crate) struct SharedRecursiveContext {
    /// Shared universal KZG parameters built at the maximum circuit degree.
    pub(crate) universal_kzg_parameters: ParamsKZG<Bls12>,
    /// Verifier-side view of the shared universal KZG parameters.
    pub(crate) universal_verifier_params: ParamsVerifierKZG<E>,
    /// Certificate-sized commitment parameters derived from the shared SRS.
    pub(crate) certificate_commitment_parameters: ParamsKZG<Bls12>,
    /// Recursive-circuit-sized commitment parameters derived from the shared SRS.
    pub(crate) recursive_commitment_parameters: ParamsKZG<Bls12>,
    /// Verifying key for the certificate relation.
    pub(crate) certificate_verifying_key: MidnightVK,
    /// Verifying key for the recursive IVC circuit.
    pub(crate) recursive_verifying_key: VerifyingKey<F, KZGCommitmentScheme<E>>,
}

/// Builds the deterministic signer keys, leaves, and commitment tree used by the assets.
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
pub(crate) fn build_deterministic_params(circuit_degree: u32) -> ParamsKZG<Bls12> {
    ParamsKZG::<Bls12>::unsafe_setup(circuit_degree, ChaCha20Rng::seed_from_u64(ASSET_SEED))
}

/// Derives circuit-specific commitment parameters from a shared universal SRS.
pub(crate) fn derive_commitment_params(
    universal_kzg_parameters: &ParamsKZG<Bls12>,
    shared_srs_degree: u32,
    circuit_degree: u32,
) -> ParamsKZG<Bls12> {
    let mut commitment_parameters = universal_kzg_parameters.clone();
    if circuit_degree < shared_srs_degree {
        commitment_parameters.downsize(circuit_degree);
    }
    commitment_parameters
}

/// Builds the shared verifier-side recursive setup from the deterministic SRS.
pub(crate) fn build_shared_recursive_context(
    setup: &AssetGenerationSetup,
) -> SharedRecursiveContext {
    let shared_srs_degree = RECURSIVE_CIRCUIT_DEGREE.max(CERTIFICATE_CIRCUIT_DEGREE);
    let universal_kzg_parameters = build_deterministic_params(shared_srs_degree);
    let universal_verifier_params = universal_kzg_parameters.verifier_params();

    let certificate_commitment_parameters = derive_commitment_params(
        &universal_kzg_parameters,
        shared_srs_degree,
        CERTIFICATE_CIRCUIT_DEGREE,
    );
    let recursive_commitment_parameters = derive_commitment_params(
        &universal_kzg_parameters,
        shared_srs_degree,
        RECURSIVE_CIRCUIT_DEGREE,
    );

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
    .expect("recursive verifying key generation should not fail");

    SharedRecursiveContext {
        universal_kzg_parameters,
        universal_verifier_params,
        certificate_commitment_parameters,
        recursive_commitment_parameters,
        certificate_verifying_key,
        recursive_verifying_key,
    }
}

/// Builds the recursive proving key for the default IVC circuit shape.
pub(crate) fn build_recursive_proving_key(
    context: &SharedRecursiveContext,
) -> ProvingKey<F, KZGCommitmentScheme<E>> {
    let default_ivc_circuit = IvcCircuit::unknown(context.certificate_verifying_key.vk());
    keygen_pk(
        context.recursive_verifying_key.clone(),
        &default_ivc_circuit,
    )
    .expect("recursive proving key generation should not fail")
}

/// Returns the certificate, recursive, and combined fixed-base maps.
pub(crate) fn build_recursive_fixed_bases(
    certificate_verifying_key: &MidnightVK,
    recursive_verifying_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
) -> (
    BTreeMap<String, C>,
    BTreeMap<String, C>,
    BTreeMap<String, C>,
) {
    let (certificate_fixed_bases, _) =
        fixed_bases_and_names(CERT_VK_NAME, certificate_verifying_key.vk());
    let (recursive_fixed_bases, _) = fixed_bases_and_names(IVC_ONE_NAME, recursive_verifying_key);
    let mut combined_fixed_bases = certificate_fixed_bases.clone();
    combined_fixed_bases.extend(recursive_fixed_bases.clone());

    (
        certificate_fixed_bases,
        recursive_fixed_bases,
        combined_fixed_bases,
    )
}

/// Builds the shared recursive global inputs from the deterministic setup.
pub(crate) fn build_recursive_global(
    setup: &AssetGenerationSetup,
    certificate_verifying_key: &MidnightVK,
    recursive_verifying_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
) -> Global {
    Global::new(
        setup.genesis_message,
        setup.genesis_verification_key,
        certificate_verifying_key.vk(),
        recursive_verifying_key,
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
    let genesis_epoch = GENESIS_EPOCH;
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
