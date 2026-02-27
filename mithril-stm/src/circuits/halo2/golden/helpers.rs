use std::collections::HashMap;
use std::fs::create_dir_all;
use std::path::PathBuf;
use std::panic::{self, AssertUnwindSafe};
use std::sync::{Arc, LazyLock, RwLock};
use std::time::Instant;

use ff::Field;
use midnight_proofs::poly::kzg::params::ParamsKZG;
use midnight_proofs::plonk::Error as PlonkError;
use midnight_zk_stdlib as zk;
use midnight_zk_stdlib::{MidnightCircuit, MidnightPK, MidnightVK};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use thiserror::Error;

use crate::LotteryTargetValue;
use crate::circuits::halo2::circuit::StmCircuit;
use crate::circuits::halo2::errors::{
    CircuitError, ProvingError, StmProofError, StmProofResult,
};
use crate::circuits::halo2::types::{Bls12, JubjubBase, MTLeaf, MerklePath};
use crate::circuits::halo2::utils::MerklePathAdapterError;
use crate::circuits::test_utils::setup::{generate_params, load_params};
use crate::hash::poseidon::MidnightPoseidonDigest;
use crate::membership_commitment::{MerkleTree as StmMerkleTree, MerkleTreeSnarkLeaf};
use crate::signature_scheme::{
    BaseFieldElement, SchnorrSigningKey, SchnorrVerificationKey, UniqueSchnorrSignature,
};

/// Base field type used throughout STM circuit golden tests.
type F = JubjubBase;

/// Witness entry tuple used by STM circuit golden tests.
type WitnessEntry = (MTLeaf, MerklePath, UniqueSchnorrSignature, u32);

/// Default number of signers used in golden test environments.
const DEFAULT_NUM_SIGNERS: usize = 3000;
/// Lottery count multiplier per quorum size used in golden test environments.
pub(crate) const LOTTERIES_PER_QUORUM: u32 = 10;
/// Default message value used by golden test cases.
const DEFAULT_TEST_MSG: u64 = 42;

/// Verification/proving key pair cached per STM circuit configuration.
type CircuitVerificationAndProvingKeyPair = (MidnightVK, MidnightPK<StmCircuit>);
/// Cache map for verification/proving keys keyed by STM circuit configuration.
type CircuitKeysCache = HashMap<StmCircuitConfig, Arc<CircuitVerificationAndProvingKeyPair>>;

/// Errors returned by `prove_and_verify_result`.
#[derive(Debug, Error)]
pub(crate) enum StmCircuitProofError {
    /// Merkle tree depth does not fit shift constraints for fixture sizing.
    #[error("invalid merkle tree depth")]
    InvalidMerkleTreeDepth,
    /// Selected leaf index is out of bounds for tree size.
    #[error("invalid selected leaf index")]
    InvalidSelectedLeafIndex,
    /// Empty indices provided where at least one index is required.
    #[error("empty indices")]
    EmptyIndices,
    /// Witness must contain at least two entries.
    #[error("witness too short")]
    WitnessTooShort,
    /// No distinct witness entries were found.
    #[error("no distinct witness entries")]
    NoDistinctWitnessEntries,
    /// Tried to build a witness from an empty signer set.
    #[error("empty signer leaves")]
    EmptySignerLeaves,
    /// Signer leaf index is out of bounds.
    #[error("invalid signer leaf index")]
    InvalidSignerFixtureIndex,
    /// Failed to decode LotteryTargetValue from field bytes.
    #[error("invalid lottery target bytes")]
    InvalidLotteryTargetBytes,
    /// Failed to decode challenge bytes into a base field element.
    #[error("invalid challenge bytes")]
    InvalidChallengeBytes,
    /// Challenge bytes decode but do not match the native challenge field value.
    #[error("challenge endianness mismatch")]
    ChallengeEndiannessMismatch,
    /// Merkle root digest has an invalid byte length.
    #[error("invalid merkle root digest length")]
    InvalidMerkleRootDigestLength,
    /// Merkle root digest is not a canonical base field element encoding.
    #[error("non-canonical merkle root digest")]
    NonCanonicalMerkleRoot,
    /// Merkle path adaptation from STM format to Halo2 witness format failed.
    #[error("merkle path adaptation failed")]
    MerklePathAdapter(#[from] MerklePathAdapterError),
    /// STM Merkle-path verification failed for the selected leaf.
    #[error("merkle path verification failed")]
    MerklePathVerificationFailed,
    /// Failed to create the local assets directory for persisted circuit params.
    #[error("failed to create params assets directory: {0}")]
    ParamsAssetsDirCreate(#[source] std::io::Error),
    /// In-memory circuit key cache lock is poisoned.
    #[error("circuit keys cache lock poisoned ({0})")]
    CircuitKeysCacheLockPoisoned(&'static str),
    /// Signature generation failed.
    #[error("signature generation failed")]
    SignatureGeneration,
    /// Signature verification failed.
    #[error("signature verification failed")]
    SignatureVerification,
    /// Proving/verification failures shared with Halo2 error model.
    #[error(transparent)]
    Proof(#[from] StmProofError),
}

/// Result type for STM circuit golden helpers.
type StmResult<T> = Result<T, StmCircuitProofError>;

fn validate_relation_for_setup(relation: &StmCircuit) -> StmProofResult<()> {
    relation
        .validate_parameters()
        .map_err(ProvingError::from)
        .map_err(StmProofError::from)
}

/// Cache key derived from the STM circuit configuration.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct StmCircuitConfig {
    k: u32,
    quorum: u32,
    num_lotteries: u32,
    merkle_tree_depth: u32,
}

/// Shared environment for STM circuit golden cases (SRS, relation, keys, sizing).
pub(crate) struct StmCircuitEnv {
    /// Structured reference string used by the Halo2/KZG proving system.
    /// Generated or loaded once per test case.
    srs: ParamsKZG<Bls12>,

    /// The STM circuit relation defining all constraints enforced in-circuit.
    relation: StmCircuit,

    /// Verification key corresponding to `relation` and `srs`.
    vk: MidnightVK,

    /// Proving key corresponding to `relation` and `vk`.
    pk: MidnightPK<StmCircuit>,

    /// Number of signers used to size the Merkle tree in golden tests.
    num_signers: usize,

    /// Number of lotteries (`m`) configured for the STM circuit relation.
    num_lotteries: u32,
}

/// Concrete STM circuit scenario inputs for proving/verifying in golden tests.
pub(crate) struct StmCircuitScenario {
    merkle_root: F,
    msg: F,
    witness: Vec<WitnessEntry>,
}

/// Signer fixture material used to build STM Merkle trees and Halo2 witness leaves.
#[derive(Clone)]
pub(crate) struct SignerFixture {
    sk: SchnorrSigningKey,
    vk: SchnorrVerificationKey,
    target_field: F,
    target_value: LotteryTargetValue,
}

impl From<&SignerFixture> for MerkleTreeSnarkLeaf {
    fn from(value: &SignerFixture) -> Self {
        MerkleTreeSnarkLeaf(value.vk, value.target_value)
    }
}

impl From<&SignerFixture> for MTLeaf {
    fn from(value: &SignerFixture) -> Self {
        MTLeaf(value.vk, value.target_field)
    }
}

fn target_value_from_field(target: F) -> StmResult<LotteryTargetValue> {
    LotteryTargetValue::from_bytes(&target.to_bytes_le())
        .map_err(|_| StmCircuitProofError::InvalidLotteryTargetBytes)
}

fn generate_signer_fixture(rng: &mut ChaCha20Rng, target: F) -> StmResult<SignerFixture> {
    let stm_sk = SchnorrSigningKey::generate(rng);
    let stm_vk = SchnorrVerificationKey::new_from_signing_key(stm_sk.clone());
    let target_value = target_value_from_field(target)?;
    Ok(SignerFixture {
        sk: stm_sk,
        vk: stm_vk,
        target_field: target,
        target_value,
    })
}

/// STM Merkle tree wrapper that exposes Halo2-friendly root and path accessors.
/// We keep MTLeaf/Halo2-style paths because the circuit witness format is still Halo2-native.
pub(crate) struct StmMerkleTreeWrapper {
    stm_tree: StmMerkleTree<MidnightPoseidonDigest, MerkleTreeSnarkLeaf>,
    root: F,
    signer_fixtures: Vec<SignerFixture>,
}

/// Selects which leaf index is assigned the controlled target.
pub(crate) enum LeafSelector {
    /// Use the leftmost leaf (index 0).
    LeftMost,
    /// Use the rightmost leaf (index n - 1).
    RightMost,
    /// Use a specific leaf index.
    Index(usize),
}

impl StmMerkleTreeWrapper {
    /// Return the Merkle root as a JubjubBase field element.
    pub(crate) fn root(&self) -> F {
        self.root
    }

    /// Return a Halo2-style Merkle path for the given leaf index.
    pub(crate) fn get_path(&self, i: usize) -> StmResult<MerklePath> {
        let stm_path = self.stm_tree.compute_merkle_tree_path(i);
        let stm_leaf: MerkleTreeSnarkLeaf = self.signer_fixture(i)?.into();
        self.stm_tree
            .to_merkle_tree_commitment()
            .verify_leaf_membership_from_path(&stm_leaf, &stm_path)
            .map_err(|_| StmCircuitProofError::MerklePathVerificationFailed)?;
        (&stm_path).try_into().map_err(Into::into)
    }

    /// Return signer fixture material at index `i`.
    fn signer_fixture(&self, i: usize) -> StmResult<&SignerFixture> {
        self.signer_fixtures
            .get(i)
            .ok_or(StmCircuitProofError::InvalidSignerFixtureIndex)
    }
}

fn decode_merkle_root(root_bytes: &[u8]) -> StmResult<F> {
    let root_array: [u8; 32] = root_bytes
        .try_into()
        .map_err(|_| StmCircuitProofError::InvalidMerkleRootDigestLength)?;
    BaseFieldElement::from_bytes(&root_array)
        .ok()
        .map(|base| base.0)
        .ok_or(StmCircuitProofError::NonCanonicalMerkleRoot)
}

fn build_merkle_tree_wrapper(
    n: usize,
    selected_index: Option<usize>,
    target: F,
) -> StmResult<StmMerkleTreeWrapper> {
    if let Some(i) = selected_index
        && i >= n
    {
        return Err(StmCircuitProofError::InvalidSelectedLeafIndex);
    }

    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut signer_fixtures = Vec::with_capacity(n);
    for i in 0..n {
        let leaf_target = if selected_index == Some(i) {
            target
        } else {
            -F::ONE
        };
        signer_fixtures.push(generate_signer_fixture(&mut rng, leaf_target)?);
    }

    let stm_leaves: Vec<MerkleTreeSnarkLeaf> = signer_fixtures.iter().map(Into::into).collect();
    let stm_tree = StmMerkleTree::<MidnightPoseidonDigest, MerkleTreeSnarkLeaf>::new(&stm_leaves);
    let root_bytes = stm_tree.to_merkle_tree_commitment().root;
    let root = decode_merkle_root(root_bytes.as_slice())?;
    Ok(StmMerkleTreeWrapper {
        stm_tree,
        root,
        signer_fixtures,
    })
}

/// Build a default Merkle tree with all leaves set to the max target.
pub(crate) fn create_default_merkle_tree(n: usize) -> StmResult<StmMerkleTreeWrapper> {
    build_merkle_tree_wrapper(n, None, -F::ONE)
}

/// Build a full tree with one controlled leaf selected by `selector` and return its index.
pub(crate) fn create_merkle_tree_with_leaf_selector(
    depth: u32,
    selector: LeafSelector,
    target: F,
) -> StmResult<(StmMerkleTreeWrapper, usize)> {
    if depth >= usize::BITS {
        return Err(StmCircuitProofError::InvalidMerkleTreeDepth);
    }
    let n = 1usize << depth;
    let selected_index = match selector {
        LeafSelector::LeftMost => 0usize,
        LeafSelector::RightMost => n - 1,
        LeafSelector::Index(i) => {
            if i >= n {
                return Err(StmCircuitProofError::InvalidSelectedLeafIndex);
            }
            i
        }
    };

    let tree = build_merkle_tree_wrapper(n, Some(selected_index), target)?;
    Ok((tree, selected_index))
}

fn transcript_message(merkle_root: F, msg: F) -> [BaseFieldElement; 2] {
    [BaseFieldElement(merkle_root), BaseFieldElement(msg)]
}

fn assert_challenge_endianness(sig: &UniqueSchnorrSignature) -> StmResult<()> {
    let challenge_bytes = sig.challenge.to_bytes();
    let challenge_native = JubjubBase::from_bytes_le(&challenge_bytes)
        .into_option()
        .ok_or(StmCircuitProofError::InvalidChallengeBytes)?;
    if challenge_native != sig.challenge.0 {
        return Err(StmCircuitProofError::ChallengeEndiannessMismatch);
    }
    Ok(())
}

fn sign_and_verify_lottery_message(
    signer_fixture: &SignerFixture,
    merkle_root: F,
    msg: F,
    rng: &mut ChaCha20Rng,
) -> StmResult<UniqueSchnorrSignature> {
    let transcript = transcript_message(merkle_root, msg);
    let stm_sig = signer_fixture
        .sk
        .sign(&transcript, rng)
        .map_err(|_| StmCircuitProofError::SignatureGeneration)?;
    assert_challenge_endianness(&stm_sig)?;
    stm_sig
        .verify(&transcript, &signer_fixture.vk)
        .map_err(|_| StmCircuitProofError::SignatureVerification)?;
    Ok(stm_sig)
}

/// Build a witness with default strictly increasing indices [0..quorum).
pub(crate) fn build_witness(
    merkle_tree: &StmMerkleTreeWrapper,
    merkle_root: F,
    msg: F,
    quorum: u32,
) -> StmResult<Vec<WitnessEntry>> {
    let indices: Vec<u32> = (0..quorum).collect();
    build_witness_with_indices(merkle_tree, merkle_root, msg, &indices)
}

/// Build a witness using caller-provided indices without enforcing ordering;
/// the circuit is responsible for strict ordering checks in negative tests.
pub(crate) fn build_witness_with_indices(
    merkle_tree: &StmMerkleTreeWrapper,
    merkle_root: F,
    msg: F,
    indices: &[u32],
) -> StmResult<Vec<WitnessEntry>> {
    build_witness_internal(
        merkle_tree,
        merkle_root,
        msg,
        WitnessBuildMode::Indices(indices),
    )
}

/// Reuse the same signer, Merkle path, and signature across indices to stress
/// Merkle-path shape in golden vectors; not a realistic lottery model, and any
/// grinding or signature randomness considerations are out of scope.
pub(crate) fn build_witness_with_fixed_signer(
    merkle_tree: &StmMerkleTreeWrapper,
    signer_index: usize,
    merkle_root: F,
    msg: F,
    indices: &[u32],
) -> StmResult<Vec<WitnessEntry>> {
    build_witness_internal(
        merkle_tree,
        merkle_root,
        msg,
        WitnessBuildMode::FixedSigner {
            signer_index,
            indices,
        },
    )
}

enum WitnessBuildMode<'a> {
    Indices(&'a [u32]),
    FixedSigner {
        signer_index: usize,
        indices: &'a [u32],
    },
}

fn build_witness_internal(
    merkle_tree: &StmMerkleTreeWrapper,
    merkle_root: F,
    msg: F,
    mode: WitnessBuildMode<'_>,
) -> StmResult<Vec<WitnessEntry>> {
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut witness = Vec::new();

    match mode {
        WitnessBuildMode::Indices(indices) => {
            if indices.is_empty() {
                return Err(StmCircuitProofError::EmptyIndices);
            }
            let num_signers = merkle_tree.signer_fixtures.len();
            if num_signers == 0 {
                return Err(StmCircuitProofError::EmptySignerLeaves);
            }

            for (i, index) in indices.iter().enumerate() {
                let signer_index = i % num_signers;
                let signer_fixture = merkle_tree.signer_fixture(signer_index)?;
                let stm_sig =
                    sign_and_verify_lottery_message(signer_fixture, merkle_root, msg, &mut rng)?;
                let merkle_path = merkle_tree.get_path(signer_index)?;
                witness.push((signer_fixture.into(), merkle_path, stm_sig, *index));
            }
        }
        WitnessBuildMode::FixedSigner {
            signer_index,
            indices,
        } => {
            if indices.is_empty() {
                return Err(StmCircuitProofError::EmptyIndices);
            }
            let signer_fixture = merkle_tree.signer_fixture(signer_index)?;
            let merkle_path = merkle_tree.get_path(signer_index)?;
            let stm_sig =
                sign_and_verify_lottery_message(signer_fixture, merkle_root, msg, &mut rng)?;
            for index in indices {
                witness.push((signer_fixture.into(), merkle_path.clone(), stm_sig, *index));
            }
        }
    }

    Ok(witness)
}

/// Return the first two witness entries if their leaves are distinct.
///
/// This helper assumes callers construct witnesses so entries `0` and `1` come
/// from different signer fixtures (negative-test precondition). If not, it
/// returns an error instead of searching later entries.
pub(crate) fn find_two_distinct_witness_entries(
    witness: &[WitnessEntry],
) -> StmResult<(usize, usize)> {
    if witness.len() < 2 {
        return Err(StmCircuitProofError::WitnessTooShort);
    }
    let leaf0 = witness[0].0;
    let leaf1 = witness[1].0;
    if leaf0 != leaf1 {
        return Ok((0, 1));
    }
    Err(StmCircuitProofError::NoDistinctWitnessEntries)
}

impl StmCircuitEnv {
    /// Number of signers used to size the default Merkle tree in golden cases.
    pub(crate) fn num_signers(&self) -> usize {
        self.num_signers
    }

    /// Number of lotteries configured for the STM circuit relation.
    pub(crate) fn num_lotteries(&self) -> u32 {
        self.num_lotteries
    }
}

impl StmCircuitScenario {
    /// Construct a new STM circuit scenario from its instance and witness data.
    pub(crate) fn new(merkle_root: F, msg: F, witness: Vec<WitnessEntry>) -> Self {
        Self {
            merkle_root,
            msg,
            witness,
        }
    }
}

/// Construct the STM circuit relation environment and setup keys for a case.
pub(crate) fn setup_stm_circuit_env(
    case_name: &str,
    k: u32,
    quorum: u32,
    num_lotteries: u32,
) -> StmResult<StmCircuitEnv> {
    let srs = load_or_generate_params(k)?;

    let num_signers: usize = DEFAULT_NUM_SIGNERS;
    let depth = num_signers.next_power_of_two().trailing_zeros();
    let relation = StmCircuit::new(quorum, num_lotteries, depth);
    validate_relation_for_setup(&relation)?;

    {
        let stm_circuit = MidnightCircuit::from_relation(&relation);
        println!("\n=== STM circuit case: {case_name} ===");
        println!("k (selected) {k}");
        println!("quorum {quorum}");
        println!("min_k {:?}", stm_circuit.min_k());
        println!("{:?}", zk::cost_model(&relation));
    }

    let config = StmCircuitConfig {
        k,
        quorum,
        num_lotteries,
        merkle_tree_depth: depth,
    };
    let key_pair = get_or_build_circuit_keys(config, &relation, &srs)?;
    let (vk, pk) = (&key_pair.0, &key_pair.1);

    Ok(StmCircuitEnv {
        srs,
        relation,
        vk: vk.clone(),
        pk: pk.clone(),
        num_signers,
        num_lotteries,
    })
}

/// Prove and verify a scenario, returning a typed result for negative tests.
pub(crate) fn prove_and_verify_result(
    env: &StmCircuitEnv,
    scenario: StmCircuitScenario,
) -> StmResult<()> {
    let instance = (scenario.merkle_root, scenario.msg);

    let start = Instant::now();
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let proof = zk::prove::<StmCircuit, blake2b_simd::State>(
        &env.srs,
        &env.pk,
        &env.relation,
        &instance,
        scenario.witness,
        &mut rng,
    )
    .map_err(map_proving_backend_error)?;
    let duration = start.elapsed();
    println!("\nProof generation took: {:?}", duration);
    println!("Proof size: {:?}", proof.len());

    let start = Instant::now();
    let verify_result = zk::verify::<StmCircuit, blake2b_simd::State>(
        &env.srs.verifier_params(),
        &env.vk,
        &instance,
        None,
        &proof,
    );
    let duration = start.elapsed();
    println!("Proof verification took: {:?}", duration);

    if verify_result.is_ok() {
        Ok(())
    } else {
        Err(StmProofError::VerificationFailed.into())
    }
}

fn map_proving_backend_error(error: PlonkError) -> StmCircuitProofError {
    // Midnight collapses circuit-side validation failures into `Error::Synthesis(String)`.
    // Rebuild the typed circuit error when the synthesis payload matches one of our guards.
    let circuit_error = match error {
        PlonkError::Synthesis(message) => CircuitError::from_synthesis_message(&message)
            .unwrap_or(CircuitError::CircuitExecutionFailed),
        _ => CircuitError::CircuitExecutionFailed,
    };

    StmProofError::ProvingFailed(ProvingError::Circuit(circuit_error)).into()
}

/// Run a case using the default message (F::from(DEFAULT_TEST_MSG)).
pub(crate) fn run_stm_circuit_case_default(case_name: &str, k: u32, quorum: u32) -> StmResult<()> {
    run_stm_circuit_case(case_name, k, quorum, F::from(DEFAULT_TEST_MSG))
}

/// Run a case with a caller-specified message.
pub(crate) fn run_stm_circuit_case(case_name: &str, k: u32, quorum: u32, msg: F) -> StmResult<()> {
    let num_lotteries = quorum * LOTTERIES_PER_QUORUM;
    let env = setup_stm_circuit_env(case_name, k, quorum, num_lotteries)?;
    let merkle_tree = create_default_merkle_tree(env.num_signers())?;

    let merkle_root = merkle_tree.root();

    let witness = build_witness(&merkle_tree, merkle_root, msg, quorum)?;
    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);

    prove_and_verify_result(&env, scenario)
}

// Load cached KZG params if present; otherwise generate and persist them for reuse.
fn load_or_generate_params(k: u32) -> StmResult<ParamsKZG<Bls12>> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let assets_dir = manifest_dir.join("src").join("circuits").join("halo2").join("assets");
    let path = assets_dir.join(format!("params_kzg_unsafe_{}", k));

    if path.exists() {
        return Ok(load_params(path.to_string_lossy().as_ref()));
    }

    create_dir_all(&assets_dir).map_err(StmCircuitProofError::ParamsAssetsDirCreate)?;
    Ok(generate_params(k, path.to_string_lossy().as_ref()))
}

/// Get cached verification/proving keys or build and insert them if missing.
fn get_or_build_circuit_keys(
    config: StmCircuitConfig,
    relation: &StmCircuit,
    srs: &ParamsKZG<Bls12>,
) -> StmResult<Arc<CircuitVerificationAndProvingKeyPair>> {
    static STM_CIRCUIT_KEYS_CACHE: LazyLock<RwLock<CircuitKeysCache>> =
        LazyLock::new(|| RwLock::new(HashMap::new()));
    if let Some(key_pair) = STM_CIRCUIT_KEYS_CACHE
        .read()
        .map_err(|_| StmCircuitProofError::CircuitKeysCacheLockPoisoned("read"))?
        .get(&config)
        .cloned()
    {
        return Ok(key_pair);
    }

    let start = Instant::now();
    let (vk, pk) = panic::catch_unwind(AssertUnwindSafe(|| {
        let vk = zk::setup_vk(srs, relation);
        let pk = zk::setup_pk(relation, &vk);
        (vk, pk)
    }))
    .map_err(|_| StmProofError::ProvingFailed(ProvingError::MidnightSetupFailed))?;
    let duration = start.elapsed();
    println!("\nvk pk generation took: {:?}", duration);

    let key_pair = Arc::new((vk, pk));
    STM_CIRCUIT_KEYS_CACHE
        .write()
        .map_err(|_| StmCircuitProofError::CircuitKeysCacheLockPoisoned("write"))?
        .insert(config, key_pair.clone());
    Ok(key_pair)
}
