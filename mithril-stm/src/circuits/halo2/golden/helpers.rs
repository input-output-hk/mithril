use std::collections::HashMap;
use std::fs::create_dir_all;
use std::io::Cursor;
use std::path::PathBuf;
use std::sync::{Arc, LazyLock, RwLock};
use std::time::Instant;

use ff::Field;
use midnight_proofs::poly::kzg::params::ParamsKZG;
use midnight_proofs::utils::SerdeFormat;
use midnight_zk_stdlib as zk;
use midnight_zk_stdlib::{MidnightCircuit, MidnightPK, MidnightVK};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use thiserror::Error;

use crate::circuits::halo2::circuit::StmCircuit;
use crate::circuits::halo2::off_circuit::merkle_tree::{MTLeaf, MerklePath, MerkleTree};
use crate::circuits::halo2::types::{Bls12, JubjubBase};
use crate::circuits::test_utils::setup::{generate_params, load_params};
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
const LOTTERIES_PER_QUORUM: u32 = 10;
/// Default message value used by golden test cases.
const DEFAULT_TEST_MSG: u64 = 42;

/// Verification/proving key pair cached per STM circuit configuration.
type CircuitVerificationAndProvingKeyPair = (MidnightVK, MidnightPK<StmCircuit>);
/// Cache map for verification/proving keys keyed by STM circuit configuration.
type CircuitKeysCache = HashMap<StmCircuitConfig, Arc<CircuitVerificationAndProvingKeyPair>>;

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

/// Selects which leaf index is assigned the controlled target.
pub(crate) enum LeafSelector {
    /// Use the leftmost leaf (index 0).
    LeftMost,
    /// Use the rightmost leaf (index n - 1).
    RightMost,
    /// Use a specific leaf index.
    Index(usize),
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

/// Build a default Merkle tree with all leaves set to the max target.
pub(crate) fn create_default_merkle_tree(
    n: usize,
) -> (Vec<SchnorrSigningKey>, Vec<MTLeaf>, MerkleTree) {
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

    let mut sks = Vec::with_capacity(n);
    let mut leaves = Vec::with_capacity(n);
    for _ in 0..n {
        // Retry until STM VK derivation succeeds (edge-case scalars); test-only guard
        // to avoid deterministic-seed flakes in golden vectors.
        let stm_sk = loop {
            let stm_sk = SchnorrSigningKey::generate(&mut rng)
                .expect("Failed to generate STM signing key");
            if SchnorrVerificationKey::new_from_signing_key(stm_sk.clone()).is_ok() {
                break stm_sk;
            }
        };
        let stm_vk = SchnorrVerificationKey::new_from_signing_key(stm_sk.clone())
            .expect("Failed to build STM verification key from signing key");
        leaves.push(MTLeaf(stm_vk.to_bytes(), -F::ONE));
        sks.push(stm_sk);
    }
    let tree = MerkleTree::create(&leaves);

    (sks, leaves, tree)
}

/// Build a full tree with one controlled leaf selected by `selector` and return its index.
pub(crate) fn create_merkle_tree_with_leaf_selector(
    depth: u32,
    selector: LeafSelector,
    target: F,
) -> (Vec<SchnorrSigningKey>, Vec<MTLeaf>, MerkleTree, usize) {
    assert!(
        depth < usize::BITS,
        "depth must be < usize::BITS to safely compute 1 << depth"
    );
    let n = 1usize << depth;
    let selected_index = match selector {
        LeafSelector::LeftMost => 0usize,
        LeafSelector::RightMost => n - 1,
        LeafSelector::Index(i) => {
            assert!(i < n, "leaf_index out of bounds for tree size");
            i
        }
    };

    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut sks = Vec::with_capacity(n);
    let mut leaves = Vec::with_capacity(n);

    for i in 0..n {
        // Retry until STM VK derivation succeeds (edge-case scalars); test-only guard
        // to avoid deterministic-seed flakes in golden vectors.
        let stm_sk = loop {
            let stm_sk = SchnorrSigningKey::generate(&mut rng)
                .expect("Failed to generate STM signing key");
            if SchnorrVerificationKey::new_from_signing_key(stm_sk.clone()).is_ok() {
                break stm_sk;
            }
        };
        let stm_vk = SchnorrVerificationKey::new_from_signing_key(stm_sk.clone())
            .expect("Failed to build STM verification key from signing key");
        let leaf_target = if i == selected_index { target } else { -F::ONE };
        let leaf = MTLeaf(stm_vk.to_bytes(), leaf_target);
        sks.push(stm_sk);
        leaves.push(leaf);
    }

    let tree = MerkleTree::create(&leaves);

    (sks, leaves, tree, selected_index)
}

/// Construct the STM circuit relation environment and setup keys for a case.
pub(crate) fn setup_stm_circuit_env(case_name: &str, k: u32, quorum: u32) -> StmCircuitEnv {
    let srs = load_or_generate_params(k);

    let num_signers: usize = DEFAULT_NUM_SIGNERS;
    let depth = num_signers.next_power_of_two().trailing_zeros();
    let num_lotteries = quorum * LOTTERIES_PER_QUORUM;
    let relation = StmCircuit::new(quorum, num_lotteries, depth);

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
    let key_pair = get_or_build_circuit_keys(config, &relation, &srs);
    let (vk, pk) = (&key_pair.0, &key_pair.1);

    {
        let mut buffer = Cursor::new(Vec::new());
        vk.write(&mut buffer, SerdeFormat::RawBytes).unwrap();
        println!("vk length {:?}", buffer.get_ref().len());
    }

    StmCircuitEnv {
        srs,
        relation,
        vk: vk.clone(),
        pk: pk.clone(),
        num_signers,
        num_lotteries,
    }
}

/// Build a witness with default strictly increasing indices [0..quorum).
pub(crate) fn build_witness(
    sks: &[SchnorrSigningKey],
    leaves: &[MTLeaf],
    merkle_tree: &MerkleTree,
    merkle_root: F,
    msg: F,
    quorum: u32,
) -> Vec<WitnessEntry> {
    let indices: Vec<u32> = (0..quorum).collect();
    build_witness_with_indices(sks, leaves, merkle_tree, merkle_root, msg, &indices)
}

/// Build a witness using caller-provided indices without enforcing ordering;
/// the circuit is responsible for strict ordering checks in negative tests.
pub(crate) fn build_witness_with_indices(
    sks: &[SchnorrSigningKey],
    leaves: &[MTLeaf],
    merkle_tree: &MerkleTree,
    merkle_root: F,
    msg: F,
    indices: &[u32],
) -> Vec<WitnessEntry> {
    assert!(!indices.is_empty(), "indices must be non-empty");
    let num_signers = sks.len();
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut witness = Vec::new();

    for (i, index) in indices.iter().enumerate() {
        let ii = i % num_signers;
        let stm_sk = sks[ii].clone();
        let stm_vk = SchnorrVerificationKey::from_bytes(&leaves[ii].0)
            .expect("Failed to parse STM verification key bytes from leaf");
        let stm_sig = stm_sk
            .sign(
                &[BaseFieldElement(merkle_root), BaseFieldElement(msg)],
                &mut rng,
            )
            .expect("STM signature generation failed");
        let challenge_bytes = stm_sig.challenge.to_bytes();
        let challenge_native = JubjubBase::from_bytes_le(&challenge_bytes)
            .into_option()
            .expect("Invalid challenge bytes");
        assert_eq!(
            challenge_native, stm_sig.challenge.0,
            "Challenge endianness mismatch between BaseFieldElement bytes and JubjubBase"
        );
        stm_sig
            .verify(
                &[BaseFieldElement(merkle_root), BaseFieldElement(msg)],
                &stm_vk,
            )
            .expect("STM signature verification failed");

        let merkle_path = merkle_tree.get_path(ii);
        let computed_root = merkle_path.compute_root(leaves[ii]);
        assert_eq!(merkle_root, computed_root);

        // any index is eligible as target is set to be the maximum
        witness.push((leaves[ii], merkle_path, stm_sig, *index));
    }

    witness
}

/// Find two witness entries with distinct leaves, for negative test construction.
pub(crate) fn find_two_distinct_witness_entries(witness: &[WitnessEntry]) -> (usize, usize) {
    assert!(witness.len() >= 2, "expected at least two witness entries");
    if witness[0].0.to_bytes() != witness[1].0.to_bytes() {
        return (0, 1);
    }
    for i in 0..witness.len() {
        let leaf_i = witness[i].0.to_bytes();
        for (j, wj) in witness.iter().enumerate().skip(i + 1) {
            if leaf_i != wj.0.to_bytes() {
                return (i, j);
            }
        }
    }
    panic!("expected at least two distinct leaves in witness");
}

/// Reuse the same signer, Merkle path, and signature across indices to stress
/// Merkle-path shape in golden vectors; not a realistic lottery model, and any
/// grinding or signature randomness considerations are out of scope.
pub(crate) fn build_witness_with_fixed_signer(
    sks: &[SchnorrSigningKey],
    leaves: &[MTLeaf],
    merkle_tree: &MerkleTree,
    signer_index: usize,
    merkle_root: F,
    msg: F,
    indices: &[u32],
) -> Vec<WitnessEntry> {
    assert!(!indices.is_empty(), "indices must be non-empty");
    assert!(
        signer_index < sks.len(),
        "signer_index out of bounds for sks"
    );
    assert!(
        signer_index < leaves.len(),
        "signer_index out of bounds for leaves"
    );
    let mut witness = Vec::new();
    let stm_sk = sks[signer_index].clone();
    let stm_vk = SchnorrVerificationKey::from_bytes(&leaves[signer_index].0)
        .expect("Failed to parse STM verification key bytes from leaf");
    let merkle_path = merkle_tree.get_path(signer_index);
    let computed_root = merkle_path.compute_root(leaves[signer_index]);
    assert_eq!(merkle_root, computed_root);

    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let stm_sig = stm_sk
        .sign(
            &[BaseFieldElement(merkle_root), BaseFieldElement(msg)],
            &mut rng,
        )
        .expect("STM signature generation failed");
    let challenge_bytes = stm_sig.challenge.to_bytes();
    let challenge_native = JubjubBase::from_bytes_le(&challenge_bytes)
        .into_option()
        .expect("Invalid challenge bytes");
    assert_eq!(
        challenge_native, stm_sig.challenge.0,
        "Challenge endianness mismatch between BaseFieldElement bytes and JubjubBase"
    );
    stm_sig
        .verify(
            &[BaseFieldElement(merkle_root), BaseFieldElement(msg)],
            &stm_vk,
        )
        .expect("STM signature verification failed");

    for index in indices {
        witness.push((
            leaves[signer_index],
            merkle_path.clone(),
            stm_sig.clone(),
            *index,
        ));
    }

    witness
}

/// Errors returned by `prove_and_verify_result`.
#[derive(Debug, Error)]
pub(crate) enum StmCircuitProofError {
    /// Proof generation failed.
    #[error("proof generation failed")]
    ProveFail,
    /// Proof verification failed.
    #[error("proof verification failed")]
    VerifyFail,
}

/// Result type for STM circuit golden helpers.
type StmResult<T> = Result<T, StmCircuitProofError>;

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
    .map_err(|_| StmCircuitProofError::ProveFail)?;
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
        Err(StmCircuitProofError::VerifyFail)
    }
}

/// Run a case using the default message (F::from(DEFAULT_TEST_MSG)).
pub(crate) fn run_stm_circuit_case_default(case_name: &str, k: u32, quorum: u32) {
    run_stm_circuit_case(case_name, k, quorum, F::from(DEFAULT_TEST_MSG));
}

/// Run a case with a caller-specified message.
pub(crate) fn run_stm_circuit_case(case_name: &str, k: u32, quorum: u32, msg: F) {
    let env = setup_stm_circuit_env(case_name, k, quorum);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();

    let witness = build_witness(&sks, &leaves, &merkle_tree, merkle_root, msg, quorum);
    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);

    prove_and_verify_result(&env, scenario).expect("Proof generation/verification failed");
}

// Load cached KZG params if present; otherwise generate and persist them for reuse.
fn load_or_generate_params(k: u32) -> ParamsKZG<Bls12> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let assets_dir = manifest_dir.join("src").join("circuits").join("halo2").join("assets");
    let path = assets_dir.join(format!("params_kzg_unsafe_{}", k));

    if path.exists() {
        return load_params(path.to_string_lossy().as_ref());
    }

    create_dir_all(&assets_dir).unwrap();
    generate_params(k, path.to_string_lossy().as_ref())
}

/// Get cached verification/proving keys or build and insert them if missing.
fn get_or_build_circuit_keys(
    config: StmCircuitConfig,
    relation: &StmCircuit,
    srs: &ParamsKZG<Bls12>,
) -> Arc<CircuitVerificationAndProvingKeyPair> {
    static STM_CIRCUIT_KEYS_CACHE: LazyLock<RwLock<CircuitKeysCache>> =
        LazyLock::new(|| RwLock::new(HashMap::new()));
    if let Some(key_pair) = STM_CIRCUIT_KEYS_CACHE.read().unwrap().get(&config).cloned() {
        return key_pair;
    }

    let start = Instant::now();
    let vk = zk::setup_vk(srs, relation);
    let pk = zk::setup_pk(relation, &vk);
    let duration = start.elapsed();
    println!("\nvk pk generation took: {:?}", duration);

    let key_pair = Arc::new((vk, pk));
    STM_CIRCUIT_KEYS_CACHE
        .write()
        .unwrap()
        .insert(config, key_pair.clone());
    key_pair
}
