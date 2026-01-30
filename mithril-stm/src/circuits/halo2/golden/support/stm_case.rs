use crate::circuits::halo2::circuit::stm::STM;
use crate::circuits::halo2::off_circuit::merkle_tree::{MTLeaf, MerklePath, MerkleTree};
use crate::circuits::halo2::off_circuit::unique_signature::{
    Signature, SigningKey, VerificationKey,
};
use crate::circuits::halo2::types::{Bls12, JubjubBase};
use crate::circuits::test_utils::setup::{generate_params, load_params};
use ff::Field;
use midnight_proofs::poly::kzg::params::ParamsKZG;
use midnight_proofs::utils::SerdeFormat;
use midnight_zk_stdlib as zk;
use midnight_zk_stdlib::{MidnightCircuit, MidnightPK, MidnightVK};
use rand_core::OsRng;
use std::fs::create_dir_all;
use std::io::Cursor;
use std::path::PathBuf;
use std::time::Instant;

type F = JubjubBase;

type WitnessEntry = (MTLeaf, MerklePath, Signature, u32);

/// Shared environment for STM golden cases (SRS, relation, keys, sizing).
pub(crate) struct STMEnv {
    srs: ParamsKZG<Bls12>,
    relation: STM,
    vk: MidnightVK,
    pk: MidnightPK<STM>,
    num_signers: usize,
    num_lotteries: u32,
}

/// Concrete STM scenario inputs for proving/verifying in golden tests.
pub(crate) struct STMScenario {
    merkle_root: F,
    msg: F,
    witness: Vec<WitnessEntry>,
}

impl STMEnv {
    /// Number of signers used to size the default Merkle tree in golden cases.
    pub(crate) fn num_signers(&self) -> usize {
        self.num_signers
    }

    /// Number of lotteries configured for the STM relation.
    pub(crate) fn num_lotteries(&self) -> u32 {
        self.num_lotteries
    }
}

impl STMScenario {
    /// Construct a new STM scenario from its instance and witness data.
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
) -> (Vec<SigningKey>, Vec<MTLeaf>, MerkleTree) {
    let mut rng = OsRng;

    let mut sks = Vec::with_capacity(n);
    let mut leaves = Vec::with_capacity(n);
    for _ in 0..n {
        let sk = SigningKey::generate(&mut rng);
        let vk = VerificationKey::from(&sk);
        leaves.push(MTLeaf(vk, -F::ONE));
        sks.push(sk);
    }
    let tree = MerkleTree::create(&leaves);

    (sks, leaves, tree)
}

/// Build a full tree with a controlled rightmost leaf and return its index.
pub(crate) fn create_merkle_tree_with_rightmost_leaf(
    depth: u32,
    target: F,
) -> (Vec<SigningKey>, Vec<MTLeaf>, MerkleTree, usize) {
    assert!(
        depth < usize::BITS,
        "depth must be < usize::BITS to safely compute 1 << depth"
    );
    let mut rng = OsRng;
    let n = 1usize << depth;
    let rightmost_index = n - 1;

    let mut sks = Vec::with_capacity(n);
    let mut leaves = Vec::with_capacity(n);

    for i in 0..n {
        let sk = SigningKey::generate(&mut rng);
        let vk = VerificationKey::from(&sk);
        let leaf_target = if i == rightmost_index { target } else { -F::ONE };
        let leaf = MTLeaf(vk, leaf_target);
        sks.push(sk);
        leaves.push(leaf);
    }

    let tree = MerkleTree::create(&leaves);

    (sks, leaves, tree, rightmost_index)
}

/// Build a full tree with a controlled leftmost leaf and return its index.
pub(crate) fn create_merkle_tree_with_leftmost_leaf(
    depth: u32,
    target: F,
) -> (Vec<SigningKey>, Vec<MTLeaf>, MerkleTree, usize) {
    assert!(
        depth < usize::BITS,
        "depth must be < usize::BITS to safely compute 1 << depth"
    );
    let mut rng = OsRng;
    let n = 1usize << depth;
    let leftmost_index = 0usize;

    let mut sks = Vec::with_capacity(n);
    let mut leaves = Vec::with_capacity(n);

    for i in 0..n {
        let sk = SigningKey::generate(&mut rng);
        let vk = VerificationKey::from(&sk);
        let leaf_target = if i == leftmost_index { target } else { -F::ONE };
        let leaf = MTLeaf(vk, leaf_target);
        sks.push(sk);
        leaves.push(leaf);
    }

    let tree = MerkleTree::create(&leaves);

    (sks, leaves, tree, leftmost_index)
}

/// Construct the STM relation environment and setup keys for a case.
pub(crate) fn setup_stm_env(case_name: &str, k: u32, quorum: u32) -> STMEnv {
    let srs = load_or_generate_params(k);

    // Keep num_signers fixed for baseline comparisons.
    let num_signers: usize = 3000;
    let depth = num_signers.next_power_of_two().trailing_zeros();
    let num_lotteries = quorum * 10;
    let relation = STM::new(quorum, num_lotteries, depth);

    {
        // Print circuit sizing information.
        let circuit = MidnightCircuit::from_relation(&relation);
        println!("\n=== STM case: {case_name} ===");
        println!("k (selected) {k}");
        println!("quorum {quorum}");
        println!("min_k {:?}", circuit.min_k());
        println!("{:?}", zk::cost_model(&relation));
    }

    let start = Instant::now();
    let vk = zk::setup_vk(&srs, &relation);
    let pk = zk::setup_pk(&relation, &vk);
    let duration = start.elapsed();
    println!("\nvk pk generation took: {:?}", duration);

    {
        let mut buffer = Cursor::new(Vec::new());
        // Serialize the MidnightVK instance to the buffer in the RawBytes format
        vk.write(&mut buffer, SerdeFormat::RawBytes).unwrap();
        // Get the size of the serialized MidnightVK
        println!("vk length {:?}", buffer.get_ref().len());
    }

    STMEnv {
        srs,
        relation,
        vk,
        pk,
        num_signers,
        num_lotteries,
    }
}

/// Build a witness with default strictly-increasing indices [0..quorum).
pub(crate) fn build_witness(
    sks: &[SigningKey],
    leaves: &[MTLeaf],
    merkle_tree: &MerkleTree,
    merkle_root: F,
    msg: F,
    quorum: u32,
) -> Vec<WitnessEntry> {
    let indices: Vec<u32> = (0..quorum).map(|i| i + 1).collect();
    build_witness_with_indices(sks, leaves, merkle_tree, merkle_root, msg, &indices)
}

/// Build a witness using caller-provided strictly increasing indices.
pub(crate) fn build_witness_with_indices(
    sks: &[SigningKey],
    leaves: &[MTLeaf],
    merkle_tree: &MerkleTree,
    merkle_root: F,
    msg: F,
    indices: &[u32],
) -> Vec<WitnessEntry> {
    assert!(!indices.is_empty(), "indices must be non-empty");
    assert!(
        indices.windows(2).all(|w| w[0] < w[1]),
        "indices must be strictly increasing"
    );
    let num_signers = sks.len();
    let mut witness = Vec::new();

    for (i, index) in indices.iter().enumerate() {
        let ii = i % num_signers;
        let usk = sks[ii].clone();
        let uvk = leaves[ii].0;
        let sig = usk.sign(&[merkle_root, msg], &mut OsRng);
        sig.verify(&[merkle_root, msg], &uvk).unwrap();

        let merkle_path = merkle_tree.get_path(ii);
        let computed_root = merkle_path.compute_root(leaves[ii]);
        assert_eq!(merkle_root, computed_root);

        // any index is eligible as target is set to be the maximum
        witness.push((leaves[ii], merkle_path, sig, *index));
    }

    witness
}

/// Reuse the same signer, Merkle path, and signature across indices to stress
/// Merkle-path shape in golden vectors; not a realistic lottery model, and any
/// grinding or signature randomness considerations are out of scope.
pub(crate) fn build_witness_with_fixed_signer(
    sks: &[SigningKey],
    leaves: &[MTLeaf],
    merkle_tree: &MerkleTree,
    signer_index: usize,
    merkle_root: F,
    msg: F,
    indices: &[u32],
) -> Vec<WitnessEntry> {
    assert!(!indices.is_empty(), "indices must be non-empty");
    assert!(
        indices.windows(2).all(|w| w[0] < w[1]),
        "indices must be strictly increasing"
    );
    assert!(signer_index < sks.len(), "signer_index out of bounds for sks");
    assert!(
        signer_index < leaves.len(),
        "signer_index out of bounds for leaves"
    );
    let mut witness = Vec::new();
    let usk = sks[signer_index].clone();
    let uvk = leaves[signer_index].0;
    let merkle_path = merkle_tree.get_path(signer_index);
    let computed_root = merkle_path.compute_root(leaves[signer_index]);
    assert_eq!(merkle_root, computed_root);

    let sig = usk.sign(&[merkle_root, msg], &mut OsRng);
    sig.verify(&[merkle_root, msg], &uvk).unwrap();

    for index in indices {
        witness.push((
            leaves[signer_index],
            merkle_path.clone(),
            sig.clone(),
            *index,
        ));
    }

    witness
}

/// Errors returned by `prove_and_verify_result`.
#[derive(Debug)]
pub(crate) enum STMProofError {
    /// Proof generation failed.
    ProveFail,
    /// Proof verification failed.
    VerifyFail,
}

/// Prove and verify a scenario, returning a typed result for negative tests.
pub(crate) fn prove_and_verify_result(
    env: &STMEnv,
    scenario: STMScenario,
) -> Result<(), STMProofError> {
    let instance = (scenario.merkle_root, scenario.msg);

    let start = Instant::now();
    let proof = zk::prove::<STM, blake2b_simd::State>(
        &env.srs,
        &env.pk,
        &env.relation,
        &instance,
        scenario.witness,
        OsRng,
    )
    .map_err(|_| STMProofError::ProveFail)?;
    let duration = start.elapsed();
    println!("\nProof generation took: {:?}", duration);
    println!("Proof size: {:?}", proof.len());

    let start = Instant::now();
    let verify_result = zk::verify::<STM, blake2b_simd::State>(
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
        Err(STMProofError::VerifyFail)
    }
}

/// Run a case using the default message (F::from(42)).
pub(crate) fn run_stm_case_default(case_name: &str, k: u32, quorum: u32) {
    run_stm_case(case_name, k, quorum, F::from(42));
}

/// Run a case with a caller-specified message.
pub(crate) fn run_stm_case(case_name: &str, k: u32, quorum: u32, msg: F) {
    let env = setup_stm_env(case_name, k, quorum);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();

    let witness = build_witness(&sks, &leaves, &merkle_tree, merkle_root, msg, quorum);
    let scenario = STMScenario::new(merkle_root, msg, witness);

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
