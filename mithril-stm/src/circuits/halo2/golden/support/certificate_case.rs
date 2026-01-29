use crate::circuits::halo2::circuit::certificate::Certificate;
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

pub(crate) struct CertificateEnv {
    srs: ParamsKZG<Bls12>,
    relation: Certificate,
    vk: MidnightVK,
    pk: MidnightPK<Certificate>,
    num_signers: usize,
}

pub(crate) struct CertificateScenario {
    merkle_root: F,
    msg: F,
    witness: Vec<WitnessEntry>,
}

fn create_default_merkle_tree(n: usize) -> (Vec<SigningKey>, Vec<MTLeaf>, MerkleTree) {
    let mut rng = OsRng;

    let mut sks = Vec::new();
    let mut leaves = Vec::new();
    for _ in 0..n {
        let sk = SigningKey::generate(&mut rng);
        let vk = VerificationKey::from(&sk);
        leaves.push(MTLeaf(vk, -F::ONE));
        sks.push(sk);
    }
    let tree = MerkleTree::create(&leaves);

    (sks, leaves, tree)
}

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

fn setup_certificate_env(case_name: &str, k: u32, quorum: u32) -> CertificateEnv {
    // let srs = filecoin_srs(k);
    let srs = load_or_generate_params(k);

    // Keep num_signers fixed for baseline comparisons.
    let num_signers: usize = 3000;
    let depth = num_signers.next_power_of_two().trailing_zeros();
    let num_lotteries = quorum * 10;
    let relation = Certificate::new(quorum, num_lotteries, depth);

    {
        // Print circuit sizing information.
        let circuit = MidnightCircuit::from_relation(&relation);
        println!("\n=== Certificate case: {case_name} ===");
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

    CertificateEnv {
        srs,
        relation,
        vk,
        pk,
        num_signers,
    }
}

fn build_witness(
    sks: &[SigningKey],
    leaves: &[MTLeaf],
    merkle_tree: &MerkleTree,
    merkle_root: F,
    msg: F,
    quorum: u32,
) -> Vec<WitnessEntry> {
    let num_signers = sks.len();
    let mut witness = Vec::new();

    for i in 0..quorum as usize {
        let ii = i % num_signers;
        let usk = sks[ii].clone();
        let uvk = leaves[ii].0;
        let sig = usk.sign(&[merkle_root, msg], &mut OsRng);
        sig.verify(&[merkle_root, msg], &uvk).unwrap();

        let merkle_path = merkle_tree.get_path(ii);
        let computed_root = merkle_path.compute_root(leaves[ii]);
        assert_eq!(merkle_root, computed_root);

        // any index is eligible as target is set to be the maximum
        witness.push((leaves[ii], merkle_path, sig, (i + 1) as u32));
    }

    witness
}

fn prove_and_verify(env: &CertificateEnv, scenario: CertificateScenario) {
    let instance = (scenario.merkle_root, scenario.msg);

    let start = Instant::now();
    let proof = zk::prove::<Certificate, blake2b_simd::State>(
        &env.srs,
        &env.pk,
        &env.relation,
        &instance,
        scenario.witness,
        OsRng,
    )
    .expect("Proof generation should not fail");
    let duration = start.elapsed();
    println!("\nProof generation took: {:?}", duration);
    println!("Proof size: {:?}", proof.len());

    let start = Instant::now();
    assert!(
        zk::verify::<Certificate, blake2b_simd::State>(
            &env.srs.verifier_params(),
            &env.vk,
            &instance,
            None,
            &proof
        )
        .is_ok()
    );
    let duration = start.elapsed();
    println!("Proof verification took: {:?}", duration);
}

pub(crate) fn run_certificate_case(case_name: &str, k: u32, quorum: u32) {
    let env = setup_certificate_env(case_name, k, quorum);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers);

    let merkle_root = merkle_tree.root();
    // message to be signed
    let msg = F::from(42);

    let witness = build_witness(&sks, &leaves, &merkle_tree, merkle_root, msg, quorum);
    let scenario = CertificateScenario {
        merkle_root,
        msg,
        witness,
    };

    prove_and_verify(&env, scenario);
}
