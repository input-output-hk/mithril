//! Benchmark helpers for the `StmCertificateCircuit`.
//!
//! Exposes `BenchEnv` and `BenchWitness` for Criterion benchmarks that measure
//! VK/PK setup, proof generation, and proof verification across parameter tiers.
//!
//! Gated behind `benchmark-internals` so this module is never compiled into release
//! builds unless explicitly requested.
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;
use std::sync::{Arc, LazyLock, RwLock};

use anyhow::{Context, anyhow};
use midnight_circuits::hash::poseidon::PoseidonState;
use midnight_curves::Bls12;
use midnight_proofs::poly::kzg::params::ParamsKZG;
use midnight_proofs::utils::SerdeFormat;
use midnight_zk_stdlib::{self as zk, MidnightPK, MidnightVK};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;

use crate::circuits::halo2::circuit::StmCertificateCircuit;
use crate::circuits::halo2::types::CircuitBase;
use crate::circuits::halo2::witness::{
    CircuitMerkleTreeLeaf, CircuitWitnessEntry, LotteryTargetValue as CircuitLotteryTargetValue,
    MerkleRoot, SignedMessageWithoutPrefix,
};
use crate::hash::poseidon::MidnightPoseidonDigest;
use crate::membership_commitment::{
    MerkleTree as StmMerkleTree, MerkleTreeSnarkLeaf as StmMerkleTreeSnarkLeaf,
};
use crate::signature_scheme::{BaseFieldElement, SchnorrSigningKey, SchnorrVerificationKey};
use crate::{LotteryIndex, Parameters, StmResult};

const DEFAULT_NUM_SIGNERS: usize = 3000;
const DEFAULT_BENCH_MSG: u64 = 42;

type BenchKeyPair = Arc<(MidnightVK, MidnightPK<StmCertificateCircuit>)>;
type BenchKeysCache = HashMap<BenchCircuitConfig, BenchKeyPair>;

static BENCH_KEYS_CACHE: LazyLock<RwLock<BenchKeysCache>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct BenchCircuitConfig {
    circuit_degree: u32,
    k: u32,
    m: u32,
    merkle_tree_depth: u32,
}

/// Full benchmark environment: SRS, compiled circuit, and derived VK/PK.
///
/// Construct with `BenchEnv::new`, then call `build_witness` + `prove` / `verify`
/// inside Criterion benchmark closures.
pub struct BenchEnv {
    srs: ParamsKZG<Bls12>,
    circuit: StmCertificateCircuit,
    vk: MidnightVK,
    pk: MidnightPK<StmCertificateCircuit>,
    num_signers: usize,
    k: u32,
}

/// Pre-built witness ready for repeated `BenchEnv::prove` calls.
pub struct BenchWitness {
    merkle_tree_commitment: MerkleRoot,
    message: SignedMessageWithoutPrefix,
    entries: Vec<CircuitWitnessEntry>,
}

impl BenchEnv {
    /// Construct a full benchmark environment for the given parameters.
    ///
    /// Loads or generates the SRS deterministically (seed 42, same as golden tests).
    /// VK/PK derivation is cached per `(circuit_degree, k, m, depth)`.
    pub fn new(circuit_degree: u32, k: u32, m: u32) -> StmResult<Self> {
        let srs = load_or_generate_srs(circuit_degree)?;
        let num_signers = DEFAULT_NUM_SIGNERS;
        let depth = num_signers.next_power_of_two().trailing_zeros();
        let params = Parameters {
            k: k as u64,
            m: m as u64,
            phi_f: 0.2,
        };
        let circuit = StmCertificateCircuit::try_new(&params, depth)
            .context("BenchEnv: failed to create StmCertificateCircuit")?;
        let config = BenchCircuitConfig {
            circuit_degree,
            k,
            m,
            merkle_tree_depth: depth,
        };
        let key_pair = get_or_build_bench_keys(config, &circuit, &srs)?;

        Ok(Self {
            srs,
            circuit,
            vk: key_pair.0.clone(),
            pk: key_pair.1.clone(),
            num_signers,
            k,
        })
    }

    /// Derive a fresh VK/PK pair from the stored SRS and circuit.
    ///
    /// Call this inside a Criterion `iter` closure to benchmark key-derivation time.
    pub fn setup_keys_for_bench(&self) -> (MidnightVK, MidnightPK<StmCertificateCircuit>) {
        let vk = zk::setup_vk(&self.srs, &self.circuit);
        let pk = zk::setup_pk(&self.circuit, &vk);
        (vk, pk)
    }

    /// Build the default witness for `self.k` lottery indices.
    ///
    /// Uses 3 000 deterministic signers (depth = 12, seed = `[0u8; 32]`).
    pub fn build_witness(&self) -> StmResult<BenchWitness> {
        let message = SignedMessageWithoutPrefix::from(DEFAULT_BENCH_MSG);
        let (tree, signer_fixtures) = build_bench_merkle_tree(self.num_signers)?;
        let commitment = bench_merkle_root(&tree)?;
        let entries = build_bench_witness(&tree, &signer_fixtures, commitment, message, self.k)?;
        Ok(BenchWitness {
            merkle_tree_commitment: commitment,
            message,
            entries,
        })
    }

    /// Generate a SNARK proof for `witness`.
    ///
    /// `witness.entries` is cloned internally so `witness` can be reused across iterations.
    pub fn prove(&self, witness: &BenchWitness) -> StmResult<Vec<u8>> {
        let instance = (witness.merkle_tree_commitment, witness.message);
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let entries = witness.entries.clone();
        zk::prove::<StmCertificateCircuit, PoseidonState<CircuitBase>>(
            &self.srs,
            &self.pk,
            &self.circuit,
            &instance,
            entries,
            &mut rng,
        )
        .map_err(|e| anyhow::Error::new(e).context("BenchEnv: proof generation failed"))
    }

    /// Verify a SNARK proof against `witness`'s public inputs.
    pub fn verify(&self, proof: &[u8], witness: &BenchWitness) -> StmResult<()> {
        let instance = (witness.merkle_tree_commitment, witness.message);
        zk::verify::<StmCertificateCircuit, PoseidonState<CircuitBase>>(
            &self.srs.verifier_params(),
            &self.vk,
            &instance,
            None,
            proof,
        )
        .map_err(|e| anyhow::Error::new(e).context("BenchEnv: proof verification failed"))
    }

    /// Return the serialized byte length of the verification key.
    pub fn vk_size_bytes(&self) -> usize {
        let mut buf = vec![];
        self.vk
            .write(&mut buf, SerdeFormat::RawBytes)
            .expect("VK serialization should not fail");
        buf.len()
    }

    /// Print the circuit cost model (rows, columns, gates) to stdout.
    pub fn print_circuit_cost(&self) {
        println!("{:?}", zk::cost_model(&self.circuit));
    }
}

struct SignerEntry {
    sk: SchnorrSigningKey,
    vk: SchnorrVerificationKey,
    circuit_target: CircuitLotteryTargetValue,
}

fn build_bench_merkle_tree(
    num_signers: usize,
) -> StmResult<(
    StmMerkleTree<MidnightPoseidonDigest, StmMerkleTreeSnarkLeaf>,
    Vec<SignerEntry>,
)> {
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let circuit_target = -CircuitLotteryTargetValue::ONE;
    let stm_target = circuit_target.into();

    let mut signer_entries = Vec::with_capacity(num_signers);
    let mut stm_leaves = Vec::with_capacity(num_signers);

    for _ in 0..num_signers {
        let sk = SchnorrSigningKey::generate(&mut rng);
        let vk = SchnorrVerificationKey::new_from_signing_key(sk.clone());
        stm_leaves.push(StmMerkleTreeSnarkLeaf(vk, stm_target));
        signer_entries.push(SignerEntry {
            sk,
            vk,
            circuit_target,
        });
    }

    let tree = StmMerkleTree::<MidnightPoseidonDigest, StmMerkleTreeSnarkLeaf>::new(&stm_leaves);
    Ok((tree, signer_entries))
}

fn bench_merkle_root(
    tree: &StmMerkleTree<MidnightPoseidonDigest, StmMerkleTreeSnarkLeaf>,
) -> StmResult<MerkleRoot> {
    let root_bytes = tree.to_merkle_tree_commitment().root;
    let root_array: [u8; 32] = root_bytes
        .as_slice()
        .try_into()
        .map_err(|_| anyhow!("bench: merkle root digest has unexpected length"))?;
    BaseFieldElement::from_bytes(&root_array)
        .ok()
        .map(Into::into)
        .ok_or_else(|| anyhow!("bench: merkle root digest is not a canonical field element"))
}

fn build_bench_witness(
    tree: &StmMerkleTree<MidnightPoseidonDigest, StmMerkleTreeSnarkLeaf>,
    signer_entries: &[SignerEntry],
    commitment: MerkleRoot,
    message: SignedMessageWithoutPrefix,
    k: u32,
) -> StmResult<Vec<CircuitWitnessEntry>> {
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let num_signers = signer_entries.len();
    let mut witness = Vec::with_capacity(k as usize);

    for i in 0..k as usize {
        let signer_index = i % num_signers;
        let entry = &signer_entries[signer_index];

        let stm_path = tree.compute_merkle_tree_path(signer_index);
        let merkle_path = crate::circuits::halo2::witness::MerklePath::try_from(&stm_path)
            .map_err(|e| anyhow!("bench: merkle path conversion failed: {e}"))?;

        let transcript: [BaseFieldElement; 2] = [commitment.into(), message.into()];
        let signature = entry
            .sk
            .sign_unique(&transcript, &mut rng)
            .map_err(|_| anyhow!("bench: signature generation failed"))?;

        let leaf = CircuitMerkleTreeLeaf(entry.vk, entry.circuit_target);

        witness.push(CircuitWitnessEntry {
            leaf,
            merkle_path,
            unique_schnorr_signature: signature,
            lottery_index: i as LotteryIndex,
        });
    }

    Ok(witness)
}

fn get_or_build_bench_keys(
    config: BenchCircuitConfig,
    circuit: &StmCertificateCircuit,
    srs: &ParamsKZG<Bls12>,
) -> StmResult<BenchKeyPair> {
    if let Some(pair) = BENCH_KEYS_CACHE
        .read()
        .map_err(|_| anyhow!("bench keys cache lock poisoned on read"))?
        .get(&config)
        .cloned()
    {
        return Ok(pair);
    }

    let vk = zk::setup_vk(srs, circuit);
    let pk = zk::setup_pk(circuit, &vk);
    let pair = Arc::new((vk, pk));

    BENCH_KEYS_CACHE
        .write()
        .map_err(|_| anyhow!("bench keys cache lock poisoned on write"))?
        .insert(config, pair.clone());

    Ok(pair)
}

fn load_or_generate_srs(circuit_degree: u32) -> StmResult<ParamsKZG<Bls12>> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let assets_dir = manifest_dir.join("src").join("circuits").join("halo2").join("assets");
    let path = assets_dir.join(format!("params_kzg_unsafe_{circuit_degree}"));

    if path.exists() {
        let file = File::open(&path)
            .with_context(|| format!("bench: failed to open SRS at '{}'", path.display()))?;
        let mut reader = BufReader::new(file);
        return ParamsKZG::read_custom(&mut reader, SerdeFormat::RawBytesUnchecked)
            .with_context(|| format!("bench: failed to parse SRS at '{}'", path.display()));
    }

    fs::create_dir_all(&assets_dir).with_context(|| {
        format!(
            "bench: failed to create SRS directory at '{}'",
            assets_dir.display()
        )
    })?;

    let srs = ParamsKZG::unsafe_setup(circuit_degree, ChaCha20Rng::seed_from_u64(42));
    let file = File::create(&path)
        .with_context(|| format!("bench: failed to create SRS file at '{}'", path.display()))?;
    let mut writer = BufWriter::new(file);
    srs.write_custom(&mut writer, SerdeFormat::RawBytesUnchecked)
        .with_context(|| format!("bench: failed to write SRS to '{}'", path.display()))?;

    Ok(srs)
}
