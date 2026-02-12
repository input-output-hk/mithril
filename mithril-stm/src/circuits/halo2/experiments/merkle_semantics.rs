use ff::Field;
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;

use crate::circuits::halo2::off_circuit::merkle_tree::{MerkleTree, MTLeaf};
use crate::circuits::halo2::types::JubjubBase;
use crate::circuits::halo2::utils::field_bytes::digest_bytes_to_base;
use crate::hash::poseidon::MidnightPoseidonDigest;
use crate::membership_commitment::{MerkleTree as StmMerkleTree, MerkleTreeSnarkLeaf};
use crate::signature_scheme::{SchnorrSigningKey, SchnorrVerificationKey};
use crate::{LotteryTargetValue, VerificationKeyForSnark};

fn build_leaves(
    rng: &mut ChaCha20Rng,
    n: usize,
    target_override: Option<JubjubBase>,
) -> (Vec<MTLeaf>, Vec<MerkleTreeSnarkLeaf>) {
    let mut halo2_leaves = Vec::with_capacity(n);
    let mut stm_leaves = Vec::with_capacity(n);

    for i in 0..n {
        let sk = loop {
            let sk = SchnorrSigningKey::generate(&mut *rng)
                .expect("Failed to generate STM signing key");
            if SchnorrVerificationKey::new_from_signing_key(sk.clone()).is_ok() {
                break sk;
            }
        };
        let vk: VerificationKeyForSnark = SchnorrVerificationKey::new_from_signing_key(sk)
            .expect("Failed to build STM verification key");
        let target = match target_override {
            Some(value) if i == 0 => value,
            _ => JubjubBase::random(&mut *rng),
        };
        let target_value =
            LotteryTargetValue::from_bytes(&target.to_bytes_le()).expect("Invalid target bytes");

        halo2_leaves.push(MTLeaf(vk.to_bytes(), target));
        stm_leaves.push(MerkleTreeSnarkLeaf(vk, target_value));
    }

    (halo2_leaves, stm_leaves)
}

fn check_tree(label: &str, halo2_leaves: &[MTLeaf], stm_leaves: &[MerkleTreeSnarkLeaf]) {
    let halo2_tree = MerkleTree::create(halo2_leaves);
    let halo2_root = halo2_tree.root();

    let stm_tree = StmMerkleTree::<MidnightPoseidonDigest, MerkleTreeSnarkLeaf>::new(stm_leaves);
    let stm_root_bytes = stm_tree.to_merkle_tree_commitment().root;
    let b_bytes: [u8; 32] = stm_root_bytes
        .as_slice()
        .try_into()
        .expect("STM root bytes must be 32 bytes");
    let stm_root = digest_bytes_to_base(&b_bytes).unwrap_or_else(|| {
        println!("Merkle semantics mismatch ({label}): STM root not canonical");
        println!("STM root bytes: {:?}", stm_root_bytes);
        panic!("STM root bytes did not decode to a field element");
    });

    if halo2_root != stm_root {
        println!("Merkle semantics mismatch ({label})");
        println!("Halo2 root: {:?}", halo2_root);
        println!("STM root bytes: {:?}", stm_root_bytes);
        println!("STM root: {:?}", stm_root);
        panic!("Merkle root mismatch");
    }
}

#[test]
fn merkle_root_semantics_equivalence() {
    let mut rng = ChaCha20Rng::from_seed([9u8; 32]);

    for &n in &[4usize, 8usize] {
        let (halo2_leaves, stm_leaves) = build_leaves(&mut rng, n, None);
        check_tree(&format!("random_n{n}"), &halo2_leaves, &stm_leaves);

        let (halo2_zero, stm_zero) = build_leaves(&mut rng, n, Some(JubjubBase::ZERO));
        check_tree(&format!("zero_target_n{n}"), &halo2_zero, &stm_zero);
    }

    println!("Merkle root semantics match for n=4 and n=8");
}
