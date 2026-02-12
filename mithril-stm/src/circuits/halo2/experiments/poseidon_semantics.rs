use digest::Digest;
use ff::Field;
use midnight_circuits::hash::poseidon::PoseidonChip;
use midnight_circuits::instructions::hash::HashCPU;
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;

use crate::circuits::halo2::types::JubjubBase;
use crate::circuits::halo2::utils::field_bytes::digest_bytes_to_base;
use crate::hash::poseidon::MidnightPoseidonDigest;

fn check_case(label: &str, u: JubjubBase, v: JubjubBase, target: JubjubBase) {
    let a = PoseidonChip::<JubjubBase>::hash(&[u, v, target]);

    let mut bytes = Vec::with_capacity(96);
    bytes.extend_from_slice(&u.to_bytes_le());
    bytes.extend_from_slice(&v.to_bytes_le());
    bytes.extend_from_slice(&target.to_bytes_le());
    let digest = MidnightPoseidonDigest::digest(&bytes);

    let mut b_bytes = [0u8; 32];
    b_bytes.copy_from_slice(digest.as_slice());
    let b = digest_bytes_to_base(&b_bytes).unwrap_or_else(|| {
        println!("Poseidon semantics mismatch ({label}): digest not canonical");
        println!("u:      {:?}", u);
        println!("v:      {:?}", v);
        println!("target: {:?}", target);
        println!("A:      {:?}", a);
        println!("B raw:  {:?}", b_bytes);
        panic!("digest output is not a canonical field element");
    });

    if a != b {
        println!("Poseidon semantics mismatch ({label})");
        println!("u:      {:?}", u);
        println!("v:      {:?}", v);
        println!("target: {:?}", target);
        println!("A:      {:?}", a);
        println!("B raw:  {:?}", b_bytes);
        println!("B:      {:?}", b);
        panic!("Poseidon hash mismatch");
    }
}

#[test]
fn poseidon_semantics_equivalence() {
    const SAMPLES: usize = 1000;

    let mut rng = ChaCha20Rng::from_seed([7u8; 32]);
    for i in 0..SAMPLES {
        let u = JubjubBase::random(&mut rng);
        let v = JubjubBase::random(&mut rng);
        let target = JubjubBase::random(&mut rng);
        check_case(&format!("random_{i}"), u, v, target);
    }

    let zero = JubjubBase::ZERO;
    let max = -JubjubBase::ONE;
    check_case("zero_zero_zero", zero, zero, zero);
    check_case("max_max_max", max, max, max);

    println!("Poseidon semantics match for {SAMPLES} random samples + edge cases");
}
