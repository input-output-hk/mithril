use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use midnight_circuits::{hash::poseidon::PoseidonChip, instructions::hash::HashCPU};
use midnight_curves::Fq as JubjubBase;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

use mithril_stm::{BaseFieldElement, SchnorrSigningKey, SchnorrVerificationKey};

fn midnight_poseidon_hash(c: &mut Criterion, nr_sigs: usize) {
    let mut group = c.benchmark_group("Schnorr".to_string());

    const DST_SIGNATURE: JubjubBase = JubjubBase::from_raw([0u64, 0, 0, 0]);
    let input = vec![JubjubBase::from_raw([1u64, 0, 0, 0]); 10];
    let mut poseidon_input = vec![DST_SIGNATURE];

    group.bench_function(
        BenchmarkId::new("Midnight Poseidon 1 base field element", nr_sigs),
        |b| {
            b.iter(|| {
                for _ in 0..nr_sigs {
                    let _hash = PoseidonChip::<JubjubBase>::hash(&poseidon_input);
                }
            })
        },
    );

    poseidon_input.extend(input);
    group.bench_function(
        BenchmarkId::new(
            "Midnight Poseidon 11 base field elements (similar to signature)",
            nr_sigs,
        ),
        |b| {
            b.iter(|| {
                for _ in 0..nr_sigs {
                    let _hash = PoseidonChip::<JubjubBase>::hash(&poseidon_input);
                }
            })
        },
    );
}

fn sign_and_verify(c: &mut Criterion, nr_sigs: usize) {
    let mut group = c.benchmark_group("Schnorr".to_string());
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut rng_sig = ChaCha20Rng::from_seed([1u8; 32]);

    let mut msg = [0u8; 32];
    rng.fill_bytes(&mut msg);
    let base_input = BaseFieldElement::from(&msg[0..32].try_into().unwrap());
    let mut mvks = Vec::new();
    let mut msks = Vec::new();
    let mut sigs = Vec::new();
    for _ in 0..nr_sigs {
        let sk = SchnorrSigningKey::generate(&mut rng);
        let vk = SchnorrVerificationKey::new_from_signing_key(sk.clone());
        let sig = sk.sign(&[base_input], &mut rng_sig).unwrap();
        sigs.push(sig);
        mvks.push(vk);
        msks.push(sk);
    }

    group.bench_function(BenchmarkId::new("Signature", nr_sigs), |b| {
        b.iter(|| {
            for sk in msks.iter() {
                let _sig = sk.sign(&[base_input], &mut rng_sig).unwrap();
            }
        })
    });

    group.bench_function(BenchmarkId::new("Verification", nr_sigs), |b| {
        b.iter(|| {
            for (vk, sig) in mvks.iter().zip(sigs.iter()) {
                assert!(sig.verify(&[base_input], vk).is_ok());
            }
        })
    });
}

fn schnorr_benches(c: &mut Criterion) {
    sign_and_verify(c, 1000);
    midnight_poseidon_hash(c, 1000);
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(1000);
                 targets =
    schnorr_benches
);
criterion_main!(benches);
