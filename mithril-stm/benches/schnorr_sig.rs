use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use dusk_jubjub::Fq as JubjubBase;
use dusk_poseidon::{Domain, Hash};
use mithril_stm::{SchnorrSigningKey, SchnorrVerificationKey};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

fn dusk_poseidon_hash(c: &mut Criterion, nr_sigs: usize) {
    let mut group = c.benchmark_group("Schnorr".to_string());

    const DST_SIGNATURE: JubjubBase = JubjubBase::from_raw([0u64, 0, 0, 0]);
    let input = vec![JubjubBase::from_raw([1u64, 0, 0, 0]); 10];
    let mut poseidon_input = vec![DST_SIGNATURE];
    poseidon_input.extend(input);

    group.bench_function(BenchmarkId::new("Dusk Poseidon 1 scalar", nr_sigs), |b| {
        b.iter(|| {
            for _ in 0..nr_sigs {
                let _hash = Hash::digest_truncated(Domain::Other, &[DST_SIGNATURE])[0];
            }
        })
    });

    group.bench_function(
        BenchmarkId::new("Dusk Poseidon 11 scalars (similar to signature)", nr_sigs),
        |b| {
            b.iter(|| {
                for _ in 0..nr_sigs {
                    let _hash = Hash::digest_truncated(Domain::Other, &poseidon_input)[0];
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
    let mut mvks = Vec::new();
    let mut msks = Vec::new();
    let mut sigs = Vec::new();
    for _ in 0..nr_sigs {
        let sk = SchnorrSigningKey::try_generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::from(&sk);
        let sig = sk.sign(&msg, &mut rng_sig).unwrap();
        sigs.push(sig);
        mvks.push(vk);
        msks.push(sk);
    }

    group.bench_function(BenchmarkId::new("Signature", nr_sigs), |b| {
        b.iter(|| {
            for sk in msks.iter() {
                let _sig = sk.sign(&msg, &mut rng_sig).unwrap();
            }
        })
    });

    group.bench_function(BenchmarkId::new("Verification", nr_sigs), |b| {
        b.iter(|| {
            for (vk, sig) in mvks.iter().zip(sigs.iter()) {
                assert!(sig.verify(&msg, vk).is_ok());
            }
        })
    });
}

fn schnorr_benches(c: &mut Criterion) {
    sign_and_verify(c, 1000);
    dusk_poseidon_hash(c, 1000);
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(1000);
                 targets =
    schnorr_benches
);
criterion_main!(benches);
