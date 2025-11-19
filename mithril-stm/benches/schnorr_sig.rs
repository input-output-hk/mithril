use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use mithril_stm::{SchnorrSigningKey, SchnorrVerificationKey};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

fn sign_and_verify(c: &mut Criterion, nr_sigs: usize) {
    let mut group = c.benchmark_group("Schnorr".to_string());
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut rng_sig = ChaCha20Rng::from_seed([1u8; 32]);

    let mut msg = [0u8; 32];
    rng.fill_bytes(&mut msg);
    let mut mvks = Vec::new();
    let mut sigs = Vec::new();
    for _ in 0..nr_sigs {
        let sk = SchnorrSigningKey::generate(&mut rng);
        let vk = SchnorrVerificationKey::from(&sk);
        let sig = sk.sign(&msg, &mut rng_sig).unwrap();
        sigs.push(sig);
        mvks.push(vk);
    }

    group.bench_function(BenchmarkId::new("Individual verif", nr_sigs), |b| {
        b.iter(|| {
            for (vk, sig) in mvks.iter().zip(sigs.iter()) {
                assert!(sig.verify(&msg, vk).is_ok());
            }
        })
    });
}

fn schnorr_benches(c: &mut Criterion) {
    sign_and_verify(c, 300);
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(1000);
                 targets =
    schnorr_benches
);
criterion_main!(benches);
