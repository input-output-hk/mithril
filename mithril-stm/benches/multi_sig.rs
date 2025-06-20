use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use mithril_stm::{SigningKey, VerificationKey};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

fn aggregate_and_verify(c: &mut Criterion, nr_sigs: usize) {
    let mut group = c.benchmark_group("BLS".to_string());
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

    let mut msg = [0u8; 32];
    rng.fill_bytes(&mut msg);
    let mut mvks = Vec::new();
    let mut sigs = Vec::new();
    for _ in 0..nr_sigs {
        let sk = SigningKey::generate(&mut rng);
        let vk = VerificationKey::from(&sk);
        let sig = sk.sign(&msg);
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

fn batch_bls_benches(c: &mut Criterion) {
    aggregate_and_verify(c, 856);
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(1000);
                 targets =
    batch_bls_benches
);
criterion_main!(benches);
