use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use mithril::msp::{VerificationKey, VerificationKeyPoP, Signature, SigningKey};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use std::time::Duration;

static NR_SIGNERS: [usize; 6] = [8, 16, 32, 64, 128, 256];

fn msp(c: &mut Criterion) {
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);
    let mut mvks = Vec::new();
    let mut sigs = Vec::new();

    let mut group = c.benchmark_group("Multi-signatures/");
    group.bench_function("Key generation", |b| {
        b.iter(|| {
            let sk = SigningKey::gen(&mut rng);
            let _pk = VerificationKeyPoP::from(&sk);
        })
    });

    let sk = SigningKey::gen(&mut rng);
    group.bench_function("Single signature", |b| b.iter(|| sk.sign(&msg)));

    for _ in 0..*NR_SIGNERS.last().unwrap() {
        let sk = SigningKey::gen(&mut rng);
        let pk = VerificationKeyPoP::from(&sk);
        let sig = sk.sign(&msg);
        sigs.push(sig);
        mvks.push(pk.vk);
    }

    for &size in NR_SIGNERS.iter() {
        group.bench_with_input(BenchmarkId::new("Aggregate keys", size), &size, |b, &i| {
            b.iter(|| mvks[..i].iter().sum::<VerificationKey>())
        });
        group.bench_with_input(
            BenchmarkId::new("Aggregate signatures", size),
            &size,
            |b, &i| b.iter(|| sigs[..i].iter().sum::<Signature>()),
        );
    }
    let ivk = mvks.iter().sum();
    let mu = sigs.iter().sum::<Signature>();
    group.bench_function("Signature verification", |b| {
        b.iter(|| mu.verify(&msg, &ivk))
    });

    group.finish();
}

criterion_group!(name = benches;
                 config = Criterion::default().measurement_time(Duration::new(5, 0));
                 targets = msp);
criterion_main!(benches);
