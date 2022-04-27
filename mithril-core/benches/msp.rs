use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use mithril::msp::Msp;
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

    let mut group = c.benchmark_group(format!("Multi-signatures/"));
    group.bench_function("Key generation", |b| b.iter(|| Msp::gen(&mut rng)));

    let (sk, _) = Msp::gen(&mut rng);
    group.bench_function("Single signature", |b| b.iter(|| Msp::sig(&sk, &msg)));

    for _ in 0..*NR_SIGNERS.last().unwrap() {
        let (sk, pk) = Msp::gen(&mut rng);
        let sig = Msp::sig(&sk, &msg);
        sigs.push(sig);
        mvks.push(pk.mvk);
    }

    for &size in NR_SIGNERS.iter() {
        group.bench_with_input(BenchmarkId::new("Aggregate keys", size), &size, |b, &i| {
            b.iter(|| Msp::aggregate_keys(&mvks[..i]))
        });
        group.bench_with_input(
            BenchmarkId::new("Aggregate signatures", size),
            &size,
            |b, &i| b.iter(|| Msp::aggregate_sigs(&sigs[..i])),
        );
    }
    let ivk = Msp::aggregate_keys(&mvks);
    let mu = Msp::aggregate_sigs(&sigs);
    group.bench_function("Signature verification", |b| {
        b.iter(|| Msp::aggregate_ver(&msg, &ivk, &mu))
    });

    group.finish();
}

criterion_group!(name = benches;
                 config = Criterion::default().measurement_time(Duration::new(5, 0));
                 targets = msp);
criterion_main!(benches);
