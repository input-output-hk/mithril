use ark_bls12_377::Bls12_377;
use ark_bls12_381::Bls12_381;
use ark_ec::PairingEngine;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use mithril::msp::Msp;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use std::time::Duration;

static NR_SIGNERS: [usize; 6] = [8, 16, 32, 64, 128, 256];

fn msp<PE: PairingEngine>(c: &mut Criterion, curve: &str) {
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);
    let mut mvks = Vec::new();
    let mut sigs = Vec::new();

    let mut group = c.benchmark_group(format!("Multi-signatures/{:?}", curve));
    group.bench_function("Key generation", |b| b.iter(|| Msp::<PE>::gen(&mut rng)));

    let (sk, _) = Msp::<PE>::gen(&mut rng);
    group.bench_function("Single signature", |b| b.iter(|| Msp::sig(&sk, &msg)));

    for _ in 0..*NR_SIGNERS.last().unwrap() {
        let (sk, pk) = Msp::<PE>::gen(&mut rng);
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

fn msp_bls12_377(c: &mut Criterion) {
    msp::<Bls12_377>(c, "Bls12_377")
}

fn msp_bls12_381(c: &mut Criterion) {
    msp::<Bls12_381>(c, "Bls12_381")
}

criterion_group!(name = benches;
                 config = Criterion::default().measurement_time(Duration::new(5, 0));
                 targets = msp_bls12_377, msp_bls12_381);
criterion_main!(benches);
