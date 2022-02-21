use ark_bls12_377::Bls12_377;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand_core::{OsRng, RngCore};
use std::time::Duration;

use mithril::merkle_tree::MerkleTree;
use mithril::msp::{Msp, MspMvk};

type C = Bls12_377;
type H = blake2::Blake2b;

pub fn gen_keys(n: usize) -> Vec<MspMvk<C>> {
    let mut rng = OsRng::default();

    (0..n).map(|_| Msp::gen(&mut rng).1.mvk).collect()
}

pub fn merkle_tree_create_benchmark(c: &mut Criterion) {
    let ps = gen_keys(32);
    c.bench_function("Merkle Tree create 2**5", |b| {
        b.iter(|| MerkleTree::<_, H>::create(black_box(&ps)))
    });
}

pub fn merkle_tree_verify_benchmark(c: &mut Criterion) {
    let mut rng = OsRng::default();
    let ps = gen_keys(32);
    let mt = MerkleTree::<_, H>::create(&ps);
    let i = rng.next_u64() as usize % 32;
    let path = mt.get_path(i);
    c.bench_function("Merkle Tree verify 2**5", |b| {
        b.iter(|| mt.to_commitment().check(&ps[i], i, &path))
    });
}

criterion_group!(name = benches;
                 config = Criterion::default().measurement_time(Duration::new(20, 0));
                 targets = merkle_tree_create_benchmark, merkle_tree_verify_benchmark);
criterion_main!(benches);
