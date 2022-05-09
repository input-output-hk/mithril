use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand_core::{OsRng, RngCore};
use std::time::Duration;

use mithril::merkle_tree::MerkleTree;

type H = blake2::Blake2b;

pub fn gen_keys(n: usize) -> Vec<Vec<u8>> {
    let mut rng = OsRng::default();
    let mut result = Vec::with_capacity(n);
    let mut val = [0u8; 48];

    for _ in 0..n {
        rng.fill_bytes(&mut val);
        result.push(val.to_vec());
    }
    result
}

pub fn merkle_tree_create_benchmark(c: &mut Criterion) {
    let ps = gen_keys(32);
    c.bench_function("Merkle Tree create 2**5", |b| {
        b.iter(|| MerkleTree::<H>::create(black_box(&ps)))
    });
}

pub fn merkle_tree_verify_benchmark(c: &mut Criterion) {
    let mut rng = OsRng::default();
    let ps = gen_keys(32);
    let mt = MerkleTree::<H>::create(&ps);
    let i = rng.next_u64() % 32;
    let path = mt.get_path(i as usize);
    c.bench_function("Merkle Tree verify 2**5", |b| {
        b.iter(|| mt.to_commitment().check(&ps[i as usize], &path))
    });
}

criterion_group!(name = benches;
                 config = Criterion::default().measurement_time(Duration::new(20, 0));
                 targets = merkle_tree_create_benchmark, merkle_tree_verify_benchmark);
criterion_main!(benches);
