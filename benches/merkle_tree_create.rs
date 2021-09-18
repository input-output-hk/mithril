use std::time::Duration;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use blstrs::{G2Projective, G1Projective};

use mithril::merkle_tree::MerkleTree;
use mithril::msp::{MspPk, MspMvk};

pub fn merkle_tree_create_benchmark(c: &mut Criterion) {
    // Generate random hash values
    let n: usize = 2_usize.pow(5_u32);
    let ps: Vec<MspPk> = (0..n).map(|_| {
        let x = rand::random::<u64>().to_le_bytes();
        let y = rand::random::<u64>().to_le_bytes();
        let z = rand::random::<u64>().to_le_bytes();
        let mvk = MspMvk(G2Projective::hash_to_curve(&x, b"mvk_bench", &[]));
        let k1 = G1Projective::hash_to_curve(&y, b"mvk_bench", &[]);
        let k2 = G1Projective::hash_to_curve(&z, b"mvk_bench", &[]);
        MspPk { mvk, k1, k2 }
    }).collect();

    c.bench_function("Merkle Tree create 2**5", |b| b.iter(|| MerkleTree::create(black_box(&ps))));
}

pub fn merkle_tree_verify_benchmark(c: &mut Criterion) {
    // Generate random hash values
    let n: usize = 2_usize.pow(5_u32);
    let ps: Vec<MspPk> = (0..n).map(|_| {
        let x = rand::random::<u64>().to_le_bytes();
        let y = rand::random::<u64>().to_le_bytes();
        let z = rand::random::<u64>().to_le_bytes();
        let mvk = MspMvk(G2Projective::hash_to_curve(&x, b"mvk_bench", &[]));
        let k1 = G1Projective::hash_to_curve(&y, b"mvk_bench", &[]);
        let k2 = G1Projective::hash_to_curve(&z, b"mvk_bench", &[]);
        MspPk { mvk, k1, k2 }
    }).collect();

    let mt =  MerkleTree::create(&ps);
    let i = rand::random::<usize>() % n;
    let path = mt.get_path(i);
    c.bench_function("Merkle Tree verify 2**5", |b| b.iter(|| mt.check(&ps[i], i, &path)));
}

criterion_group!(name = benches;
                 config = Criterion::default().measurement_time(Duration::new(20, 0));
                 targets = merkle_tree_create_benchmark, merkle_tree_verify_benchmark);
criterion_main!(benches);
