use std::ops::Range;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use mithril_common::{
    crypto_helper::{MKHashMap, MKTree},
    entities::BlockRange,
};

const K: usize = 1_000;
const M: usize = 1_000 * K;
const TOTAL_LEAVES_BENCHES: &[usize] = &[K, 10 * K, 100 * K, M, 10 * M];
const BLOCK_RANGE_LENGTH: u64 = 15;

fn generate_merkle_trees_by_block_range(
    total_leaves: usize,
    block_range_length: u64,
) -> Vec<(BlockRange, MKTree)> {
    (0..total_leaves as u64 / block_range_length)
        .map(|block_range_index| {
            let block_range = BlockRange::new(
                block_range_index * block_range_length,
                (block_range_index + 1) * block_range_length,
            );
            let leaves = <Range<u64> as Clone>::clone(&block_range)
                .map(|leaf_index| leaf_index.to_string())
                .collect::<Vec<_>>();
            let merkle_tree_block_range = MKTree::new(&leaves).unwrap();

            (block_range, merkle_tree_block_range)
        })
        .collect::<Vec<_>>()
}

fn create_merkle_map_root_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group("create_merkle_map");
    for total_leaves in TOTAL_LEAVES_BENCHES.iter() {
        let mk_trees_by_block_range =
            generate_merkle_trees_by_block_range(*total_leaves, BLOCK_RANGE_LENGTH)
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect::<Vec<_>>();

        group.bench_with_input(
            BenchmarkId::from_parameter(total_leaves),
            total_leaves,
            |b, &_total_leaves| {
                b.iter(|| {
                    let mk_map = MKHashMap::new(mk_trees_by_block_range.as_slice()).unwrap();
                    mk_map.compute_root().unwrap();
                });
            },
        );
    }
    group.finish();
}

fn create_merkle_map_proof_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group("create_merkle_map_proof");
    for total_leaves in TOTAL_LEAVES_BENCHES.iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(total_leaves),
            total_leaves,
            |b, &_total_leaves| {
                b.iter(|| {
                    let mk_trees_by_block_range =
                        generate_merkle_trees_by_block_range(*total_leaves, BLOCK_RANGE_LENGTH);
                    let mk_map_key_to_prove = mk_trees_by_block_range[0].0.to_owned();
                    let mk_map_tree_to_prove = mk_trees_by_block_range[0].1.to_owned();
                    let mk_trees_by_block_range = mk_trees_by_block_range
                        .into_iter()
                        .map(|(k, v)| (k, v.into()))
                        .collect::<Vec<_>>();
                    let mk_map = MKHashMap::new(mk_trees_by_block_range.as_slice()).unwrap();
                    let mk_map_value_to_prove = mk_trees_by_block_range[0].1.to_owned();
                    let mut mk_map_compressed = mk_map.compress().unwrap();
                    mk_map_compressed
                        .insert(mk_map_key_to_prove, mk_map_value_to_prove)
                        .unwrap();
                    let leaves_to_prove = mk_map_tree_to_prove
                        .leaves()
                        .into_iter()
                        .take(1)
                        .collect::<Vec<_>>();
                    mk_map.compute_proof(&leaves_to_prove).unwrap();
                });
            },
        );
    }
    group.finish();
}

fn verify_merkle_map_proof_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group("verify_merkle_map_proof");
    for total_leaves in TOTAL_LEAVES_BENCHES.iter() {
        let mk_trees_by_block_range =
            generate_merkle_trees_by_block_range(*total_leaves, BLOCK_RANGE_LENGTH);
        let mk_map_key_to_prove = mk_trees_by_block_range[0].0.to_owned();
        let mk_map_tree_to_prove = mk_trees_by_block_range[0].1.to_owned();
        let mk_trees_by_block_range = mk_trees_by_block_range
            .into_iter()
            .map(|(k, v)| (k, v.into()))
            .collect::<Vec<_>>();
        let mk_map = MKHashMap::new(mk_trees_by_block_range.as_slice()).unwrap();
        let mk_map_value_to_prove = mk_trees_by_block_range[0].1.to_owned();
        let mut mk_map_compressed = mk_map.compress().unwrap();
        mk_map_compressed
            .insert(mk_map_key_to_prove, mk_map_value_to_prove)
            .unwrap();
        let leaves_to_prove = mk_map_tree_to_prove
            .leaves()
            .into_iter()
            .take(1)
            .collect::<Vec<_>>();
        let mk_map_proof = mk_map.compute_proof(&leaves_to_prove).unwrap();
        mk_map.compute_proof(&leaves_to_prove).unwrap();

        group.bench_with_input(
            BenchmarkId::from_parameter(total_leaves),
            total_leaves,
            |b, &_total_leaves| {
                b.iter(|| mk_map_proof.verify().unwrap());
            },
        );
    }
    group.finish();
}

criterion_group!(
    name=benches;
    config = Criterion::default().sample_size(10);
    targets=create_merkle_map_root_benches,create_merkle_map_proof_benches,verify_merkle_map_proof_benches
);
criterion_main!(benches);
