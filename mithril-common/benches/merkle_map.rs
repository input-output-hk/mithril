use std::ops::Range;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use mithril_common::{
    crypto_helper::{MKMap, MKMapProof, MKTree},
    entities::BlockRange,
    signable_builder,
};

const K: usize = 1_000;
const M: usize = 1_000 * K;
const TOTAL_LEAVES_BENCHES: &[usize] = &[K, 10 * K, 100 * K, M, 10 * M];
const BLOCK_RANGE_LENGTH_BENCH: u64 = signable_builder::BLOCK_RANGE_LENGTH;

fn generate_merkle_trees_by_block_range_iterator(
    total_leaves: usize,
    block_range_length: u64,
) -> impl Iterator<Item = (BlockRange, MKTree)> {
    (0..total_leaves as u64 / block_range_length).map(move |block_range_index| {
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
}

fn generate_merkle_map(
    mk_trees_by_block_range_iterator: impl Iterator<Item = (BlockRange, MKTree)>,
) -> MKMap<BlockRange> {
    let mut mk_map = MKMap::new(&[]).unwrap();
    for (block_range, mk_tree) in mk_trees_by_block_range_iterator {
        mk_map.insert(block_range, mk_tree.into()).unwrap();
    }
    mk_map
}

fn generate_merkle_map_compressed(
    mk_trees_by_block_range_iterator: impl Iterator<Item = (BlockRange, MKTree)>,
) -> MKMap<BlockRange> {
    let mut mk_map = MKMap::new(&[]).unwrap();
    for (block_range, mk_tree) in mk_trees_by_block_range_iterator {
        mk_map
            .insert(block_range, mk_tree.compute_root().unwrap().into())
            .unwrap();
    }
    mk_map
}

fn generate_merkle_map_proof(
    mk_trees_by_block_range_iterator: impl Iterator<Item = (BlockRange, MKTree)>,
    mk_map_compressed: &MKMap<BlockRange>,
) -> MKMapProof<BlockRange> {
    let (mk_map_key_to_prove, mk_map_tree_to_prove) =
        &mk_trees_by_block_range_iterator.take(1).collect::<Vec<_>>()[0];
    let leaves_to_prove = mk_map_tree_to_prove
        .leaves()
        .into_iter()
        .take(1)
        .collect::<Vec<_>>();
    let mut mk_map_compressed = mk_map_compressed.clone();
    mk_map_compressed
        .insert(
            mk_map_key_to_prove.to_owned(),
            mk_map_tree_to_prove.to_owned().into(),
        )
        .unwrap();
    mk_map_compressed.compute_proof(&leaves_to_prove).unwrap()
}

fn create_merkle_map_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group(format!(
        "create_merkle_map_ranges_{BLOCK_RANGE_LENGTH_BENCH}_blocks"
    ));
    for total_leaves in TOTAL_LEAVES_BENCHES.iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(total_leaves),
            total_leaves,
            |b, &_total_leaves| {
                b.iter(|| {
                    let mk_trees_by_block_range_iterator =
                        generate_merkle_trees_by_block_range_iterator(
                            *total_leaves,
                            BLOCK_RANGE_LENGTH_BENCH,
                        );
                    let mk_map = generate_merkle_map(mk_trees_by_block_range_iterator);
                    mk_map.compute_root().unwrap();
                });
            },
        );
    }
    group.finish();
}

fn create_merkle_map_compressed_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group(format!(
        "create_merkle_map_compressed_ranges_{BLOCK_RANGE_LENGTH_BENCH}_blocks"
    ));
    for total_leaves in TOTAL_LEAVES_BENCHES.iter() {
        let mk_trees_by_block_range_iterator =
            generate_merkle_trees_by_block_range_iterator(*total_leaves, BLOCK_RANGE_LENGTH_BENCH);
        let mk_map_compressed = generate_merkle_map_compressed(mk_trees_by_block_range_iterator);
        group.bench_with_input(
            BenchmarkId::from_parameter(total_leaves),
            total_leaves,
            |b, &_total_leaves| {
                b.iter(|| {
                    let mk_map_compressed_clone = mk_map_compressed.clone();
                    mk_map_compressed_clone.compute_root().unwrap();
                });
            },
        );
    }
    group.finish();
}

fn create_merkle_map_proof_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group(format!(
        "create_merkle_map_proof_ranges_{BLOCK_RANGE_LENGTH_BENCH}_blocks"
    ));
    for total_leaves in TOTAL_LEAVES_BENCHES.iter() {
        let mk_trees_by_block_range_iterator =
            generate_merkle_trees_by_block_range_iterator(*total_leaves, BLOCK_RANGE_LENGTH_BENCH);
        let mk_map_compressed = generate_merkle_map_compressed(mk_trees_by_block_range_iterator);

        group.bench_with_input(
            BenchmarkId::from_parameter(total_leaves),
            total_leaves,
            |b, &_total_leaves| {
                b.iter(|| {
                    let mk_trees_by_block_range_iterator =
                        generate_merkle_trees_by_block_range_iterator(
                            *total_leaves,
                            BLOCK_RANGE_LENGTH_BENCH,
                        );
                    generate_merkle_map_proof(mk_trees_by_block_range_iterator, &mk_map_compressed);
                });
            },
        );
    }
    group.finish();
}

fn verify_merkle_map_proof_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group(format!(
        "verify_merkle_map_proof_ranges_{BLOCK_RANGE_LENGTH_BENCH}_blocks"
    ));
    for total_leaves in TOTAL_LEAVES_BENCHES.iter() {
        let mk_trees_by_block_range_iterator =
            generate_merkle_trees_by_block_range_iterator(*total_leaves, BLOCK_RANGE_LENGTH_BENCH);
        let mk_map_compressed = generate_merkle_map_compressed(mk_trees_by_block_range_iterator);
        let mk_trees_by_block_range_iterator =
            generate_merkle_trees_by_block_range_iterator(*total_leaves, BLOCK_RANGE_LENGTH_BENCH);
        let mk_map_proof =
            generate_merkle_map_proof(mk_trees_by_block_range_iterator, &mk_map_compressed);

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
    name = benches;
    config = Criterion::default().sample_size(10);
    targets =
        create_merkle_map_benches,
        create_merkle_map_compressed_benches,
        create_merkle_map_proof_benches,
        verify_merkle_map_proof_benches
);
criterion_main!(benches);
