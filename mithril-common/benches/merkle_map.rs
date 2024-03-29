use std::ops::Range;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use mithril_common::{
    crypto_helper::{MKMap, MKMapNode, MKMapProof, MKMapValue, MKTree, MKTreeNode},
    entities::BlockRange,
};

// Shortcuts for magnitudes: K for thousand, M for million, B for billion
const K: u64 = 1_000;
const M: u64 = 1_000 * K;
const B: u64 = 1_000 * M;
const TOTAL_TRANSACTIONS_BENCHES: &[u64] = &[
    K,
    10 * K,
    100 * K,
    M,
    10 * M,
    50 * M,
    100 * M,
    250 * M,
    500 * M,
    B,
];
const BLOCK_RANGE_LENGTH_BENCH: u64 = 15;
const TOTAL_TRANSACTIONS_PER_BLOCK: u64 = 50;
const MAX_TRANSACTIONS_PER_PROOF_BENCH: u64 = 100;

fn generate_block_ranges_nodes_iterator(
    total_transactions: u64,
    total_transactions_per_block: u64,
    block_range_length: u64,
    max_uncompressed_block_ranges: u64,
) -> impl Iterator<Item = (BlockRange, MKMapNode<BlockRange>)> {
    let total_transactions_per_block_range = total_transactions_per_block * block_range_length;
    let total_block_ranges = total_transactions / total_transactions_per_block_range;
    assert!(
        total_block_ranges > 0,
        "total_block_ranges should be strictly greater than 0"
    );
    (0..total_block_ranges).map(move |block_range_index| {
        let block_range = BlockRange::new(
            block_range_index * total_transactions_per_block_range,
            (block_range_index + 1) * total_transactions_per_block_range,
        );
        let mk_map_node = if block_range_index < max_uncompressed_block_ranges {
            let leaves = <Range<u64> as Clone>::clone(&block_range)
                .map(|leaf_index| leaf_index.to_string())
                .collect::<Vec<_>>();
            let merkle_tree_block_range = MKTree::new(&leaves).unwrap();
            merkle_tree_block_range.into()
        } else {
            MKMapNode::TreeNode(block_range.to_string().into())
        };

        (block_range, mk_map_node)
    })
}

fn generate_merkle_map_compressed(
    block_ranges_nodes_iterator: impl Iterator<Item = (BlockRange, MKMapNode<BlockRange>)>,
) -> MKMap<BlockRange, MKMapNode<BlockRange>> {
    let mut mk_map = MKMap::new(&[]).unwrap();
    for (block_range, mk_map_node) in block_ranges_nodes_iterator {
        mk_map
            .insert(block_range, mk_map_node.compute_root().unwrap().into())
            .unwrap();
    }
    mk_map
}

fn generate_merkle_map_root(
    block_ranges_nodes_iterator: impl Iterator<Item = (BlockRange, MKMapNode<BlockRange>)>,
    mk_map_compressed: &MKMap<BlockRange, MKMapNode<BlockRange>>,
) -> MKMapNode<BlockRange> {
    let total_full_mk_tree = 1;
    let (mk_map, _) = generate_merkle_map(
        block_ranges_nodes_iterator,
        mk_map_compressed,
        total_full_mk_tree,
    );

    mk_map.compute_root().unwrap().into()
}

fn generate_merkle_map_proof(
    block_ranges_nodes_iterator: impl Iterator<Item = (BlockRange, MKMapNode<BlockRange>)>,
    mk_map_compressed: &MKMap<BlockRange, MKMapNode<BlockRange>>,
    total_proofs: u64,
) -> MKMapProof<BlockRange> {
    let (mk_map, leaves_to_prove_all) =
        generate_merkle_map(block_ranges_nodes_iterator, mk_map_compressed, total_proofs);

    mk_map.compute_proof(&leaves_to_prove_all).unwrap()
}

fn generate_merkle_map(
    block_ranges_nodes_iterator: impl Iterator<Item = (BlockRange, MKMapNode<BlockRange>)>,
    mk_map_compressed: &MKMap<BlockRange, MKMapNode<BlockRange>>,
    total_proofs: u64,
) -> (MKMap<BlockRange, MKMapNode<BlockRange>>, Vec<MKTreeNode>) {
    let mut leaves_to_prove_all: Vec<MKTreeNode> = vec![];
    let mut mk_map = mk_map_compressed.clone();
    for (mk_map_key_to_prove, mk_map_node_to_prove) in &block_ranges_nodes_iterator
        .take(total_proofs as usize)
        .collect::<Vec<_>>()
    {
        let mktree_to_prove = if let MKMapNode::Tree(mktree_to_prove) = mk_map_node_to_prove {
            mktree_to_prove
        } else {
            panic!("Expected MKMapNode::Tree");
        };
        let leaves_to_prove = mktree_to_prove
            .leaves()
            .into_iter()
            .take(1)
            .collect::<Vec<_>>();
        leaves_to_prove_all.extend(leaves_to_prove);
        mk_map
            .insert(
                mk_map_key_to_prove.to_owned(),
                mk_map_node_to_prove.to_owned(),
            )
            .unwrap();
    }

    (mk_map, leaves_to_prove_all)
}

fn create_merkle_map_root_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group(format!(
        "create_merkle_map_root(blocks_ranges_length={BLOCK_RANGE_LENGTH_BENCH},txs_per_block={TOTAL_TRANSACTIONS_PER_BLOCK})"
    ));
    for total_leaves in TOTAL_TRANSACTIONS_BENCHES.iter() {
        let mk_trees_by_block_range_iterator = generate_block_ranges_nodes_iterator(
            *total_leaves,
            TOTAL_TRANSACTIONS_PER_BLOCK,
            BLOCK_RANGE_LENGTH_BENCH,
            1,
        );
        let mk_map_compressed = generate_merkle_map_compressed(mk_trees_by_block_range_iterator);
        group.bench_with_input(
            BenchmarkId::from_parameter(total_leaves),
            total_leaves,
            |b, &_total_leaves| {
                b.iter(|| {
                    let mk_trees_by_block_range_iterator = generate_block_ranges_nodes_iterator(
                        *total_leaves,
                        TOTAL_TRANSACTIONS_PER_BLOCK,
                        BLOCK_RANGE_LENGTH_BENCH,
                        1,
                    );
                    generate_merkle_map_root(mk_trees_by_block_range_iterator, &mk_map_compressed);
                });
            },
        );
    }
    group.finish();
}

fn create_merkle_map_proof_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group(format!(
        "create_merkle_map_proof(blocks_ranges_length={BLOCK_RANGE_LENGTH_BENCH},txs_per_block={TOTAL_TRANSACTIONS_PER_BLOCK},txs_per_proof={MAX_TRANSACTIONS_PER_PROOF_BENCH})"
    ));
    for total_leaves in TOTAL_TRANSACTIONS_BENCHES.iter() {
        let mk_trees_by_block_range_iterator = generate_block_ranges_nodes_iterator(
            *total_leaves,
            TOTAL_TRANSACTIONS_PER_BLOCK,
            BLOCK_RANGE_LENGTH_BENCH,
            MAX_TRANSACTIONS_PER_PROOF_BENCH,
        );
        let mk_map_compressed = generate_merkle_map_compressed(mk_trees_by_block_range_iterator);

        group.bench_with_input(
            BenchmarkId::from_parameter(total_leaves),
            total_leaves,
            |b, &_total_leaves| {
                b.iter(|| {
                    let mk_trees_by_block_range_iterator = generate_block_ranges_nodes_iterator(
                        *total_leaves,
                        TOTAL_TRANSACTIONS_PER_BLOCK,
                        BLOCK_RANGE_LENGTH_BENCH,
                        MAX_TRANSACTIONS_PER_PROOF_BENCH,
                    );
                    generate_merkle_map_proof(
                        mk_trees_by_block_range_iterator,
                        &mk_map_compressed,
                        MAX_TRANSACTIONS_PER_PROOF_BENCH,
                    );
                });
            },
        );
    }
    group.finish();
}

fn verify_merkle_map_proof_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group(format!(
        "verify_merkle_map_proof(blocks_ranges_length={BLOCK_RANGE_LENGTH_BENCH},txs_per_block={TOTAL_TRANSACTIONS_PER_BLOCK},txs_per_proof={MAX_TRANSACTIONS_PER_PROOF_BENCH})"
    ));
    for total_leaves in TOTAL_TRANSACTIONS_BENCHES.iter() {
        let mk_trees_by_block_range_iterator = generate_block_ranges_nodes_iterator(
            *total_leaves,
            TOTAL_TRANSACTIONS_PER_BLOCK,
            BLOCK_RANGE_LENGTH_BENCH,
            MAX_TRANSACTIONS_PER_PROOF_BENCH,
        );
        let mk_map_compressed = generate_merkle_map_compressed(mk_trees_by_block_range_iterator);
        let mk_trees_by_block_range_iterator = generate_block_ranges_nodes_iterator(
            *total_leaves,
            TOTAL_TRANSACTIONS_PER_BLOCK,
            BLOCK_RANGE_LENGTH_BENCH,
            MAX_TRANSACTIONS_PER_PROOF_BENCH,
        );
        let mk_map_proof = generate_merkle_map_proof(
            mk_trees_by_block_range_iterator,
            &mk_map_compressed,
            MAX_TRANSACTIONS_PER_PROOF_BENCH,
        );

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
    config = Criterion::default().sample_size(100);
    targets =
        create_merkle_map_root_benches,
        create_merkle_map_proof_benches,
        verify_merkle_map_proof_benches
);
criterion_main!(benches);
