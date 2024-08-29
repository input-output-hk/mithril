use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use mithril_common::crypto_helper::{MKTree, MKTreeNode};
use mithril_signer::MKTreeStoreSqlite;

// Shortcuts for magnitudes: K for thousand, M for million
const K: usize = 1_000;
const M: usize = 1_000 * K;
const TOTAL_LEAVES_BENCHES: &[usize] = &[K, 10 * K, 100 * K, M, 10 * M];

fn generate_merkle_tree(total_leaves: usize) -> MKTree<MKTreeStoreSqlite> {
    MKTree::new(
        (0..total_leaves)
            .map(|i| format!("bench-{i}").into())
            .collect::<Vec<MKTreeNode>>()
            .as_slice(),
    )
    .unwrap()
}

fn create_merkle_tree_root_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group("create_merkle_tree_signer_sqlite");
    for total_leaves in TOTAL_LEAVES_BENCHES.iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(total_leaves),
            total_leaves,
            |b, &total_leaves| {
                b.iter(|| {
                    let mk_tree = generate_merkle_tree(total_leaves);
                    mk_tree.compute_root().unwrap();
                });
            },
        );
    }
    group.finish();
}

criterion_group!(
    name=benches;
    config = Criterion::default().sample_size(10);
    targets=
      create_merkle_tree_root_benches,
);
criterion_main!(benches);
