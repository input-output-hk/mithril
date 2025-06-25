use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use mithril_common::crypto_helper::{MKTree, MKTreeNode, MKTreeStoreInMemory};

// Shortcuts for magnitudes: K for thousand, M for million
const K: usize = 1_000;
const M: usize = 1_000 * K;
const TOTAL_LEAVES_BENCHES: &[usize] = &[K, 10 * K, 100 * K, M, 10 * M];
const TOTAL_LEAVES_TO_PROVE_BENCHES: &[usize] = &[1, 10, 100];
const TOTAL_LEAVES_TO_APPEND_BENCHES: &[usize] = &[1, 10, 100];

fn generate_merkle_tree(total_leaves: usize) -> MKTree<MKTreeStoreInMemory> {
    MKTree::new(
        (0..total_leaves)
            .map(|i| format!("bench-{i}").into())
            .collect::<Vec<MKTreeNode>>()
            .as_slice(),
    )
    .unwrap()
}

fn create_merkle_tree_root_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group("create_merkle_tree");
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

fn append_merkle_tree_benches(c: &mut Criterion) {
    for total_leaves_to_append in TOTAL_LEAVES_TO_APPEND_BENCHES.iter() {
        let mut group = c.benchmark_group(format!(
            "append_merkle_tree[appended_leaves={total_leaves_to_append}]"
        ));
        for total_leaves in TOTAL_LEAVES_BENCHES.iter() {
            let mut mk_tree = generate_merkle_tree(*total_leaves);
            let leaves_to_append = &mk_tree.leaves()[..*total_leaves_to_append];
            group.bench_with_input(
                BenchmarkId::from_parameter(total_leaves),
                total_leaves,
                |b, &_total_leaves| {
                    b.iter(|| {
                        mk_tree.append(leaves_to_append).unwrap();
                    });
                },
            );
        }
        group.finish();
    }
}

fn create_merkle_tree_proof_benches(c: &mut Criterion) {
    for total_leaves_to_prove in TOTAL_LEAVES_TO_PROVE_BENCHES.iter() {
        let mut group = c.benchmark_group(format!(
            "create_merkle_tree_proof[proved_leaves={total_leaves_to_prove}]"
        ));
        for total_leaves in TOTAL_LEAVES_BENCHES.iter() {
            let mk_tree = generate_merkle_tree(*total_leaves);
            let leaves_to_prove = &mk_tree.leaves()[..*total_leaves_to_prove];
            group.bench_with_input(
                BenchmarkId::from_parameter(total_leaves),
                total_leaves,
                |b, &_total_leaves| {
                    b.iter(|| {
                        mk_tree.compute_proof(leaves_to_prove).unwrap();
                    });
                },
            );
        }
        group.finish();
    }
}

fn verify_merkle_tree_proof_benches(c: &mut Criterion) {
    for total_leaves_to_prove in TOTAL_LEAVES_TO_PROVE_BENCHES.iter() {
        let mut group = c.benchmark_group(format!(
            "verify_merkle_tree_proof[proved_leaves={total_leaves_to_prove}]"
        ));
        for total_leaves in TOTAL_LEAVES_BENCHES.iter() {
            let mk_tree = generate_merkle_tree(*total_leaves);
            let leaves_to_prove = &mk_tree.leaves()[..*total_leaves_to_prove];
            let mk_proof = mk_tree.compute_proof(leaves_to_prove).unwrap();
            group.bench_with_input(
                BenchmarkId::from_parameter(total_leaves),
                total_leaves,
                |b, &_total_leaves| {
                    b.iter(|| mk_proof.verify().unwrap());
                },
            );
        }
        group.finish();
    }
}

criterion_group!(
    name=benches;
    config = Criterion::default().sample_size(10);
    targets=
      create_merkle_tree_root_benches,
      append_merkle_tree_benches,
      create_merkle_tree_proof_benches,
      verify_merkle_tree_proof_benches
);
criterion_main!(benches);
