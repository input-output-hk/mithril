use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

use mithril_common::entities::{BlockNumber, BlockRange};

fn all_block_ranges_in(c: &mut Criterion) {
    let mut group = c.benchmark_group("all_block_ranges_in");
    for end_bound in [
        BlockRange::LENGTH * 100,
        BlockRange::LENGTH * 10_000,
        BlockRange::LENGTH * 10_000_000,
        BlockRange::LENGTH * 10_000_000_000,
    ] {
        group.bench_with_input(
            BenchmarkId::from_parameter(format!("0..{end_bound}")),
            &end_bound,
            |b, &end_bound| {
                b.iter(|| BlockRange::all_block_ranges_in(BlockNumber(0)..=end_bound));
            },
        );
    }
    group.finish();
}

criterion_group!(
    name = benches;
    config = Criterion::default().sample_size(100);
    targets = all_block_ranges_in
);
criterion_main!(benches);
