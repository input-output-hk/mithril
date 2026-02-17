use std::sync::Arc;

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use sqlite::ConnectionThreadSafe;

use mithril_aggregator::database::repository::AggregatorCardanoChainDataRepository;
use mithril_cardano_node_chain::chain_importer::ChainDataStore;
use mithril_common::entities::{BlockNumber, CardanoBlockWithTransactions, SlotNumber};
use mithril_common::test::TempDir;
use mithril_persistence::sqlite::{ConnectionBuilder, SqliteConnectionPool};

fn cardano_tx_db_connection(db_file_name: &str) -> ConnectionThreadSafe {
    let db_path = TempDir::create("aggregator_benches", "bench_get_blocks_and_transactions")
        .join(db_file_name);

    if db_path.exists() {
        std::fs::remove_file(db_path.clone()).unwrap();
    }

    ConnectionBuilder::open_file(&db_path)
        .with_migrations(
            mithril_persistence::database::cardano_transaction_migration::get_migrations(),
        )
        .build()
        .unwrap()
}

fn generate_blocks_with_transaction(
    nb_blocks: usize,
    nb_transactions_per_block: usize,
) -> Vec<CardanoBlockWithTransactions> {
    (0..nb_blocks)
        .map(|i| {
            CardanoBlockWithTransactions::new(
                format!("block_hash-{i}"),
                BlockNumber(i as u64),
                SlotNumber(i as u64 * 100),
                (0..nb_transactions_per_block)
                    .map(|ti| format!("tx_hash-b{i}t{ti}"))
                    .collect(),
            )
        })
        .collect()
}

async fn init_db(
    nb_blocks_in_db: usize,
    nb_transactions_per_block: usize,
) -> AggregatorCardanoChainDataRepository {
    println!(
        "Generating a db with {nb_blocks_in_db} blocks, {nb_transactions_per_block} transactions per block ..."
    );
    let start_instant = tokio::time::Instant::now();

    let transactions = generate_blocks_with_transaction(nb_blocks_in_db, nb_transactions_per_block);
    let connection = cardano_tx_db_connection(&format!(
        "cardano_blk-{nb_blocks_in_db}-tx-{nb_transactions_per_block}.db",
    ));
    let repository = AggregatorCardanoChainDataRepository::new(Arc::new(
        SqliteConnectionPool::build_from_connection(connection),
    ));
    repository.store_blocks_and_transactions(transactions).await.unwrap();

    let generation_duration = start_instant.elapsed();
    println!(
        "Database generated in {}s:{:04}ms",
        generation_duration.as_secs(),
        generation_duration.subsec_millis()
    );

    repository
}

fn run_bench(c: &mut Criterion, nb_blocks_in_db: usize, nb_transactions_per_block: usize) {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let repository =
        runtime.block_on(async { init_db(nb_blocks_in_db, nb_transactions_per_block).await });

    let mut group = c.benchmark_group(format!(
        "Get blocks with transactions - {nb_blocks_in_db} blocks/{nb_transactions_per_block} tx per block"
    ));
    for max_block_number in [100, 10_000, 100_000, 1_000_000] {
        group.bench_with_input(
            BenchmarkId::from_parameter(format!("range(0..{max_block_number})")),
            &max_block_number,
            |b, &max_block_number| {
                b.to_async(&runtime).iter(|| async {
                    let _blocks_with_tx = repository
                        .get_blocks_with_transactions_in_range_blocks(
                            BlockNumber(0)..BlockNumber(max_block_number),
                        )
                        .await
                        .unwrap();
                });
            },
        );
    }
    group.finish();
}

fn bench_get_blocks_with_transactions(c: &mut Criterion) {
    // Four rounds of benchmarks:
    // - one with a db of 1M blocks, each block with 1 transaction
    // - one with a db of 1M blocks, each block with 20 transactions
    // - one with a db of 10M blocks, each block with 1 transaction
    // - one with a db of 10M blocks, each block with 20 transactions
    // Each time the number of blocks with transactions to read is 100, 10_000, 100_000, 1_000_000.
    run_bench(c, 1_000_000, 1);
    run_bench(c, 1_000_000, 20);
    run_bench(c, 10_000_000, 1);
    run_bench(c, 10_000_000, 20);
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(20);
    targets = bench_get_blocks_with_transactions
}
criterion_main!(benches);
