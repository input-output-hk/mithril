use std::sync::Arc;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use sqlite::ConnectionThreadSafe;

use mithril_aggregator::services::TransactionStore;
use mithril_common::{entities::CardanoTransaction, test_utils::TempDir};
use mithril_persistence::database::repository::CardanoTransactionRepository;
use mithril_persistence::sqlite::{ConnectionBuilder, SqliteConnectionPool};

fn cardano_tx_db_connection(db_file_name: &str) -> ConnectionThreadSafe {
    let db_path =
        TempDir::create("aggregator_benches", "bench_get_transactions").join(db_file_name);

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

fn generate_transactions(nb_transactions: usize) -> Vec<CardanoTransaction> {
    // Note: we irrealistically generate transactions where each are on a different block.
    // This is to trick the repository `get_transactions_in_range` method to read the expected number
    // of transactions.
    (0..nb_transactions)
        .map(|i| {
            CardanoTransaction::new(
                format!("tx_hash-{}", i),
                i as u64,
                i as u64 * 100,
                format!("block_hash-{}", i),
                i as u64 + 1,
            )
        })
        .collect()
}

async fn init_db(nb_transaction_in_db: usize) -> CardanoTransactionRepository {
    println!("Generating a db with {nb_transaction_in_db} transactions, one per block ...");
    let transactions = generate_transactions(nb_transaction_in_db);
    let connection = cardano_tx_db_connection(&format!("cardano_tx-{nb_transaction_in_db}.db",));
    let repository = CardanoTransactionRepository::new(Arc::new(
        SqliteConnectionPool::build_from_connection(connection),
    ));
    repository.store_transactions(transactions).await.unwrap();

    repository
}

fn run_bench(c: &mut Criterion, nb_transaction_in_db: usize) {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let repository = runtime.block_on(async { init_db(nb_transaction_in_db).await });

    let mut group = c.benchmark_group(format!(
        "Get transactions - {nb_transaction_in_db} tx in db"
    ));
    for max_block_number in [100, 10_000, 100_000, 1_000_000] {
        group.bench_with_input(
            BenchmarkId::from_parameter(format!(
                "get_transactions_in_range(0..{max_block_number})"
            )),
            &max_block_number,
            |b, &max_block_number| {
                b.to_async(&runtime).iter(|| async {
                    let _transactions = repository
                        .get_transactions_in_range(0..max_block_number)
                        .await
                        .unwrap();
                });
            },
        );
    }
    group.finish();
}

fn bench_get_transactions(c: &mut Criterion) {
    // Two rounds of benchmarks: one with 1M transactions in the db, and one with 10M transactions.
    // Each time the number of transactions to read is 100, 10_000, 100_000, 1_000_000.
    run_bench(c, 1_000_000);
    run_bench(c, 10_000_000);
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(20);
    targets = bench_get_transactions
}
criterion_main!(benches);
