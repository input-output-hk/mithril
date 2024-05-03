use criterion::{criterion_group, criterion_main, Criterion};
use sqlite::ConnectionThreadSafe;
use std::sync::Arc;

use mithril_aggregator::{
    database::repository::CardanoTransactionRepository, services::TransactionStore,
};
use mithril_common::{entities::CardanoTransaction, test_utils::TempDir};
use mithril_persistence::sqlite::ConnectionBuilder;

fn cardano_tx_db_connection() -> ConnectionThreadSafe {
    let db_path =
        TempDir::create("aggregator_benches", "bench_store_transactions").join("cardano_tx.db");

    if db_path.exists() {
        std::fs::remove_file(db_path.clone()).unwrap();
    }

    ConnectionBuilder::open_file(&db_path)
        .with_migrations(
            mithril_aggregator::database::cardano_transaction_migration::get_migrations(),
        )
        .build()
        .unwrap()
}

fn generate_transactions(nb_transactions: usize) -> Vec<CardanoTransaction> {
    (0..nb_transactions)
        .map(|i| {
            CardanoTransaction::new(
                format!("tx_hash-{}", i),
                i as u64,
                i as u64 + 1,
                format!("block_hash-{}", i),
                i as u64 + 2,
            )
        })
        .collect()
}

fn bench_store_transactions(c: &mut Criterion) {
    const NB_CARDANO_TRANSACTIONS: usize = 1_000_000;
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let transactions = generate_transactions(NB_CARDANO_TRANSACTIONS);

    let mut group = c.benchmark_group("Store transactions");
    group.bench_function("store_transactions", |bencher| {
        bencher.to_async(&runtime).iter(|| async {
            let connection = Arc::new(cardano_tx_db_connection());
            let repository = CardanoTransactionRepository::new(connection);
            repository.store_transactions(transactions.clone()).await
        });
    });

    group.finish();
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = bench_store_transactions
}
criterion_main!(benches);
