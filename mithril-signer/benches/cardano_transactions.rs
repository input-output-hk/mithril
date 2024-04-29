use std::{path::Path, sync::Arc};

use criterion::{criterion_group, criterion_main, Criterion};
use mithril_common::{entities::CardanoTransaction, StdResult};
use mithril_persistence::sqlite::ConnectionBuilder;
use mithril_signer::{database::repository::CardanoTransactionRepository, TransactionStore};
use sqlite::ConnectionThreadSafe;

fn cardano_tx_db_connection() -> StdResult<ConnectionThreadSafe> {
    let db_path = Path::new("./cardano_tx.db");

    if db_path.exists() {
        std::fs::remove_file(db_path)?;
    }

    let connection = ConnectionBuilder::open_file(db_path)
        .with_migrations(mithril_signer::database::cardano_transaction_migration::get_migrations())
        .build()?;
    Ok(connection)
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

fn bench_store_transactions_transactions(c: &mut Criterion) {
    const NB_CARDANO_TRANSACTIONS: usize = 100000;
    let runtime = tokio::runtime::Runtime::new().unwrap();

    let mut group = c.benchmark_group("Create transactions");

    group.bench_function("store_transactions", |bencher| {
        bencher.to_async(&runtime).iter(|| async {
            let connection = Arc::new(cardano_tx_db_connection().unwrap());
            let repository = CardanoTransactionRepository::new(connection);
            repository
                .store_transactions(generate_transactions(NB_CARDANO_TRANSACTIONS))
                .await
        });
    });

    for chunks_size in [10, 50, 100, 200] {
        group.bench_function(
            format!("store_transactions_with_chunks_size = {}", chunks_size),
            |bencher| {
                bencher.to_async(&runtime).iter(|| async {
                    let connection = Arc::new(cardano_tx_db_connection().unwrap());
                    let repository = CardanoTransactionRepository::new(connection.clone());
                    repository
                        .store_transactions_with_chunks_size(
                            generate_transactions(NB_CARDANO_TRANSACTIONS),
                            chunks_size,
                        )
                        .await
                });
            },
        );
    }

    for chunks_size in [10, 50, 100, 200] {
        group.bench_function(
            format!(
                "store_transactions_with_chunks_size_and_collect = {}",
                chunks_size
            ),
            |bencher| {
                bencher.to_async(&runtime).iter(|| async {
                    let connection = Arc::new(cardano_tx_db_connection().unwrap());
                    let repository = CardanoTransactionRepository::new(connection.clone());
                    repository
                        .store_transactions_with_chunks_size_and_collect(
                            generate_transactions(NB_CARDANO_TRANSACTIONS),
                            chunks_size,
                        )
                        .await
                });
            },
        );
    }

    group.finish();
}

criterion_group!(benches, bench_store_transactions_transactions);
criterion_main!(benches);
