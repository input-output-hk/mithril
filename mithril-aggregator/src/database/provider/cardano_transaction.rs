use mithril_common::{
    entities::{Beacon, BlockNumber, CardanoTransaction, ImmutableFileNumber, TransactionHash},
    signable_builder::TransactionStore,
    sqlite::{
        HydrationError, Projection, Provider, SourceAlias, SqLiteEntity, SqliteConnection,
        WhereCondition,
    },
    StdResult,
};

use anyhow::Context;
use async_trait::async_trait;
use sqlite::{Row, Value};
use std::{iter::repeat, sync::Arc};

use crate::services::TransactionsRetriever;

/// Cardano Transaction record is the representation of a cardano transaction.
#[derive(Debug, PartialEq, Clone)]
pub struct CardanoTransactionRecord {
    /// Unique hash of the transaction
    pub transaction_hash: TransactionHash,

    /// Block number of the transaction
    pub block_number: BlockNumber,

    /// Immutable file number of the transaction
    pub immutable_file_number: ImmutableFileNumber,
}

impl From<CardanoTransaction> for CardanoTransactionRecord {
    fn from(transaction: CardanoTransaction) -> Self {
        Self {
            transaction_hash: transaction.transaction_hash,
            block_number: transaction.block_number,
            immutable_file_number: transaction.immutable_file_number,
        }
    }
}

impl From<CardanoTransactionRecord> for CardanoTransaction {
    fn from(other: CardanoTransactionRecord) -> CardanoTransaction {
        CardanoTransaction {
            transaction_hash: other.transaction_hash,
            block_number: other.block_number,
            immutable_file_number: other.immutable_file_number,
        }
    }
}

impl SqLiteEntity for CardanoTransactionRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let transaction_hash = row.read::<&str, _>(0);
        let block_number = row.read::<i64, _>(1);
        let block_number = u64::try_from(block_number)
            .map_err(|e| HydrationError::InvalidData(format!("Integer field cardano_tx.block_number (value={block_number}) is incompatible with u64 representation. Error = {e}")))?;
        let immutable_file_number = row.read::<i64, _>(2);
        let immutable_file_number = u64::try_from(immutable_file_number)
            .map_err(|e| HydrationError::InvalidData(format!("Integer field cardano_tx.immutable_file_number (value={immutable_file_number}) is incompatible with u64 representation. Error = {e}")))?;

        Ok(Self {
            transaction_hash: transaction_hash.to_string(),
            block_number,
            immutable_file_number,
        })
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            (
                "transaction_hash",
                "{:cardano_tx:}.transaction_hash",
                "text",
            ),
            ("block_number", "{:cardano_tx:}.block_number", "int"),
            (
                "immutable_file_number",
                "{:cardano_tx:}.immutable_file_number",
                "int",
            ),
        ])
    }
}

struct CardanoTransactionProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> CardanoTransactionProvider<'client> {
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    // Useful in test and probably in the future.
    #[allow(dead_code)]
    fn get_transaction_hash_condition(&self, transaction_hash: &TransactionHash) -> WhereCondition {
        WhereCondition::new(
            "transaction_hash = ?*",
            vec![Value::String(transaction_hash.to_owned())],
        )
    }

    pub(crate) fn get_transaction_up_to_beacon_condition(&self, beacon: &Beacon) -> WhereCondition {
        WhereCondition::new(
            "immutable_file_number <= ?*",
            vec![Value::Integer(beacon.immutable_file_number as i64)],
        )
    }
}

impl<'client> Provider<'client> for CardanoTransactionProvider<'client> {
    type Entity = CardanoTransactionRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!(
            "select {projection} from cardano_tx where {condition} order by transaction_hash desc"
        )
    }
}

struct InsertCardanoTransactionProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> InsertCardanoTransactionProvider<'client> {
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    fn get_insert_condition(&self, record: &CardanoTransactionRecord) -> StdResult<WhereCondition> {
        let expression =
            "(transaction_hash, block_number, immutable_file_number) values (?1, ?2, ?3)";
        let parameters = vec![
            Value::String(record.transaction_hash.clone()),
            Value::Integer(record.block_number.try_into()?),
            Value::Integer(record.immutable_file_number.try_into()?),
        ];

        Ok(WhereCondition::new(expression, parameters))
    }

    fn get_insert_many_condition(
        &self,
        transactions_records: Vec<CardanoTransactionRecord>,
    ) -> WhereCondition {
        let columns = "(transaction_hash, block_number, immutable_file_number)";
        let values_columns: Vec<&str> = repeat("(?*, ?*, ?*)")
            .take(transactions_records.len())
            .collect();

        let values: Vec<Value> = transactions_records
            .into_iter()
            .flat_map(|record| {
                vec![
                    Value::String(record.transaction_hash),
                    Value::Integer(record.block_number.try_into().unwrap()),
                    Value::Integer(record.immutable_file_number.try_into().unwrap()),
                ]
            })
            .collect();

        WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values,
        )
    }
}

impl<'client> Provider<'client> for InsertCardanoTransactionProvider<'client> {
    type Entity = CardanoTransactionRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert or ignore into cardano_tx {condition} returning {projection}")
    }
}

/// ## Cardano transaction repository
///
/// This is a business oriented layer to perform actions on the database through
/// providers.
pub struct CardanoTransactionRepository {
    connection: Arc<SqliteConnection>,
}

impl CardanoTransactionRepository {
    /// Instantiate service
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }

    /// Return all the [CardanoTransactionRecord]s in the database.
    pub async fn get_all_transactions(&self) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = CardanoTransactionProvider::new(&self.connection);
        let filters = WhereCondition::default();
        let transactions = provider.find(filters)?;

        Ok(transactions.collect())
    }

    /// Return all the [CardanoTransactionRecord]s in the database up to the given beacon.
    pub async fn get_transactions_up_to(
        &self,
        beacon: &Beacon,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = CardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_transaction_up_to_beacon_condition(beacon);
        let transactions = provider.find(filters)?;

        Ok(transactions.collect())
    }

    /// Return the [CardanoTransactionRecord] for the given transaction hash.
    pub async fn get_transaction(
        &self,
        transaction_hash: &TransactionHash,
    ) -> StdResult<Option<CardanoTransactionRecord>> {
        let provider = CardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_transaction_hash_condition(transaction_hash);
        let mut transactions = provider.find(filters)?;

        Ok(transactions.next())
    }

    /// Create a new [CardanoTransactionRecord] in the database.
    pub async fn create_transaction(
        &self,
        transaction_hash: &TransactionHash,
        block_number: BlockNumber,
        immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<Option<CardanoTransactionRecord>> {
        let provider = InsertCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_insert_condition(&CardanoTransactionRecord {
            transaction_hash: transaction_hash.to_owned(),
            block_number,
            immutable_file_number,
        })?;
        let mut cursor = provider.find(filters)?;

        Ok(cursor.next())
    }

    /// Create new [CardanoTransactionRecord]s in the database.
    pub async fn create_transactions(
        &self,
        transactions: Vec<CardanoTransactionRecord>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = InsertCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_insert_many_condition(transactions);
        let cursor = provider.find(filters)?;

        Ok(cursor.collect())
    }
}

#[async_trait]
impl TransactionStore for CardanoTransactionRepository {
    async fn store_transactions(&self, transactions: &[CardanoTransaction]) -> StdResult<()> {
        let records: Vec<CardanoTransactionRecord> =
            transactions.iter().map(|tx| tx.to_owned().into()).collect();
        self.create_transactions(records)
            .await
            .with_context(|| "CardanoTransactionRepository can not store transactions")?;

        Ok(())
    }
}

#[async_trait]
impl TransactionsRetriever for CardanoTransactionRepository {
    async fn get_up_to(&self, beacon: &Beacon) -> StdResult<Vec<CardanoTransaction>> {
        self.get_transactions_up_to(beacon).await.map(|v| {
            v.into_iter()
                .map(|record| record.into())
                .collect::<Vec<CardanoTransaction>>()
        })
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::sqlite::SourceAlias;
    use sqlite::Connection;

    use crate::{dependency_injection::DependenciesBuilder, Configuration};

    use super::*;

    async fn get_connection() -> Arc<SqliteConnection> {
        let config = Configuration::new_sample();
        let mut builder = DependenciesBuilder::new(config);
        builder
            .get_sqlite_connection_cardano_transaction()
            .await
            .unwrap()
    }

    #[test]
    fn cardano_transaction_projection() {
        let projection = CardanoTransactionRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]);

        assert_eq!(
            "cardano_tx.transaction_hash as transaction_hash, cardano_tx.block_number as block_number, cardano_tx.immutable_file_number as immutable_file_number".to_string(),
            projection.expand(aliases)
        )
    }

    #[test]
    fn provider_transaction_hash_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = CardanoTransactionProvider::new(&connection);
        let (expr, params) = provider
            .get_transaction_hash_condition(
                &"0405a78c637f5c637e3146e293c0045ea80a07fac8f245901e7b491182931650".to_string(),
            )
            .expand();

        assert_eq!("transaction_hash = ?1".to_string(), expr);
        assert_eq!(
            vec![Value::String(
                "0405a78c637f5c637e3146e293c0045ea80a07fac8f245901e7b491182931650".to_string()
            )],
            params,
        );
    }

    #[test]
    fn provider_transaction_up_to_beacon_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = CardanoTransactionProvider::new(&connection);
        let (expr, params) = provider
            .get_transaction_up_to_beacon_condition(&Beacon {
                immutable_file_number: 2309,
                ..Beacon::default()
            })
            .expand();

        assert_eq!("immutable_file_number <= ?1".to_string(), expr);
        assert_eq!(vec![Value::Integer(2309)], params,);
    }

    #[test]
    fn insert_provider_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = InsertCardanoTransactionProvider::new(&connection);
        let (expr, params) = provider
            .get_insert_condition(&CardanoTransactionRecord {
                transaction_hash:
                    "0405a78c637f5c637e3146e293c0045ea80a07fac8f245901e7b491182931650".to_string(),
                block_number: 10,
                immutable_file_number: 99,
            })
            .unwrap()
            .expand();

        assert_eq!(
            "(transaction_hash, block_number, immutable_file_number) values (?1, ?2, ?3)"
                .to_string(),
            expr
        );
        assert_eq!(
            vec![
                Value::String(
                    "0405a78c637f5c637e3146e293c0045ea80a07fac8f245901e7b491182931650".to_string()
                ),
                Value::Integer(10),
                Value::Integer(99)
            ],
            params
        );
    }

    #[test]
    fn insert_provider_many_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = InsertCardanoTransactionProvider::new(&connection);
        let (expr, params) = provider
            .get_insert_many_condition(vec![
                CardanoTransactionRecord {
                    transaction_hash: "tx-hash-123".to_string(),
                    block_number: 10,
                    immutable_file_number: 99,
                },
                CardanoTransactionRecord {
                    transaction_hash: "tx-hash-456".to_string(),
                    block_number: 11,
                    immutable_file_number: 100,
                },
            ])
            .expand();

        assert_eq!(
            "(transaction_hash, block_number, immutable_file_number) values (?1, ?2, ?3), (?4, ?5, ?6)"
                .to_string(),
            expr
        );
        assert_eq!(
            vec![
                Value::String("tx-hash-123".to_string()),
                Value::Integer(10),
                Value::Integer(99),
                Value::String("tx-hash-456".to_string()),
                Value::Integer(11),
                Value::Integer(100)
            ],
            params
        );
    }

    #[tokio::test]
    async fn repository_create_and_get_transaction() {
        let connection = get_connection().await;
        let repository = CardanoTransactionRepository::new(connection.clone());
        repository
            .create_transaction(&"tx-hash-123".to_string(), 10, 99)
            .await
            .unwrap();
        repository
            .create_transaction(&"tx-hash-456".to_string(), 11, 100)
            .await
            .unwrap();
        let transaction_result = repository
            .get_transaction(&"tx-hash-123".to_string())
            .await
            .unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: 10,
                immutable_file_number: 99
            }),
            transaction_result
        );
    }

    #[tokio::test]
    async fn repository_create_ignore_further_transactions_when_exists() {
        let connection = get_connection().await;
        let repository = CardanoTransactionRepository::new(connection.clone());
        repository
            .create_transaction(&"tx-hash-123".to_string(), 10, 99)
            .await
            .unwrap();
        repository
            .create_transaction(&"tx-hash-123".to_string(), 11, 100)
            .await
            .unwrap();
        let transaction_result = repository
            .get_transaction(&"tx-hash-123".to_string())
            .await
            .unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: 10,
                immutable_file_number: 99
            }),
            transaction_result
        );
    }

    #[tokio::test]
    async fn repository_store_transactions_and_get_stored_transactions() {
        let connection = get_connection().await;
        let repository = CardanoTransactionRepository::new(connection.clone());

        let cardano_transactions = vec![
            CardanoTransaction {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: 10,
                immutable_file_number: 99,
            },
            CardanoTransaction {
                transaction_hash: "tx-hash-456".to_string(),
                block_number: 11,
                immutable_file_number: 100,
            },
        ];
        repository
            .store_transactions(&cardano_transactions)
            .await
            .unwrap();

        let transaction_result = repository
            .get_transaction(&"tx-hash-123".to_string())
            .await
            .unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: 10,
                immutable_file_number: 99
            }),
            transaction_result
        );

        let transaction_result = repository
            .get_transaction(&"tx-hash-456".to_string())
            .await
            .unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-456".to_string(),
                block_number: 11,
                immutable_file_number: 100,
            }),
            transaction_result
        );
    }

    #[tokio::test]
    async fn repository_store_transactions_and_get_up_to_beacon_transactions() {
        let connection = get_connection().await;
        let repository = CardanoTransactionRepository::new(connection.clone());

        let cardano_transactions: Vec<CardanoTransaction> = (20..=40)
            .map(|i| CardanoTransaction {
                transaction_hash: format!("tx-hash-{i}"),
                block_number: i % 10,
                immutable_file_number: i,
            })
            .collect();
        repository
            .store_transactions(&cardano_transactions)
            .await
            .unwrap();

        let transaction_result = repository
            .get_up_to(&Beacon::new("".to_string(), 1, 34))
            .await
            .unwrap();

        assert_eq!(
            cardano_transactions[0..=14]
                .iter()
                .cloned()
                .rev()
                .collect::<Vec<_>>(),
            transaction_result
        );

        let transaction_result = repository
            .get_up_to(&Beacon::new("".to_string(), 1, 300))
            .await
            .unwrap();

        assert_eq!(
            cardano_transactions.into_iter().rev().collect::<Vec<_>>(),
            transaction_result
        );

        let transaction_result = repository
            .get_up_to(&Beacon::new("".to_string(), 1, 19))
            .await
            .unwrap();

        assert_eq!(Vec::<CardanoTransaction>::new(), transaction_result);
    }

    #[tokio::test]
    async fn repository_store_transactions_and_get_all_stored_transactions() {
        let connection = get_connection().await;
        let repository = CardanoTransactionRepository::new(connection.clone());

        let cardano_transactions = vec![
            CardanoTransaction {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: 10,
                immutable_file_number: 99,
            },
            CardanoTransaction {
                transaction_hash: "tx-hash-456".to_string(),
                block_number: 11,
                immutable_file_number: 100,
            },
        ];
        repository
            .store_transactions(&cardano_transactions)
            .await
            .unwrap();

        let transactions_result = repository.get_all_transactions().await.unwrap();
        let transactions_expected: Vec<CardanoTransactionRecord> = cardano_transactions
            .iter()
            .rev()
            .map(|tx| tx.clone().into())
            .collect();

        assert_eq!(transactions_expected, transactions_result);
    }

    #[tokio::test]
    async fn repository_store_transactions_doesnt_erase_existing_data() {
        let connection = get_connection().await;
        let repository = CardanoTransactionRepository::new(connection.clone());

        repository
            .create_transaction(&"tx-hash-000".to_string(), 1, 9)
            .await
            .unwrap();

        let cardano_transactions = vec![CardanoTransaction {
            transaction_hash: "tx-hash-123".to_string(),
            block_number: 10,
            immutable_file_number: 99,
        }];
        repository
            .store_transactions(&cardano_transactions)
            .await
            .unwrap();

        let transaction_result = repository
            .get_transaction(&"tx-hash-000".to_string())
            .await
            .unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-000".to_string(),
                block_number: 1,
                immutable_file_number: 9
            }),
            transaction_result
        );
    }
}
