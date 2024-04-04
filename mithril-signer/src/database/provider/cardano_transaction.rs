use crate::TransactionStore;
use mithril_common::{
    entities::{
        BlockHash, BlockNumber, CardanoTransaction, ImmutableFileNumber, SlotNumber,
        TransactionHash,
    },
    StdResult,
};
use mithril_persistence::sqlite::{
    HydrationError, Projection, Provider, SourceAlias, SqLiteEntity, SqliteConnection,
    WhereCondition,
};

use anyhow::Context;
use async_trait::async_trait;
use sqlite::{Row, Value};
use std::{iter::repeat, sync::Arc};

/// Cardano Transaction record is the representation of a cardano transaction.
#[derive(Debug, PartialEq, Clone)]
pub struct CardanoTransactionRecord {
    /// Unique hash of the transaction
    pub transaction_hash: TransactionHash,

    /// Block number of the transaction
    pub block_number: BlockNumber,

    /// Slot number of the transaction
    pub slot_number: SlotNumber,

    /// Block hash of the transaction
    pub block_hash: BlockHash,

    /// Immutable file number of the transaction
    pub immutable_file_number: ImmutableFileNumber,
}

impl From<CardanoTransaction> for CardanoTransactionRecord {
    fn from(transaction: CardanoTransaction) -> Self {
        Self {
            transaction_hash: transaction.transaction_hash,
            block_number: transaction.block_number,
            slot_number: transaction.slot_number,
            block_hash: transaction.block_hash,
            immutable_file_number: transaction.immutable_file_number,
        }
    }
}

impl From<CardanoTransactionRecord> for CardanoTransaction {
    fn from(other: CardanoTransactionRecord) -> CardanoTransaction {
        CardanoTransaction {
            transaction_hash: other.transaction_hash,
            block_number: other.block_number,
            slot_number: other.slot_number,
            block_hash: other.block_hash,
            immutable_file_number: other.immutable_file_number,
        }
    }
}

impl SqLiteEntity for CardanoTransactionRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        // TODO: generalize this method to other hydrator
        fn try_to_u64(field: &str, value: i64) -> Result<u64, HydrationError> {
            u64::try_from(value)
            .map_err(|e| HydrationError::InvalidData(format!("Integer field cardano_tx.{field} (value={value}) is incompatible with u64 representation. Error = {e}")))
        }

        let transaction_hash = row.read::<&str, _>(0);
        let block_number = try_to_u64("block_number", row.read::<i64, _>(1))?;
        let slot_number = try_to_u64("slot_number", row.read::<i64, _>(2))?;
        let block_hash = row.read::<&str, _>(3);
        let immutable_file_number = try_to_u64("immutable_file_number", row.read::<i64, _>(4))?;

        Ok(Self {
            transaction_hash: transaction_hash.to_string(),
            block_number,
            slot_number,
            block_hash: block_hash.to_string(),
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
            ("slot_number", "{:cardano_tx:}.slot_number", "int"),
            ("block_hash", "{:cardano_tx:}.block_hash", "text"),
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

    pub(crate) fn get_transaction_up_to_beacon_condition(
        &self,
        beacon: ImmutableFileNumber,
    ) -> WhereCondition {
        WhereCondition::new(
            "immutable_file_number <= ?*",
            vec![Value::Integer(beacon as i64)],
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

        format!("select {projection} from cardano_tx where {condition} order by rowid")
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
        self.get_insert_many_condition(vec![record.clone()])
    }

    fn get_insert_many_condition(
        &self,
        transactions_records: Vec<CardanoTransactionRecord>,
    ) -> StdResult<WhereCondition> {
        let columns =
            "(transaction_hash, block_number, slot_number, block_hash, immutable_file_number)";
        let values_columns: Vec<&str> = repeat("(?*, ?*, ?*, ?*, ?*)")
            .take(transactions_records.len())
            .collect();

        let values: StdResult<Vec<Value>> =
            transactions_records
                .into_iter()
                .try_fold(vec![], |mut vec, record| {
                    vec.append(&mut vec![
                        Value::String(record.transaction_hash),
                        Value::Integer(record.block_number.try_into()?),
                        Value::Integer(record.slot_number.try_into()?),
                        Value::String(record.block_hash.clone()),
                        Value::Integer(record.immutable_file_number.try_into()?),
                    ]);
                    Ok(vec)
                });

        Ok(WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values?,
        ))
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

    /// Return all the [CardanoTransactionRecord]s in the database using chronological order.
    pub async fn get_all_transactions(&self) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = CardanoTransactionProvider::new(&self.connection);
        let filters = WhereCondition::default();
        let transactions = provider.find(filters)?;

        Ok(transactions.collect())
    }

    /// Return all the [CardanoTransactionRecord]s in the database up to the given beacon using
    /// chronological order.
    pub async fn get_transactions_up_to(
        &self,
        beacon: ImmutableFileNumber,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = CardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_transaction_up_to_beacon_condition(beacon);
        let transactions = provider.find(filters)?;

        Ok(transactions.collect())
    }

    /// Return the [CardanoTransactionRecord] for the given transaction hash.
    pub async fn get_transaction<T: Into<TransactionHash>>(
        &self,
        transaction_hash: T,
    ) -> StdResult<Option<CardanoTransactionRecord>> {
        let provider = CardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_transaction_hash_condition(&transaction_hash.into());
        let mut transactions = provider.find(filters)?;

        Ok(transactions.next())
    }

    /// Create a new [CardanoTransactionRecord] in the database.
    pub async fn create_transaction<T: Into<TransactionHash>, U: Into<BlockHash>>(
        &self,
        transaction_hash: T,
        block_number: BlockNumber,
        slot_number: SlotNumber,
        block_hash: U,
        immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<Option<CardanoTransactionRecord>> {
        let provider = InsertCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_insert_condition(&CardanoTransactionRecord {
            transaction_hash: transaction_hash.into(),
            block_number,
            slot_number,
            block_hash: block_hash.into(),
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
        let filters = provider.get_insert_many_condition(transactions)?;
        let cursor = provider.find(filters)?;

        Ok(cursor.collect())
    }
}

#[async_trait]
impl TransactionStore for CardanoTransactionRepository {
    async fn get_highest_beacon(&self) -> StdResult<Option<ImmutableFileNumber>> {
        let sql = "select max(immutable_file_number) as highest from cardano_tx;";
        match self
            .connection
            .prepare(sql)
            .with_context(|| {
                format!(
                    "Prepare query error: SQL=`{}`",
                    &sql.replace('\n', " ").trim()
                )
            })?
            .iter()
            .next()
        {
            None => Ok(None),
            Some(row) => {
                let highest = row?.read::<Option<i64>, _>(0);
                highest
                    .map(u64::try_from)
                    .transpose()
                    .with_context(||
                        format!("Integer field max(immutable_file_number) (value={highest:?}) is incompatible with u64 representation.")
                    )
            }
        }
    }

    async fn get_up_to(&self, beacon: ImmutableFileNumber) -> StdResult<Vec<CardanoTransaction>> {
        self.get_transactions_up_to(beacon).await.map(|v| {
            v.into_iter()
                .map(|record| record.into())
                .collect::<Vec<CardanoTransaction>>()
        })
    }

    async fn store_transactions(&self, transactions: &[CardanoTransaction]) -> StdResult<()> {
        let records: Vec<CardanoTransactionRecord> =
            transactions.iter().map(|tx| tx.to_owned().into()).collect();
        self.create_transactions(records)
            .await
            .with_context(|| "CardanoTransactionRepository can not store transactions")?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mithril_persistence::sqlite::SourceAlias;
    use sqlite::Connection;

    use crate::{Configuration, ProductionServiceBuilder};

    use super::*;

    async fn get_connection() -> Arc<SqliteConnection> {
        let party_id = "party-id-123".to_string();
        let configuration = Configuration::new_sample(&party_id);
        let production_service_builder = ProductionServiceBuilder::new(&configuration);
        production_service_builder
            .build_sqlite_connection(
                ":memory:",
                crate::database::cardano_transaction_migration::get_migrations(),
            )
            .await
            .unwrap()
    }

    #[test]
    fn cardano_transaction_projection() {
        let projection = CardanoTransactionRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]);

        assert_eq!(
            "cardano_tx.transaction_hash as transaction_hash, cardano_tx.block_number as block_number, cardano_tx.slot_number as slot_number, cardano_tx.block_hash as block_hash, cardano_tx.immutable_file_number as immutable_file_number".to_string(),
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
            .get_transaction_up_to_beacon_condition(2309)
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
                slot_number: 50,
                block_hash: "block_hash".to_string(),
                immutable_file_number: 99,
            })
            .unwrap()
            .expand();

        assert_eq!(
            "(transaction_hash, block_number, slot_number, block_hash, immutable_file_number) values (?1, ?2, ?3, ?4, ?5)"
                .to_string(),
            expr
        );
        assert_eq!(
            vec![
                Value::String(
                    "0405a78c637f5c637e3146e293c0045ea80a07fac8f245901e7b491182931650".to_string()
                ),
                Value::Integer(10),
                Value::Integer(50),
                Value::String("block_hash".to_string()),
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
                    transaction_hash: "tx_hash-123".to_string(),
                    block_number: 10,
                    slot_number: 50,
                    block_hash: "block_hash-123".to_string(),
                    immutable_file_number: 99,
                },
                CardanoTransactionRecord {
                    transaction_hash: "tx_hash-456".to_string(),
                    block_number: 11,
                    slot_number: 51,
                    block_hash: "block_hash-456".to_string(),
                    immutable_file_number: 100,
                },
            ])
            .unwrap()
            .expand();

        assert_eq!(
            "(transaction_hash, block_number, slot_number, block_hash, immutable_file_number) values (?1, ?2, ?3, ?4, ?5), (?6, ?7, ?8, ?9, ?10)"
                .to_string(),
            expr
        );
        assert_eq!(
            vec![
                Value::String("tx_hash-123".to_string()),
                Value::Integer(10),
                Value::Integer(50),
                Value::String("block_hash-123".to_string()),
                Value::Integer(99),
                Value::String("tx_hash-456".to_string()),
                Value::Integer(11),
                Value::Integer(51),
                Value::String("block_hash-456".to_string()),
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
            .create_transaction("tx-hash-123", 10, 50, "block_hash-123", 99)
            .await
            .unwrap();
        repository
            .create_transaction("tx-hash-456", 11, 51, "block_hash-456", 100)
            .await
            .unwrap();
        let transaction_result = repository.get_transaction("tx-hash-123").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: 10,
                slot_number: 50,
                block_hash: "block_hash-123".to_string(),
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
            .create_transaction("tx-hash-123", 10, 50, "block_hash-123", 99)
            .await
            .unwrap();
        repository
            .create_transaction("tx-hash-123", 11, 51, "block_hash-123-bis", 100)
            .await
            .unwrap();
        let transaction_result = repository.get_transaction("tx-hash-123").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: 10,
                slot_number: 50,
                block_hash: "block_hash-123".to_string(),
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
            CardanoTransaction::new("tx-hash-123", 10, 50, "block-hash-123", 99),
            CardanoTransaction::new("tx-hash-456", 11, 51, "block-hash-456", 100),
        ];
        repository
            .store_transactions(&cardano_transactions)
            .await
            .unwrap();

        let transaction_result = repository.get_transaction("tx-hash-123").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: 10,
                slot_number: 50,
                block_hash: "block-hash-123".to_string(),
                immutable_file_number: 99
            }),
            transaction_result
        );

        let transaction_result = repository.get_transaction("tx-hash-456").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-456".to_string(),
                block_number: 11,
                slot_number: 51,
                block_hash: "block-hash-456".to_string(),
                immutable_file_number: 100,
            }),
            transaction_result
        );
    }

    #[tokio::test]
    async fn repository_get_up_to_beacon_transactions() {
        let connection = get_connection().await;
        let repository = CardanoTransactionRepository::new(connection.clone());

        let cardano_transactions: Vec<CardanoTransaction> = (20..=40)
            .map(|i| CardanoTransaction {
                transaction_hash: format!("tx-hash-{i}"),
                block_number: i % 10,
                slot_number: i * 100,
                block_hash: format!("block-hash-{i}"),
                immutable_file_number: i,
            })
            .collect();
        repository
            .store_transactions(&cardano_transactions)
            .await
            .unwrap();

        let transaction_result = repository.get_up_to(34).await.unwrap();

        assert_eq!(cardano_transactions[0..=14].to_vec(), transaction_result);

        let transaction_result = repository.get_up_to(300).await.unwrap();

        assert_eq!(
            cardano_transactions.into_iter().collect::<Vec<_>>(),
            transaction_result
        );

        let transaction_result = repository.get_up_to(19).await.unwrap();

        assert_eq!(Vec::<CardanoTransaction>::new(), transaction_result);
    }

    #[tokio::test]
    async fn repository_get_all_stored_transactions() {
        let connection = get_connection().await;
        let repository = CardanoTransactionRepository::new(connection.clone());

        let cardano_transactions = vec![
            CardanoTransaction::new("tx-hash-123".to_string(), 10, 50, "block-hash-123", 99),
            CardanoTransaction::new("tx-hash-456".to_string(), 11, 51, "block-hash-456", 100),
        ];
        repository
            .store_transactions(&cardano_transactions)
            .await
            .unwrap();

        let transactions_result = repository.get_all_transactions().await.unwrap();
        let transactions_expected: Vec<CardanoTransactionRecord> = cardano_transactions
            .iter()
            .map(|tx| tx.clone().into())
            .collect();

        assert_eq!(transactions_expected, transactions_result);
    }

    #[tokio::test]
    async fn repository_store_transactions_doesnt_erase_existing_data() {
        let connection = get_connection().await;
        let repository = CardanoTransactionRepository::new(connection.clone());

        repository
            .create_transaction("tx-hash-000", 1, 5, "block-hash", 9)
            .await
            .unwrap();

        let cardano_transactions = vec![CardanoTransaction::new(
            "tx-hash-123",
            10,
            50,
            "block-hash-123",
            99,
        )];
        repository
            .store_transactions(&cardano_transactions)
            .await
            .unwrap();

        let transaction_result = repository.get_transaction("tx-hash-000").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-000".to_string(),
                block_number: 1,
                slot_number: 5,
                block_hash: "block-hash".to_string(),
                immutable_file_number: 9
            }),
            transaction_result
        );
    }

    #[tokio::test]
    async fn repository_get_highest_beacon_without_transactions_in_db() {
        let connection = get_connection().await;
        let repository = CardanoTransactionRepository::new(connection.clone());

        let highest_beacon = repository.get_highest_beacon().await.unwrap();
        assert_eq!(None, highest_beacon);
    }

    #[tokio::test]
    async fn repository_get_highest_beacon_with_transactions_in_db() {
        let connection = get_connection().await;
        let repository = CardanoTransactionRepository::new(connection.clone());

        let cardano_transactions = vec![
            CardanoTransaction::new("tx-hash-123".to_string(), 10, 50, "block-hash-123", 50),
            CardanoTransaction::new("tx-hash-456".to_string(), 11, 51, "block-hash-456", 100),
        ];
        repository
            .store_transactions(&cardano_transactions)
            .await
            .unwrap();

        let highest_beacon = repository.get_highest_beacon().await.unwrap();
        assert_eq!(Some(100), highest_beacon);
    }
}
