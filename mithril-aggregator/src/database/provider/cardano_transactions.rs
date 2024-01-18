use mithril_common::{
    entities::ImmutableFileNumber,
    sqlite::{
        HydrationError, Projection, Provider, SourceAlias, SqLiteEntity, SqliteConnection,
        WhereCondition,
    },
    StdResult,
};

use sqlite::{Row, Value};
use std::sync::Arc;

/// TransactionHash is the unique identifier of a cardano transaction.
pub type TransactionHash = String;

/// BlockNumber is the block number of a cardano transaction.
pub type BlockNumber = u64;

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
}

impl<'client> Provider<'client> for InsertCardanoTransactionProvider<'client> {
    type Entity = CardanoTransactionRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert into cardano_tx {condition} returning {projection}")
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
    ) -> StdResult<CardanoTransactionRecord> {
        let provider = InsertCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_insert_condition(&CardanoTransactionRecord {
            transaction_hash: transaction_hash.to_owned(),
            block_number,
            immutable_file_number,
        })?;
        let mut cursor = provider.find(filters)?;

        cursor
            .next()
            .ok_or_else(|| panic!("Inserting a Cardano transaction should not return nothing."))
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
            .get_sqlite_connection_cardano_transactions()
            .await
            .unwrap()
    }

    #[test]
    fn cardano_transactions_projection() {
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
}
