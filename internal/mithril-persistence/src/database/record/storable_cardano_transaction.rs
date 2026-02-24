use sqlite::Row;

use mithril_common::entities::{BlockHash, TransactionHash};

use crate::sqlite::{HydrationError, Projection, SqLiteEntity};

/// The record representation of a cardano transaction that can be stored in the database.
#[derive(Debug, PartialEq, Clone)]
pub struct StorableCardanoTransactionRecord {
    /// Unique hash of the transaction
    pub transaction_hash: TransactionHash,

    /// Block hash of the transaction
    pub block_hash: BlockHash,
}

impl StorableCardanoTransactionRecord {
    /// SQLite max variables per prepared query is `32 766`, given each record needs to binds two variables and to leave some
    /// room for other variables (i.e. in WHERE clause) we fix this limit.
    pub const MAX_PER_INSERT: usize = 10_000;

    /// StorableCardanoTransactionRecord factory
    pub fn new<T: Into<TransactionHash>, U: Into<BlockHash>>(hash: T, block_hash: U) -> Self {
        Self {
            transaction_hash: hash.into(),
            block_hash: block_hash.into(),
        }
    }
}

impl SqLiteEntity for StorableCardanoTransactionRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let transaction_hash = row.read::<&str, _>(0);
        let block_hash = row.read::<&str, _>(1);

        Ok(Self {
            transaction_hash: transaction_hash.to_string(),
            block_hash: block_hash.to_string(),
        })
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            (
                "transaction_hash",
                "{:cardano_tx:}.transaction_hash",
                "text",
            ),
            ("block_hash", "{:cardano_tx:}.block_hash", "text"),
        ])
    }
}
