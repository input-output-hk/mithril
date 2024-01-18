use mithril_common::entities::ImmutableFileNumber;

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
