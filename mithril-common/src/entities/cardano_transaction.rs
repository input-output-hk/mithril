use super::ImmutableFileNumber;

/// TransactionHash is the unique identifier of a cardano transaction.
pub type TransactionHash = String;

/// BlockNumber is the block number of a cardano transaction.
pub type BlockNumber = u64;

#[derive(Debug, PartialEq, Clone)]
/// Cardano transaction representation
pub struct CardanoTransaction {
    /// Unique hash of the transaction
    pub transaction_hash: TransactionHash,

    /// Block number of the transaction
    pub block_number: BlockNumber,

    /// Immutable file number of the transaction
    pub immutable_file_number: ImmutableFileNumber,
}
