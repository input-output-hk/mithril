use mithril_common::{crypto_helper::MKProof, entities::TransactionHash};

/// A cryptographic proof of a set of Cardano transactions is included in the global Cardano transactions set
pub struct TransactionsSetProof {
    /// Hashes of the certified transactions
    transactions_hashes: Vec<TransactionHash>,

    /// Proof of the transactions
    transactions_proof: MKProof,
}
