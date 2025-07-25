use crate::StdResult;
use crate::crypto_helper::{MKMapProof, ProtocolMkProof};
use crate::entities::TransactionHash;

use super::BlockRange;

/// A cryptographic proof of a set of Cardano transactions is included in the global Cardano transactions set
#[derive(Clone, Debug, PartialEq)]
pub struct CardanoTransactionsSetProof {
    /// Hashes of the certified transactions
    pub(crate) transactions_hashes: Vec<TransactionHash>,

    /// Proof of the transactions
    pub(crate) transactions_proof: ProtocolMkProof,
}

impl CardanoTransactionsSetProof {
    /// CardanoTransactionsSetProof factory
    pub fn new<T: Into<MKMapProof<BlockRange>>>(
        transactions_hashes: Vec<TransactionHash>,
        transactions_proof: T,
    ) -> Self {
        Self {
            transactions_hashes,
            transactions_proof: ProtocolMkProof::new(transactions_proof.into()),
        }
    }

    /// Return the hex encoded merkle root of this proof
    pub fn merkle_root(&self) -> String {
        self.transactions_proof.compute_root().to_hex()
    }

    /// Get the hashes of the transactions certified by this proof
    pub fn transactions_hashes(&self) -> &[TransactionHash] {
        &self.transactions_hashes
    }

    /// Verify that transactions set proof is valid
    pub fn verify(&self) -> StdResult<()> {
        self.transactions_proof.verify()?;
        for hash in &self.transactions_hashes {
            self.transactions_proof.contains(&hash.to_owned().into())?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::crypto_helper::MKTreeStoreInMemory;
    use crate::entities::BlockNumber;
    use crate::test::entities_extensions::CardanoTransactionsSetProofTestExtension;

    use super::*;

    #[test]
    fn should_verify_where_all_hashes_are_contained_in_the_proof() {
        let leaves = vec![
            (BlockNumber(0), "tx-1".to_string()),
            (BlockNumber(1), "tx-2".to_string()),
            (BlockNumber(1), "tx-3".to_string()),
            (BlockNumber(10), "tx-4".to_string()),
            (BlockNumber(20), "tx-5".to_string()),
            (BlockNumber(22), "tx-6".to_string()),
        ];
        let proof =
            CardanoTransactionsSetProof::from_leaves::<MKTreeStoreInMemory>(&leaves).unwrap();

        proof.verify().expect("The proof should be valid");
    }

    #[test]
    fn shouldnt_verify_where_at_least_one_hash_is_not_contained_in_the_proof() {
        let leaves = vec![
            (BlockNumber(0), "tx-1".to_string()),
            (BlockNumber(1), "tx-2".to_string()),
            (BlockNumber(1), "tx-3".to_string()),
            (BlockNumber(10), "tx-4".to_string()),
            (BlockNumber(20), "tx-5".to_string()),
            (BlockNumber(22), "tx-6".to_string()),
        ];
        let proof =
            CardanoTransactionsSetProof::from_leaves::<MKTreeStoreInMemory>(&leaves).unwrap();
        let mut transactions_hashes_tampered = proof.transactions_hashes().to_vec();
        transactions_hashes_tampered.push("tx-123".to_string());
        let proof = CardanoTransactionsSetProof {
            transactions_hashes: transactions_hashes_tampered,
            ..proof
        };

        proof.verify().expect_err("The proof should be invalid");
    }
}
