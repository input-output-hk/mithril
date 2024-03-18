use crate::crypto_helper::{MKMapProof, ProtocolMkProof};
use crate::entities::TransactionHash;
use crate::messages::CardanoTransactionsSetProofMessagePart;
use crate::{StdError, StdResult};

use super::BlockRange;

/// A cryptographic proof of a set of Cardano transactions is included in the global Cardano transactions set
#[derive(Clone, Debug, PartialEq)]
pub struct CardanoTransactionsSetProof {
    /// Hashes of the certified transactions
    transactions_hashes: Vec<TransactionHash>,

    /// Proof of the transactions
    transactions_proof: ProtocolMkProof,
}

impl CardanoTransactionsSetProof {
    /// CardanoTransactionsSetProof factory
    pub fn new(
        transactions_hashes: Vec<TransactionHash>,
        transactions_proof: MKMapProof<BlockRange>,
    ) -> Self {
        Self {
            transactions_hashes,
            transactions_proof: ProtocolMkProof::new(transactions_proof),
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

    cfg_test_tools! {
        /// Retrieve a dummy proof (for test only)
        pub fn dummy() -> Self {
            use crate::crypto_helper::{MKMap, MKTree, MKMapNode};

            let transaction_hashes_by_block_range = vec![
                (
                    (0..10).into(),
                    vec!["tx-1".to_string(), "tx-2".to_string(), "tx-3".to_string()],
                ),
                (
                    (10..20).into(),
                    vec!["tx-4".to_string()],
                ),
                (
                    (20..30).into(),
                    vec!["tx-5".to_string(), "tx-6".to_string()],
                ),
            ];
            let transaction_hashes_to_certify = transaction_hashes_by_block_range
                .iter()
                .flat_map(|(_, h)| h.to_owned())
                .collect::<Vec<TransactionHash>>();

            let mk_hash_map: MKMap<_, MKMapNode<_>> = MKMap::new(
                transaction_hashes_by_block_range
                    .into_iter()
                    .map(|(block_range, transactions)| {
                        (block_range, MKTree::new(&transactions).unwrap().into())
                    })
                    .collect::<Vec<_>>()
                    .as_slice(),
            )
            .unwrap();
            let proof = mk_hash_map
                .compute_proof(&transaction_hashes_to_certify)
                .unwrap();
            Self::new(transaction_hashes_to_certify, proof)
        }
    }
}

impl TryFrom<CardanoTransactionsSetProof> for CardanoTransactionsSetProofMessagePart {
    type Error = StdError;

    fn try_from(proof: CardanoTransactionsSetProof) -> Result<Self, Self::Error> {
        Ok(Self {
            transactions_hashes: proof.transactions_hashes,
            proof: proof.transactions_proof.to_json_hex()?,
        })
    }
}

impl TryFrom<CardanoTransactionsSetProofMessagePart> for CardanoTransactionsSetProof {
    type Error = StdError;

    fn try_from(proof: CardanoTransactionsSetProofMessagePart) -> Result<Self, Self::Error> {
        Ok(Self {
            transactions_hashes: proof.transactions_hashes,
            transactions_proof: ProtocolMkProof::from_json_hex(&proof.proof)?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_verify_where_all_hashes_are_contained_in_the_proof() {
        let proof = CardanoTransactionsSetProof::dummy();

        proof.verify().expect("The proof should be valid");
    }

    #[test]
    fn shouldnt_verify_where_at_least_one_hash_is_not_contained_in_the_proof() {
        let proof = CardanoTransactionsSetProof::dummy();
        let mut transactions_hashes_tampered = proof.transactions_hashes().to_vec();
        transactions_hashes_tampered.push("tx-123".to_string());
        let proof = CardanoTransactionsSetProof {
            transactions_hashes: transactions_hashes_tampered,
            ..proof
        };

        proof.verify().expect_err("The proof should be invalid");
    }
}
