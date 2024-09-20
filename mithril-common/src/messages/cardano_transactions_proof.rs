use crate::entities::{
    BlockNumber, CardanoTransactionsSetProof, ProtocolMessage, ProtocolMessagePartKey,
    TransactionHash,
};
use crate::messages::CardanoTransactionsSetProofMessagePart;
use crate::StdError;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

/// A cryptographic proof for a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
#[cfg_attr(
    target_family = "wasm",
    wasm_bindgen(getter_with_clone, js_name = "CardanoTransactionsProofs")
)]
pub struct CardanoTransactionsProofsMessage {
    /// Hash of the certificate that validate this proof merkle root
    pub certificate_hash: String,

    /// Transactions that have been certified
    pub certified_transactions: Vec<CardanoTransactionsSetProofMessagePart>,

    /// Transactions that could not be certified
    pub non_certified_transactions: Vec<TransactionHash>,

    /// Latest block number that has been certified
    pub latest_block_number: BlockNumber,
}

#[cfg_attr(
    target_family = "wasm",
    wasm_bindgen(js_class = "CardanoTransactionsProofs")
)]
impl CardanoTransactionsProofsMessage {
    /// Transactions that have been certified
    #[cfg_attr(target_family = "wasm", wasm_bindgen(getter))]
    pub fn transactions_hashes(&self) -> Vec<TransactionHash> {
        self.certified_transactions
            .iter()
            .flat_map(|ct| ct.transactions_hashes.clone())
            .collect::<Vec<_>>()
    }
}

/// Set of transactions verified by [CardanoTransactionsProofsMessage::verify].
///
/// Can be used to reconstruct part of a [ProtocolMessage] in order to check that
/// it is indeed signed by a certificate.
#[derive(Debug, Clone, PartialEq)]
pub struct VerifiedCardanoTransactions {
    certificate_hash: String,
    merkle_root: String,
    certified_transactions: Vec<TransactionHash>,
    latest_block_number: BlockNumber,
}

impl VerifiedCardanoTransactions {
    /// Hash of the certificate that signs this struct Merkle root.
    pub fn certificate_hash(&self) -> &str {
        &self.certificate_hash
    }

    /// Hashes of the certified transactions
    pub fn certified_transactions(&self) -> &[TransactionHash] {
        &self.certified_transactions
    }

    /// Fill the given [ProtocolMessage] with the data associated with this
    /// verified transactions set.
    pub fn fill_protocol_message(&self, message: &mut ProtocolMessage) {
        message.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            self.merkle_root.clone(),
        );

        message.set_message_part(
            ProtocolMessagePartKey::LatestBlockNumber,
            self.latest_block_number.to_string(),
        );
    }
}

/// Error encountered or produced by the [cardano transaction proof verification][CardanoTransactionsProofsMessage::verify].
#[derive(Error, Debug)]
pub enum VerifyCardanoTransactionsProofsError {
    /// The verification of an individual [CardanoTransactionsSetProofMessagePart] failed.
    #[error("Invalid set proof for transactions hashes: {transactions_hashes:?}")]
    InvalidSetProof {
        /// Hashes of the invalid transactions
        transactions_hashes: Vec<TransactionHash>,
        /// Error source
        source: StdError,
    },

    /// No certified transactions set proof to verify
    #[error("There's no certified transaction to verify")]
    NoCertifiedTransaction,

    /// Not all certified transactions set proof have the same merkle root.
    ///
    /// This is problematic because all the set proof should be generated from the same
    /// merkle tree which root is signed in the [certificate][crate::entities::Certificate].
    #[error("All certified transactions set proofs must share the same Merkle root")]
    NonMatchingMerkleRoot,

    /// An individual [CardanoTransactionsSetProofMessagePart] could not be converted to a
    /// [CardanoTransactionsProofsMessage] for verification.
    #[error("Malformed data or unknown Cardano Set Proof format")]
    MalformedData(#[source] StdError),
}

impl CardanoTransactionsProofsMessage {
    /// Create a new `CardanoTransactionsProofsMessage`
    pub fn new(
        certificate_hash: &str,
        certified_transactions: Vec<CardanoTransactionsSetProofMessagePart>,
        non_certified_transactions: Vec<TransactionHash>,
        latest_block_number: BlockNumber,
    ) -> Self {
        Self {
            certificate_hash: certificate_hash.to_string(),
            certified_transactions,
            non_certified_transactions,
            latest_block_number,
        }
    }

    /// Verify that all the certified transactions proofs are valid
    ///
    /// The following checks will be executed:
    ///
    /// 1 - Check that each Merkle proof is valid
    ///
    /// 2 - Check that all proofs share the same Merkle root
    ///
    /// 3 - Assert that there's at least one certified transaction
    ///
    /// If every check is okay, the hex encoded Merkle root of the proof will be returned.
    pub fn verify(
        &self,
    ) -> Result<VerifiedCardanoTransactions, VerifyCardanoTransactionsProofsError> {
        let mut merkle_root = None;

        for certified_transaction in &self.certified_transactions {
            let certified_transaction: CardanoTransactionsSetProof = certified_transaction
                .clone()
                .try_into()
                .map_err(VerifyCardanoTransactionsProofsError::MalformedData)?;
            certified_transaction.verify().map_err(|e| {
                VerifyCardanoTransactionsProofsError::InvalidSetProof {
                    transactions_hashes: certified_transaction.transactions_hashes().to_vec(),
                    source: e,
                }
            })?;

            let tx_merkle_root = Some(certified_transaction.merkle_root());

            if merkle_root.is_none() {
                merkle_root = tx_merkle_root;
            } else if merkle_root != tx_merkle_root {
                return Err(VerifyCardanoTransactionsProofsError::NonMatchingMerkleRoot);
            }
        }

        Ok(VerifiedCardanoTransactions {
            certificate_hash: self.certificate_hash.clone(),
            merkle_root: merkle_root
                .ok_or(VerifyCardanoTransactionsProofsError::NoCertifiedTransaction)?,
            certified_transactions: self
                .certified_transactions
                .iter()
                .flat_map(|c| c.transactions_hashes.clone())
                .collect(),
            latest_block_number: self.latest_block_number,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::crypto_helper::MKProof;

    use super::*;

    #[test]
    fn verify_malformed_proofs_fail() {
        let txs_proofs = CardanoTransactionsProofsMessage::new(
            "whatever",
            vec![CardanoTransactionsSetProofMessagePart {
                transactions_hashes: vec![],
                proof: "invalid".to_string(),
            }],
            vec![],
            BlockNumber(99999),
        );

        let error = txs_proofs
            .verify()
            .expect_err("Malformed txs proofs should fail to verify itself");
        assert!(
            matches!(
                error,
                VerifyCardanoTransactionsProofsError::MalformedData(_)
            ),
            "Expected 'MalformedData' error but got '{:?}'",
            error
        );
    }

    #[test]
    fn verify_no_certified_transaction_fail() {
        let txs_proofs =
            CardanoTransactionsProofsMessage::new("whatever", vec![], vec![], BlockNumber(99999));

        let error = txs_proofs
            .verify()
            .expect_err("Proofs without certified transactions should fail to verify itself");
        assert!(
            matches!(
                error,
                VerifyCardanoTransactionsProofsError::NoCertifiedTransaction
            ),
            "Expected 'NoCertifiedTransactions' error but got '{:?}'",
            error
        );
    }

    #[test]
    fn verify_valid_proofs() {
        let set_proof = CardanoTransactionsSetProof::dummy();
        let expected = VerifiedCardanoTransactions {
            certificate_hash: "whatever".to_string(),
            merkle_root: set_proof.merkle_root(),
            certified_transactions: set_proof.transactions_hashes().to_vec(),
            latest_block_number: BlockNumber(99999),
        };
        let txs_proofs = CardanoTransactionsProofsMessage::new(
            "whatever",
            vec![set_proof.try_into().unwrap()],
            vec![],
            BlockNumber(99999),
        );

        let verified_txs = txs_proofs
            .verify()
            .expect("Valid txs proofs should verify itself");

        assert_eq!(expected, verified_txs);
    }

    #[test]
    fn verify_invalid_proofs() {
        let set_proof = CardanoTransactionsSetProof::new(
            vec!["invalid1".to_string()],
            MKProof::from_leaves(&["invalid2"]).unwrap(),
        );
        let txs_proofs = CardanoTransactionsProofsMessage::new(
            "whatever",
            vec![set_proof.try_into().unwrap()],
            vec![],
            BlockNumber(99999),
        );

        let error = txs_proofs
            .verify()
            .expect_err("Invalid txs proofs should fail to verify itself");

        assert!(
            matches!(
                error,
                VerifyCardanoTransactionsProofsError::InvalidSetProof { .. },
            ),
            "Expected 'InvalidSetProof' error but got '{:?}'",
            error
        );
    }

    #[test]
    fn verify_valid_proof_with_different_merkle_root_fail() {
        let set_proofs = vec![
            CardanoTransactionsSetProof::new(
                vec!["tx-1".to_string()],
                MKProof::from_leaves(&["tx-1"]).unwrap(),
            ),
            CardanoTransactionsSetProof::new(
                vec!["tx-2".to_string()],
                MKProof::from_leaves(&["tx-2"]).unwrap(),
            ),
        ];
        let txs_proofs = CardanoTransactionsProofsMessage::new(
            "whatever",
            set_proofs
                .into_iter()
                .map(|p| p.try_into().unwrap())
                .collect(),
            vec![],
            BlockNumber(99999),
        );

        let error = txs_proofs
            .verify()
            .expect_err("Txs proofs with non matching merkle root should fail to verify itself");

        assert!(
            matches!(
                error,
                VerifyCardanoTransactionsProofsError::NonMatchingMerkleRoot { .. },
            ),
            "Expected 'NonMatchingMerkleRoot' error but got '{:?}'",
            error
        );
    }

    #[cfg(feature = "fs")]
    mod fs_only {
        use crate::crypto_helper::{MKMap, MKMapNode, MKTreeStoreInMemory};
        use crate::entities::{BlockNumber, BlockRange, CardanoTransaction, SlotNumber};
        use crate::signable_builder::{
            CardanoTransactionsSignableBuilder, MockBlockRangeRootRetriever,
            MockTransactionsImporter, SignableBuilder,
        };
        use slog::Logger;
        use std::sync::Arc;

        use super::*;

        #[tokio::test]
        async fn verify_hashes_from_verified_cardano_transaction_and_from_signable_builder_are_equals(
        ) {
            let transactions = vec![
                CardanoTransaction::new(
                    "tx-hash-123",
                    BlockNumber(10),
                    SlotNumber(1),
                    "block_hash",
                ),
                CardanoTransaction::new(
                    "tx-hash-456",
                    BlockNumber(20),
                    SlotNumber(2),
                    "block_hash",
                ),
            ];

            assert_eq!(
                from_verified_cardano_transaction(&transactions, 99999).compute_hash(),
                from_signable_builder(&transactions, BlockNumber(99999))
                    .await
                    .compute_hash()
            );

            assert_ne!(
                from_verified_cardano_transaction(&transactions, 99999).compute_hash(),
                from_signable_builder(&transactions, BlockNumber(123456))
                    .await
                    .compute_hash()
            );
        }

        fn from_verified_cardano_transaction(
            transactions: &[CardanoTransaction],
            block_number: u64,
        ) -> ProtocolMessage {
            let set_proof = CardanoTransactionsSetProof::from_leaves::<MKTreeStoreInMemory>(
                transactions
                    .iter()
                    .map(|t| (t.block_number, t.transaction_hash.clone()))
                    .collect::<Vec<_>>()
                    .as_slice(),
            )
            .unwrap();

            let verified_transactions_fake = VerifiedCardanoTransactions {
                certificate_hash: "whatever".to_string(),
                merkle_root: set_proof.merkle_root(),
                certified_transactions: set_proof.transactions_hashes().to_vec(),
                latest_block_number: BlockNumber(block_number),
            };

            let mut message = ProtocolMessage::new();
            verified_transactions_fake.fill_protocol_message(&mut message);

            message
        }

        async fn from_signable_builder(
            transactions: &[CardanoTransaction],
            block_number: BlockNumber,
        ) -> ProtocolMessage {
            let seed_protocol_message = ProtocolMessage::new();
            let mut transaction_importer = MockTransactionsImporter::new();
            transaction_importer
                .expect_import()
                .return_once(move |_| Ok(()));
            let mut block_range_root_retriever = MockBlockRangeRootRetriever::new();

            let transactions_imported = transactions.to_vec();
            block_range_root_retriever
                .expect_compute_merkle_map_from_block_range_roots()
                .return_once(move |_| {
                    MKMap::<
                        BlockRange,
                        MKMapNode<BlockRange, MKTreeStoreInMemory>,
                        MKTreeStoreInMemory,
                    >::new_from_iter(transactions_imported.into_iter().map(
                        |tx| {
                            (
                                BlockRange::from_block_number(tx.block_number),
                                MKMapNode::TreeNode(tx.transaction_hash.clone().into()),
                            )
                        },
                    ))
                });
            let cardano_transaction_signable_builder = CardanoTransactionsSignableBuilder::new(
                Arc::new(transaction_importer),
                Arc::new(block_range_root_retriever),
                Logger::root(slog::Discard, slog::o!()),
            );
            cardano_transaction_signable_builder
                .compute_protocol_message(block_number, seed_protocol_message)
                .await
                .unwrap()
        }
    }
}
