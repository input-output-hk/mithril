use serde::{Deserialize, Serialize};

use crate::entities::{
    BlockNumber, CardanoTransaction, ProtocolMessage, ProtocolMessagePartKey, TransactionHash,
};
use crate::messages::proof_v2::ProofMessageVerifier;
use crate::messages::{CardanoTransactionMessagePart, MkSetProofMessagePart, VerifyProofsV2Error};

#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

/// A cryptographic proof for a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
#[cfg_attr(
    target_family = "wasm",
    wasm_bindgen(getter_with_clone, js_name = "CardanoTransactionsProofsV2")
)]
pub struct CardanoTransactionsProofsV2Message {
    /// Hash of the certificate that validate this proof merkle root
    pub certificate_hash: String,

    /// Transactions that have been certified
    pub certified_transactions: Vec<MkSetProofMessagePart<CardanoTransactionMessagePart>>,

    /// Hashes of the transactions that could not be certified
    pub non_certified_transactions: Vec<String>,

    /// Latest block number that has been certified
    pub latest_block_number: BlockNumber,
}

/// Set of transactions verified by [CardanoTransactionsProofsV2Message::verify].
///
/// Can be used to reconstruct part of a [ProtocolMessage] in order to check that
/// it is indeed signed by a certificate.
#[derive(Debug, Clone, PartialEq)]
pub struct VerifiedCardanoTransactionsV2 {
    certificate_hash: String,
    merkle_root: String,
    certified_transactions: Vec<CardanoTransactionMessagePart>,
    latest_block_number: BlockNumber,
}

impl VerifiedCardanoTransactionsV2 {
    /// Hash of the certificate that signs this struct Merkle root.
    pub fn certificate_hash(&self) -> &str {
        &self.certificate_hash
    }

    /// Certified transactions
    pub fn certified_transactions(&self) -> &[CardanoTransactionMessagePart] {
        &self.certified_transactions
    }

    /// Hashes of the certified transactions
    pub fn certified_transactions_hashes(&self) -> impl Iterator<Item = &TransactionHash> + '_ {
        self.certified_transactions.iter().map(|t| &t.transaction_hash)
    }

    /// Fill the given [ProtocolMessage] with the data associated with this
    /// verified transactions set.
    pub fn fill_protocol_message(&self, message: &mut ProtocolMessage) {
        message.set_message_part(
            ProtocolMessagePartKey::CardanoBlocksTransactionsMerkleRoot,
            self.merkle_root.clone(),
        );

        message.set_message_part(
            ProtocolMessagePartKey::LatestBlockNumber,
            self.latest_block_number.to_string(),
        );
    }
}

impl CardanoTransactionsProofsV2Message {
    /// Create a new `ProofsV2CardanoTransactionsMessage`
    pub fn new(
        certificate_hash: &str,
        certified_transactions: Vec<MkSetProofMessagePart<CardanoTransactionMessagePart>>,
        non_certified_transactions: Vec<String>,
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
    pub fn verify(&self) -> Result<VerifiedCardanoTransactionsV2, VerifyProofsV2Error> {
        let merkle_root =
            ProofMessageVerifier::<_, CardanoTransaction>::new("Cardano transactions", |tx| {
                tx.transaction_hash.clone()
            })
            .verify(&self.certified_transactions)?;

        Ok(VerifiedCardanoTransactionsV2 {
            certificate_hash: self.certificate_hash.clone(),
            merkle_root,
            certified_transactions: self
                .certified_transactions
                .iter()
                .flat_map(|t| t.items.clone())
                .collect(),
            latest_block_number: self.latest_block_number,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::crypto_helper::{MKMap, MKMapNode, MKTreeStoreInMemory};
    use crate::entities::{
        BlockNumber, BlockRange, CardanoBlockTransactionMkTreeNode, CardanoTransaction, MkSetProof,
        SlotNumber,
    };
    use crate::signable_builder::{
        CardanoBlocksTransactionsSignableBuilder, MockBlockRangeRootRetriever,
        MockBlocksTransactionsImporter, SignableBuilder,
    };
    use crate::test::entities_extensions::MkSetProofTestExtension;
    use crate::test::mock_extensions::MockBuilder;

    use super::*;

    #[tokio::test]
    async fn verify_hashes_from_verified_cardano_transaction_and_from_signable_builder_are_equals()
    {
        let transactions = vec![
            CardanoTransaction::new(
                "tx-hash-123",
                BlockNumber(10),
                SlotNumber(1),
                "block_hash-10",
            ),
            CardanoTransaction::new(
                "tx-hash-456",
                BlockNumber(20),
                SlotNumber(2),
                "block_hash-20",
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
        let set_proof = MkSetProof::from_leaves::<MKTreeStoreInMemory>(transactions).unwrap();

        let verified_transactions_fake = VerifiedCardanoTransactionsV2 {
            certificate_hash: "whatever".to_string(),
            merkle_root: set_proof.merkle_root(),
            certified_transactions: transactions.iter().cloned().map(Into::into).collect(),
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
        let importer = MockBuilder::<MockBlocksTransactionsImporter>::configure(|mock| {
            mock.expect_import().return_once(move |_| Ok(()));
        });
        let block_range_root_retriever =
            MockBuilder::<MockBlockRangeRootRetriever<MKTreeStoreInMemory>>::configure(|mock| {
                let transactions_imported = transactions.to_vec();
                mock.expect_compute_merkle_map_from_block_range_roots()
                    .return_once(move |_| {
                        MKMap::<
                            BlockRange,
                            MKMapNode<BlockRange, MKTreeStoreInMemory>,
                            MKTreeStoreInMemory,
                        >::new_from_iter(
                            transactions_imported.into_iter().map(|tx| {
                                (
                                    BlockRange::from_block_number(tx.block_number),
                                    MKMapNode::TreeNode(
                                        CardanoBlockTransactionMkTreeNode::from(tx).into(),
                                    ),
                                )
                            }),
                        )
                    });
            });

        let signable_builder =
            CardanoBlocksTransactionsSignableBuilder::new(importer, block_range_root_retriever);
        signable_builder.compute_protocol_message(block_number).await.unwrap()
    }
}
