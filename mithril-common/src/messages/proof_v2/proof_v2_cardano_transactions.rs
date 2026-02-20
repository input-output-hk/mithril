use serde::{Deserialize, Serialize};

use crate::entities::{
    BlockNumber, CardanoTransaction, ProtocolMessage, ProtocolMessagePartKey, TransactionHash,
};
use crate::messages::proof_v2::ProofMessageVerifier;
use crate::messages::{CardanoTransactionMessagePart, MkSetProofMessagePart, VerifyProofsV2Error};

#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

/// A cryptographic proof for a set of items
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
#[cfg_attr(
    target_family = "wasm",
    wasm_bindgen(getter_with_clone, js_name = "CardanoTransactionsProofs")
)]
pub struct ProofsV2CardanoTransactionsMessage {
    /// Hash of the certificate that validate this proof merkle root
    pub certificate_hash: String,

    /// Transactions that have been certified
    pub certified_transactions: Vec<MkSetProofMessagePart<CardanoTransactionMessagePart>>,

    /// Hashes of the transactions that could not be certified
    pub non_certified_transactions: Vec<String>,

    /// Latest block number that has been certified
    pub latest_block_number: BlockNumber,
}

/// Set of transactions verified by [ProofsV2CardanoTransactionsMessage::verify].
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

impl ProofsV2CardanoTransactionsMessage {
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

    /// Verify that all the certified items proofs are valid
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
            ProofMessageVerifier::<_, CardanoTransaction>::new(|tx| tx.transaction_hash.clone())
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
    // use std::sync::Arc;
    //
    // use crate::crypto_helper::{MKMap, MKMapNode, MKProof, MKTreeStoreInMemory};
    // use crate::entities::{BlockNumber, BlockRange, CardanoTransaction, SlotNumber};
    // use crate::signable_builder::{
    //     CardanoTransactionsSignableBuilder, MockLegacyBlockRangeRootRetriever,
    //     MockTransactionsImporter, SignableBuilder,
    // };
    // use crate::test::crypto_helper::MKProofTestExtension;
    // use crate::test::double::Dummy;
    // use crate::test::entities_extensions::CardanoTransactionsSetProofTestExtension;
    //
    // use super::*;
    //
    // #[test]
    // fn verify_malformed_proofs_fail() {
    //     let txs_proofs = ProofsV2CardanoTransactionsMessage::new(
    //         "whatever",
    //         vec![CardanoTransactionsSetProofMessagePart {
    //             transactions_hashes: vec![],
    //             proof: "invalid".to_string(),
    //         }],
    //         vec![],
    //         BlockNumber(99999),
    //     );
    //
    //     let error = txs_proofs
    //         .verify()
    //         .expect_err("Malformed txs proofs should fail to verify itself");
    //     assert!(
    //         matches!(error, VerifyProofsV2Error::MalformedData(_)),
    //         "Expected 'MalformedData' error but got '{error:?}'"
    //     );
    // }
    //
    // #[test]
    // fn verify_no_certified_transaction_fail() {
    //     let txs_proofs =
    //         ProofsV2CardanoTransactionsMessage::new("whatever", vec![], vec![], BlockNumber(99999));
    //
    //     let error = txs_proofs
    //         .verify()
    //         .expect_err("Proofs without certified transactions should fail to verify itself");
    //     assert!(
    //         matches!(error, VerifyProofsV2Error::NoCertifiedTransaction),
    //         "Expected 'NoCertifiedTransactions' error but got '{error:?}'"
    //     );
    // }
    //
    // #[test]
    // fn verify_valid_proofs() {
    //     let set_proof = CardanoTransactionsSetProof::dummy();
    //     let expected = VerifiedCardanoTransactionsV2 {
    //         certificate_hash: "whatever".to_string(),
    //         merkle_root: set_proof.merkle_root(),
    //         certified_transactions: set_proof.transactions_hashes().to_vec(),
    //         latest_block_number: BlockNumber(99999),
    //     };
    //     let txs_proofs = ProofsV2CardanoTransactionsMessage::new(
    //         "whatever",
    //         vec![set_proof.try_into().unwrap()],
    //         vec![],
    //         BlockNumber(99999),
    //     );
    //
    //     let verified_txs = txs_proofs.verify().expect("Valid txs proofs should verify itself");
    //
    //     assert_eq!(expected, verified_txs);
    // }
    //
    // #[test]
    // fn verify_invalid_proofs() {
    //     let set_proof = CardanoTransactionsSetProof::new(
    //         vec!["invalid1".to_string()],
    //         MKProof::from_leaves(&["invalid2"]).unwrap(),
    //     );
    //     let txs_proofs = ProofsV2CardanoTransactionsMessage::new(
    //         "whatever",
    //         vec![set_proof.try_into().unwrap()],
    //         vec![],
    //         BlockNumber(99999),
    //     );
    //
    //     let error = txs_proofs
    //         .verify()
    //         .expect_err("Invalid txs proofs should fail to verify itself");
    //
    //     assert!(
    //         matches!(error, VerifyProofsV2Error::InvalidSetProof { .. },),
    //         "Expected 'InvalidSetProof' error but got '{error:?}'"
    //     );
    // }
    //
    // #[test]
    // fn verify_valid_proof_with_different_merkle_root_fail() {
    //     let set_proofs = vec![
    //         CardanoTransactionsSetProof::new(
    //             vec!["tx-1".to_string()],
    //             MKProof::from_leaves(&["tx-1"]).unwrap(),
    //         ),
    //         CardanoTransactionsSetProof::new(
    //             vec!["tx-2".to_string()],
    //             MKProof::from_leaves(&["tx-2"]).unwrap(),
    //         ),
    //     ];
    //     let txs_proofs = ProofsV2CardanoTransactionsMessage::new(
    //         "whatever",
    //         set_proofs.into_iter().map(|p| p.try_into().unwrap()).collect(),
    //         vec![],
    //         BlockNumber(99999),
    //     );
    //
    //     let error = txs_proofs
    //         .verify()
    //         .expect_err("Txs proofs with non matching merkle root should fail to verify itself");
    //
    //     assert!(
    //         matches!(error, VerifyProofsV2Error::NonMatchingMerkleRoot,),
    //         "Expected 'NonMatchingMerkleRoot' error but got '{error:?}'"
    //     );
    // }
    //
    // #[tokio::test]
    // async fn verify_hashes_from_verified_cardano_transaction_and_from_signable_builder_are_equals()
    // {
    //     let transactions = vec![
    //         CardanoTransaction::new("tx-hash-123", BlockNumber(10), SlotNumber(1), "block_hash"),
    //         CardanoTransaction::new("tx-hash-456", BlockNumber(20), SlotNumber(2), "block_hash"),
    //     ];
    //
    //     assert_eq!(
    //         from_verified_cardano_transaction(&transactions, 99999).compute_hash(),
    //         from_signable_builder(&transactions, BlockNumber(99999))
    //             .await
    //             .compute_hash()
    //     );
    //
    //     assert_ne!(
    //         from_verified_cardano_transaction(&transactions, 99999).compute_hash(),
    //         from_signable_builder(&transactions, BlockNumber(123456))
    //             .await
    //             .compute_hash()
    //     );
    // }
    //
    // fn from_verified_cardano_transaction(
    //     transactions: &[CardanoTransaction],
    //     block_number: u64,
    // ) -> ProtocolMessage {
    //     let set_proof = CardanoTransactionsSetProof::from_leaves::<MKTreeStoreInMemory>(
    //         transactions
    //             .iter()
    //             .map(|t| (t.block_number, t.transaction_hash.clone()))
    //             .collect::<Vec<_>>()
    //             .as_slice(),
    //     )
    //     .unwrap();
    //
    //     let verified_transactions_fake = VerifiedCardanoTransactionsV2 {
    //         certificate_hash: "whatever".to_string(),
    //         merkle_root: set_proof.merkle_root(),
    //         certified_transactions: set_proof.transactions_hashes().to_vec(),
    //         latest_block_number: BlockNumber(block_number),
    //     };
    //
    //     let mut message = ProtocolMessage::new();
    //     verified_transactions_fake.fill_protocol_message(&mut message);
    //
    //     message
    // }
    //
    // async fn from_signable_builder(
    //     transactions: &[CardanoTransaction],
    //     block_number: BlockNumber,
    // ) -> ProtocolMessage {
    //     let mut transaction_importer = MockTransactionsImporter::new();
    //     transaction_importer.expect_import().return_once(move |_| Ok(()));
    //     let mut block_range_root_retriever = MockLegacyBlockRangeRootRetriever::new();
    //
    //     let transactions_imported = transactions.to_vec();
    //     block_range_root_retriever
    //         .expect_compute_merkle_map_from_block_range_roots()
    //         .return_once(move |_| {
    //             MKMap::<
    //                 BlockRange,
    //                 MKMapNode<BlockRange, MKTreeStoreInMemory>,
    //                 MKTreeStoreInMemory,
    //             >::new_from_iter(transactions_imported.into_iter().map(
    //                 |tx| {
    //                     (
    //                         BlockRange::from_block_number(tx.block_number),
    //                         MKMapNode::TreeNode(tx.transaction_hash.clone().into()),
    //                     )
    //                 },
    //             ))
    //         });
    //     let cardano_transaction_signable_builder = CardanoTransactionsSignableBuilder::new(
    //         Arc::new(transaction_importer),
    //         Arc::new(block_range_root_retriever),
    //     );
    //     cardano_transaction_signable_builder
    //         .compute_protocol_message(block_number)
    //         .await
    //         .unwrap()
    // }
}
