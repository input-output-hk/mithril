use crate::entities::{IntoMKTreeNode, MkSetProof};
use crate::messages::{MkSetProofMessagePart, VerifyProofsV2Error};

pub(crate) struct ProofMessageVerifier<T, U> {
    hash_extractor: fn(&U) -> String,
    _phantom: std::marker::PhantomData<T>,
}

impl<T, U> ProofMessageVerifier<T, U>
where
    T: Clone,
    U: IntoMKTreeNode + Clone + From<T>,
{
    pub(super) fn new(hash_extractor: fn(&U) -> String) -> Self {
        Self {
            hash_extractor,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn proof_message_into_entity(
        message: &MkSetProofMessagePart<T>,
    ) -> Result<MkSetProof<U>, VerifyProofsV2Error>
    where
        T: Clone,
        U: IntoMKTreeNode + Clone + From<T>,
    {
        let mk_set_proof: MkSetProof<U> = message
            .clone()
            .try_into()
            .map_err(VerifyProofsV2Error::MalformedData)?;
        Ok(mk_set_proof)
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
    pub fn verify(
        &self,
        items: &[MkSetProofMessagePart<T>],
    ) -> Result<String, VerifyProofsV2Error> {
        let mut merkle_root = None;

        for item in items {
            let certified_item = Self::proof_message_into_entity(item)?;
            certified_item
                .verify()
                .map_err(|e| VerifyProofsV2Error::InvalidSetProof {
                    transactions_hashes: certified_item
                        .items
                        .iter()
                        .map(self.hash_extractor)
                        .collect(),
                    source: e,
                })?;

            let tx_merkle_root = Some(certified_item.merkle_root());

            if merkle_root.is_none() {
                merkle_root = tx_merkle_root;
            } else if merkle_root != tx_merkle_root {
                return Err(VerifyProofsV2Error::NonMatchingMerkleRoot);
            }
        }

        merkle_root.ok_or(VerifyProofsV2Error::NoCertifiedTransaction)
    }
}

#[cfg(test)]
mod tests {
    use crate::crypto_helper::{
        MKMap, MKMapNode, MKMapProof, MKProof, MKTree, MKTreeStoreInMemory,
    };
    use crate::entities::{BlockNumber, BlockRange};
    use crate::test::crypto_helper::MKProofTestExtension;
    use crate::test::entities_extensions::BlockRangeTestExtension;

    use super::*;

    fn mk_proof_for(items: &[&str]) -> MKMapProof<BlockRange> {
        let mk_map: MKMap<_, MKMapNode<BlockRange, MKTreeStoreInMemory>, MKTreeStoreInMemory> =
            MKMap::new(&[(BlockRange::new(0, 100), MKTree::new(items).unwrap().into())]).unwrap();
        mk_map.compute_proof(items).unwrap()
    }
    // TODO: ADAPT THE TESTS

    // #[test]
    // fn verify_malformed_proofs_fail() {
    //     let txs_proofs = MkSetProof::<String>::new(vec![CardanoTransactionsSetProofMessagePart {
    //         transactions_hashes: vec![],
    //         proof: "invalid".to_string(),
    //     }]);
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
    // fn verify_no_certified_item_fail() {
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
}
