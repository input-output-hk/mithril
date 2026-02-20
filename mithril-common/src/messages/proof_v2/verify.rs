use crate::entities::{IntoMKTreeNode, MkSetProof};
use crate::messages::{MkSetProofMessagePart, VerifyProofsV2Error};

pub(crate) struct ProofMessageVerifier<T, U> {
    subject: &'static str,
    hash_extractor: fn(&U) -> String,
    _phantom: std::marker::PhantomData<T>,
}

impl<T, U> ProofMessageVerifier<T, U>
where
    T: Clone,
    U: IntoMKTreeNode + Clone + From<T>,
{
    pub fn new(subject: &'static str, hash_extractor: fn(&U) -> String) -> Self {
        Self {
            subject,
            hash_extractor,
            _phantom: std::marker::PhantomData,
        }
    }

    fn proof_message_into_entity(
        &self,
        message: &MkSetProofMessagePart<T>,
    ) -> Result<MkSetProof<U>, VerifyProofsV2Error> {
        message
            .clone()
            .try_into()
            .map_err(|e| VerifyProofsV2Error::MalformedData(self.subject, e))
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
            let certified_item = self.proof_message_into_entity(item)?;
            certified_item
                .verify()
                .map_err(|e| VerifyProofsV2Error::InvalidSetProof {
                    subject: self.subject,
                    hashes: certified_item.items.iter().map(self.hash_extractor).collect(),
                    source: e,
                })?;

            let tx_merkle_root = Some(certified_item.merkle_root());

            if merkle_root.is_none() {
                merkle_root = tx_merkle_root;
            } else if merkle_root != tx_merkle_root {
                return Err(VerifyProofsV2Error::NonMatchingMerkleRoot(self.subject));
            }
        }

        merkle_root.ok_or(VerifyProofsV2Error::NoCertifiedTransaction(self.subject))
    }
}

#[cfg(test)]
mod tests {
    use crate::crypto_helper::{
        MKMap, MKMapNode, MKTree, MKTreeStoreInMemory, ProtocolKey, ProtocolMkProof,
    };
    use crate::entities::BlockRange;
    use crate::test::entities_extensions::BlockRangeTestExtension;

    use super::*;

    fn mk_map_proof_for(items: &[&str]) -> ProtocolMkProof {
        let mk_map: MKMap<_, MKMapNode<BlockRange, MKTreeStoreInMemory>, MKTreeStoreInMemory> =
            MKMap::new(&[(BlockRange::new(0, 100), MKTree::new(items).unwrap().into())]).unwrap();
        ProtocolKey::new(mk_map.compute_proof(items).unwrap())
    }

    #[test]
    fn verify_malformed_proofs_fail() {
        let proof_message = MkSetProofMessagePart::<&str> {
            items: vec![],
            proof: "invalid".to_string(),
        };

        let error = ProofMessageVerifier::<_, &str>::new("subject", |i| i.to_string())
            .verify(&[proof_message])
            .expect_err("Malformed txs proofs should fail to verify itself");
        assert!(
            matches!(error, VerifyProofsV2Error::MalformedData(..)),
            "Expected 'MalformedData' error but got '{error:?}'"
        );
    }

    #[test]
    fn verify_no_certified_item_fail() {
        let error = ProofMessageVerifier::<&str, &str>::new("subject", |i| i.to_string())
            .verify(&[])
            .expect_err("Proofs without certified item should fail to verify itself");
        assert!(
            matches!(error, VerifyProofsV2Error::NoCertifiedTransaction(..)),
            "Expected 'NoCertifiedTransactions' error but got '{error:?}'"
        );
    }

    #[test]
    fn verify_valid_proofs() {
        let items = vec!["item1", "item2", "item3"];
        let proof = mk_map_proof_for(&items);
        let txs_proofs = MkSetProofMessagePart::<&str> {
            items: items.clone(),
            proof: proof.to_json_hex().unwrap(),
        };

        let merkle_root = ProofMessageVerifier::<&str, &str>::new("subject", |i| i.to_string())
            .verify(&[txs_proofs])
            .expect("Valid proof should verify itself");

        assert_eq!(proof.compute_root().to_hex(), merkle_root);
    }

    #[test]
    fn verify_proof_with_items_not_included_in_its_merkle_tree_fail() {
        let txs_proofs = MkSetProofMessagePart::<&str> {
            items: vec!["invalid"],
            proof: mk_map_proof_for(&["item1"]).to_json_hex().unwrap(),
        };

        let error = ProofMessageVerifier::<&str, &str>::new("subject", |i| i.to_string())
            .verify(&[txs_proofs])
            .expect_err(
                "Proofs with items not included in its merkle tree should fail to verify itself",
            );

        assert!(
            matches!(error, VerifyProofsV2Error::InvalidSetProof { .. }),
            "Expected 'InvalidSetProof' error but got '{error:?}'"
        );
    }

    #[test]
    fn verify_valid_proof_with_different_merkle_root_fail() {
        let txs_proofs = vec![
            MkSetProofMessagePart::<&str> {
                items: vec!["item1"],
                proof: mk_map_proof_for(&["item1", "item2"]).to_json_hex().unwrap(),
            },
            MkSetProofMessagePart::<&str> {
                items: vec!["item2"],
                proof: mk_map_proof_for(&["item2", "other"]).to_json_hex().unwrap(),
            },
        ];

        let error = ProofMessageVerifier::<&str, &str>::new("subject", |i| i.to_string())
            .verify(&txs_proofs)
            .expect_err("Txs proofs with non matching merkle root should fail to verify itself");

        assert!(
            matches!(error, VerifyProofsV2Error::NonMatchingMerkleRoot(..)),
            "Expected 'NonMatchingMerkleRoot' error but got '{error:?}'"
        );
    }
}
