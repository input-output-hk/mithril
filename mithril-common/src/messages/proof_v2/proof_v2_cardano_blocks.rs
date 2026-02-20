use serde::{Deserialize, Serialize};

use crate::entities::{
    BlockHash, BlockNumber, CardanoBlock, ProtocolMessage, ProtocolMessagePartKey,
};
use crate::messages::proof_v2::ProofMessageVerifier;
use crate::messages::{CardanoBlockMessagePart, MkSetProofMessagePart, VerifyProofsV2Error};

#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

/// A cryptographic proof for a set of Cardano blocks
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
#[cfg_attr(
    target_family = "wasm",
    wasm_bindgen(getter_with_clone, js_name = "CardanoBlocksProofs")
)]
pub struct CardanoBlocksProofsMessage {
    /// Hash of the certificate that validate this proof merkle root
    pub certificate_hash: String,

    /// Blocks that have been certified
    pub certified_blocks: Vec<MkSetProofMessagePart<CardanoBlockMessagePart>>,

    /// Hashes of the blocks that could not be certified
    pub non_certified_blocks: Vec<String>,

    /// Latest block number that has been certified
    pub latest_block_number: BlockNumber,
}

/// Set of blocks verified by [CardanoBlocksProofsMessage::verify].
///
/// Can be used to reconstruct part of a [ProtocolMessage] in order to check that
/// it is indeed signed by a certificate.
#[derive(Debug, Clone, PartialEq)]
pub struct VerifiedCardanoBlocks {
    certificate_hash: String,
    merkle_root: String,
    certified_blocks: Vec<CardanoBlockMessagePart>,
    latest_block_number: BlockNumber,
}

impl VerifiedCardanoBlocks {
    /// Hash of the certificate that signs this struct Merkle root.
    pub fn certificate_hash(&self) -> &str {
        &self.certificate_hash
    }

    /// Certified blocks
    pub fn certified_blocks(&self) -> &[CardanoBlockMessagePart] {
        &self.certified_blocks
    }

    /// Hashes of the certified blocks
    pub fn certified_blocks_hashes(&self) -> impl Iterator<Item = &BlockHash> + '_ {
        self.certified_blocks.iter().map(|b| &b.block_hash)
    }

    /// Fill the given [ProtocolMessage] with the data associated with this
    /// verified blocks set.
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

impl CardanoBlocksProofsMessage {
    /// Create a new `ProofsV2CardanoBlocksMessage`
    pub fn new(
        certificate_hash: &str,
        certified_blocks: Vec<MkSetProofMessagePart<CardanoBlockMessagePart>>,
        non_certified_blocks: Vec<String>,
        latest_block_number: BlockNumber,
    ) -> Self {
        Self {
            certificate_hash: certificate_hash.to_string(),
            certified_blocks,
            non_certified_blocks,
            latest_block_number,
        }
    }

    /// Verify that all the certified blocks proofs are valid
    ///
    /// The following checks will be executed:
    ///
    /// 1 - Check that each Merkle proof is valid
    ///
    /// 2 - Check that all proofs share the same Merkle root
    ///
    /// 3 - Assert that there's at least one certified block
    ///
    /// If every check is okay, the hex encoded Merkle root of the proof will be returned.
    pub fn verify(&self) -> Result<VerifiedCardanoBlocks, VerifyProofsV2Error> {
        let merkle_root = ProofMessageVerifier::<_, CardanoBlock>::new("Cardano blocks", |block| {
            block.block_hash.clone()
        })
        .verify(&self.certified_blocks)?;

        Ok(VerifiedCardanoBlocks {
            certificate_hash: self.certificate_hash.clone(),
            merkle_root,
            certified_blocks: self.certified_blocks.iter().flat_map(|t| t.items.clone()).collect(),
            latest_block_number: self.latest_block_number,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::crypto_helper::{MKMap, MKMapNode, MKTreeStoreInMemory};
    use crate::entities::{
        BlockNumber, BlockRange, CardanoBlockTransactionMkTreeNode, MkSetProof, SlotNumber,
    };
    use crate::signable_builder::{
        CardanoBlocksTransactionsSignableBuilder, MockBlockRangeRootRetriever,
        MockBlocksTransactionsImporter, SignableBuilder,
    };
    use crate::test::entities_extensions::MkSetProofTestExtension;
    use crate::test::mock_extensions::MockBuilder;

    use super::*;

    #[tokio::test]
    async fn verify_hashes_from_verified_cardano_blocks_and_from_signable_builder_are_equals() {
        let blocks = vec![
            CardanoBlock::new("block_hash-10", BlockNumber(10), SlotNumber(1)),
            CardanoBlock::new("block_hash-20", BlockNumber(20), SlotNumber(2)),
        ];

        assert_eq!(
            from_verified_cardano_blocks(&blocks, 99999).compute_hash(),
            from_signable_builder(&blocks, BlockNumber(99999))
                .await
                .compute_hash()
        );

        assert_ne!(
            from_verified_cardano_blocks(&blocks, 99999).compute_hash(),
            from_signable_builder(&blocks, BlockNumber(123456))
                .await
                .compute_hash()
        );
    }

    fn from_verified_cardano_blocks(blocks: &[CardanoBlock], block_number: u64) -> ProtocolMessage {
        let set_proof = MkSetProof::from_leaves::<MKTreeStoreInMemory>(blocks).unwrap();

        let verified_blocks_fake = VerifiedCardanoBlocks {
            certificate_hash: "whatever".to_string(),
            merkle_root: set_proof.merkle_root(),
            certified_blocks: blocks.iter().cloned().map(Into::into).collect(),
            latest_block_number: BlockNumber(block_number),
        };

        let mut message = ProtocolMessage::new();
        verified_blocks_fake.fill_protocol_message(&mut message);

        message
    }

    async fn from_signable_builder(
        blocks: &[CardanoBlock],
        block_number: BlockNumber,
    ) -> ProtocolMessage {
        let importer = MockBuilder::<MockBlocksTransactionsImporter>::configure(|mock| {
            mock.expect_import().return_once(move |_| Ok(()));
        });
        let block_range_root_retriever = MockBuilder::<
            MockBlockRangeRootRetriever<MKTreeStoreInMemory>,
        >::configure(|mock| {
            let blocks_imported = blocks.to_vec();
            mock.expect_compute_merkle_map_from_block_range_roots()
                .return_once(move |_| {
                    MKMap::<
                        BlockRange,
                        MKMapNode<BlockRange, MKTreeStoreInMemory>,
                        MKTreeStoreInMemory,
                    >::new_from_iter(blocks_imported.into_iter().map(|block| {
                        (
                            BlockRange::from_block_number(block.block_number),
                            MKMapNode::TreeNode(
                                CardanoBlockTransactionMkTreeNode::from(block).into(),
                            ),
                        )
                    }))
                });
        });

        let signable_builder =
            CardanoBlocksTransactionsSignableBuilder::new(importer, block_range_root_retriever);
        signable_builder.compute_protocol_message(block_number).await.unwrap()
    }
}
