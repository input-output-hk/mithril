use serde::{Deserialize, Serialize};

use crate::entities::{BlockHash, BlockNumber, BlockNumberOffset, CardanoBlock};
use crate::messages::proof_v2::ProofMessageVerifier;
use crate::messages::{CardanoBlockMessagePart, MkSetProofMessagePart, VerifyProofsV2Error};

#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

/// A cryptographic proof for a set of Cardano blocks
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[cfg_attr(
    target_family = "wasm",
    wasm_bindgen(getter_with_clone, js_name = "CardanoBlocksProofs")
)]
pub struct CardanoBlocksProofsMessage {
    /// Hash of the certificate that validates this proof Merkle root
    pub certificate_hash: String,

    /// Blocks that have been certified
    // Note: Skip in wasm as `wasm_bindgen` doesn't support generics
    #[cfg_attr(target_family = "wasm", wasm_bindgen(skip))]
    pub certified_blocks: Option<MkSetProofMessagePart<CardanoBlockMessagePart>>,

    /// Hashes of the blocks that could not be certified
    pub non_certified_blocks: Vec<String>,

    /// Latest block number that has been certified by the associated Mithril certificate
    pub latest_block_number: BlockNumber,

    /// Security parameter that has been certified by the associated Mithril certificate
    pub security_parameter: BlockNumberOffset,
}

#[cfg_attr(target_family = "wasm", wasm_bindgen(js_class = "CardanoBlocksProofs"))]
impl CardanoBlocksProofsMessage {
    /// Cardano blocks that have been certified
    // Note: Wasm only as rust code can access them through the field directly
    #[cfg(target_family = "wasm")]
    #[wasm_bindgen(getter)]
    pub fn certified_blocks(&self) -> Vec<CardanoBlockMessagePart> {
        self.certified_blocks
            .as_ref()
            .map(|cbs| cbs.items.clone())
            .unwrap_or_default()
    }

    /// Hashes of the Cardano blocks that have been certified
    #[cfg_attr(target_family = "wasm", wasm_bindgen(getter))]
    pub fn blocks_hashes(&self) -> Vec<BlockHash> {
        self.certified_blocks
            .as_ref()
            .map(|cbs| cbs.items.iter().map(|cb| cb.block_hash.clone()).collect())
            .unwrap_or_default()
    }
}

/// Blocks successfully verified by [`CardanoBlocksProofsMessage::verify`].
///
/// Can be used to reconstruct a [`ProtocolMessage`][crate::entities::ProtocolMessage]
/// and confirm it was signed by a certificate.
#[derive(Debug, Clone, PartialEq)]
pub struct VerifiedCardanoBlocks {
    certificate_hash: String,
    merkle_root: String,
    certified_blocks: Vec<CardanoBlockMessagePart>,
    latest_block_number: BlockNumber,
    security_parameter: BlockNumberOffset,
}

impl VerifiedCardanoBlocks {
    /// Hash of the certificate that signs this struct Merkle root.
    pub fn certificate_hash(&self) -> &str {
        &self.certificate_hash
    }

    /// Hex encoded Merkle root of the certified blocks
    pub fn certified_merkle_root(&self) -> &str {
        &self.merkle_root
    }

    /// Certified blocks
    pub fn certified_blocks(&self) -> &[CardanoBlockMessagePart] {
        &self.certified_blocks
    }

    /// Hashes of the certified blocks
    pub fn certified_blocks_hashes(&self) -> Vec<BlockHash> {
        self.certified_blocks.iter().map(|b| b.block_hash.clone()).collect()
    }

    /// Latest block number that has been certified by the associated Mithril certificate
    pub fn latest_certified_block_number(&self) -> BlockNumber {
        self.latest_block_number
    }

    /// Security parameter that has been certified by the associated Mithril certificate
    pub fn security_parameter(&self) -> BlockNumberOffset {
        self.security_parameter
    }
}

impl CardanoBlocksProofsMessage {
    /// Create a new `ProofsV2CardanoBlocksMessage`
    pub fn new(
        certificate_hash: &str,
        certified_blocks: Option<MkSetProofMessagePart<CardanoBlockMessagePart>>,
        non_certified_blocks: Vec<String>,
        latest_block_number: BlockNumber,
        security_parameter: BlockNumberOffset,
    ) -> Self {
        Self {
            certificate_hash: certificate_hash.to_string(),
            certified_blocks,
            non_certified_blocks,
            latest_block_number,
            security_parameter,
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
        const SUBJECT: &str = "Cardano blocks";
        let certified_blocks = self
            .certified_blocks
            .as_ref()
            .ok_or(VerifyProofsV2Error::NoCertifiedItem(SUBJECT))?;
        let merkle_root =
            ProofMessageVerifier::<_, CardanoBlock>::new(SUBJECT, |block| block.block_hash.clone())
                .verify(certified_blocks)?;

        Ok(VerifiedCardanoBlocks {
            certificate_hash: self.certificate_hash.clone(),
            merkle_root,
            certified_blocks: certified_blocks.items.clone(),
            latest_block_number: self.latest_block_number,
            security_parameter: self.security_parameter,
        })
    }
}
