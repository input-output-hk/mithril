use serde::{Deserialize, Serialize};

use crate::entities::{BlockNumber, BlockNumberOffset, CardanoTransaction, TransactionHash};
use crate::messages::proof_v2::ProofMessageVerifier;
use crate::messages::{CardanoTransactionMessagePart, MkSetProofMessagePart, VerifyProofsV2Error};

#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

/// A cryptographic proof for a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[cfg_attr(
    target_family = "wasm",
    wasm_bindgen(getter_with_clone, js_name = "CardanoTransactionsProofsV2")
)]
pub struct CardanoTransactionsProofsV2Message {
    /// Hash of the certificate that validates this proof Merkle root
    pub certificate_hash: String,

    /// Transactions that have been certified
    // Note: Skip in wasm as `wasm_bindgen` doesn't support generics
    #[cfg_attr(target_family = "wasm", wasm_bindgen(skip))]
    pub certified_transactions: Option<MkSetProofMessagePart<CardanoTransactionMessagePart>>,

    /// Hashes of the transactions that could not be certified
    pub non_certified_transactions: Vec<String>,

    /// Latest block number that has been certified by the associated Mithril certificate
    pub latest_block_number: BlockNumber,

    /// Security parameter that has been certified by the associated Mithril certificate
    pub security_parameter: BlockNumberOffset,
}

#[cfg_attr(
    target_family = "wasm",
    wasm_bindgen(js_class = "CardanoTransactionsProofsV2")
)]
impl CardanoTransactionsProofsV2Message {
    /// Cardano transactions that have been certified
    // Note: Wasm only as rust code can access them through the field directly
    #[cfg(target_family = "wasm")]
    #[wasm_bindgen(getter)]
    pub fn certified_transactions(&self) -> Vec<CardanoTransactionMessagePart> {
        self.certified_transactions
            .as_ref()
            .map(|ctxs| ctxs.items.clone())
            .unwrap_or_default()
    }

    /// Hashes of the Cardano transactions that have been certified
    #[cfg_attr(target_family = "wasm", wasm_bindgen(getter))]
    pub fn transactions_hashes(&self) -> Vec<TransactionHash> {
        self.certified_transactions
            .as_ref()
            .map(|ctxs| ctxs.items.iter().map(|t| t.transaction_hash.clone()).collect())
            .unwrap_or_default()
    }
}

/// Transactions successfully verified by [`CardanoTransactionsProofsV2Message::verify`].
///
/// Can be used to reconstruct a [`ProtocolMessage`][crate::entities::ProtocolMessage]
/// and confirm it was signed by a certificate.
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

    /// Hex encoded Merkle root of the certified transactions
    pub fn certified_merkle_root(&self) -> &str {
        &self.merkle_root
    }

    /// Certified transactions
    pub fn certified_transactions(&self) -> &[CardanoTransactionMessagePart] {
        &self.certified_transactions
    }

    /// Hashes of the certified transactions
    pub fn certified_transactions_hashes(&self) -> Vec<TransactionHash> {
        self.certified_transactions
            .iter()
            .map(|t| t.transaction_hash.clone())
            .collect()
    }

    /// Latest block number that has been certified by the associated Mithril certificate
    pub fn latest_certified_block_number(&self) -> BlockNumber {
        self.latest_block_number
    }
}

impl CardanoTransactionsProofsV2Message {
    /// Create a new `ProofsV2CardanoTransactionsMessage`
    pub fn new(
        certificate_hash: &str,
        certified_transactions: Option<MkSetProofMessagePart<CardanoTransactionMessagePart>>,
        non_certified_transactions: Vec<String>,
        latest_block_number: BlockNumber,
        security_parameter: BlockNumberOffset,
    ) -> Self {
        Self {
            certificate_hash: certificate_hash.to_string(),
            certified_transactions,
            non_certified_transactions,
            latest_block_number,
            security_parameter,
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
        const SUBJECT: &str = "Cardano transactions";
        let certified_transactions = self
            .certified_transactions
            .as_ref()
            .ok_or(VerifyProofsV2Error::NoCertifiedItem(SUBJECT))?;
        let merkle_root = ProofMessageVerifier::<_, CardanoTransaction>::new(SUBJECT, |tx| {
            tx.transaction_hash.clone()
        })
        .verify(certified_transactions)?;

        Ok(VerifiedCardanoTransactionsV2 {
            certificate_hash: self.certificate_hash.clone(),
            merkle_root,
            certified_transactions: certified_transactions.items.clone(),
            latest_block_number: self.latest_block_number,
        })
    }
}
