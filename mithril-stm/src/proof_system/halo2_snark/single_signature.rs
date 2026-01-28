use std::hash::{Hash, Hasher};

use serde::{Deserialize, Serialize};

use crate::{LotteryIndex, UniqueSchnorrSignature};

/// Single signature for the Snark proof system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SingleSignatureForSnark {
    /// The underlying Schnorr signature
    schnorr_signature: UniqueSchnorrSignature,
    /// The index(es) for which the signature is valid
    indexes: Vec<LotteryIndex>,
}

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl SingleSignatureForSnark {
    /// Create and return a new instance of `SingleSignatureForSnark` for given
    /// `schnorr_signature` and `indexes`.
    pub(crate) fn new(
        schnorr_signature: UniqueSchnorrSignature,
        indexes: Vec<LotteryIndex>,
    ) -> Self {
        Self {
            schnorr_signature,
            indexes,
        }
    }

    /// Return `indices` of the single signature
    pub(crate) fn get_indices(&self) -> &[LotteryIndex] {
        &self.indexes
    }

    /// Set `indexes` of single signature to given `indices`
    pub(crate) fn set_indices(&mut self, indices: &[LotteryIndex]) {
        self.indexes = indices.to_vec()
    }

    /// Return `schnorr_signature` of single signature
    pub(crate) fn get_schnorr_signature(&self) -> UniqueSchnorrSignature {
        self.schnorr_signature
    }
}

impl Hash for SingleSignatureForSnark {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.schnorr_signature.to_bytes(), state)
    }
}

impl PartialEq for SingleSignatureForSnark {
    fn eq(&self, other: &Self) -> bool {
        self.schnorr_signature == other.schnorr_signature
    }
}

impl Eq for SingleSignatureForSnark {}
