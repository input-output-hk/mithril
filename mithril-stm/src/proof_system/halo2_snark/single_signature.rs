use serde::{Deserialize, Serialize};

use crate::{LotteryIndex, UniqueSchnorrSignature};

/// Single signature for the Snark proof system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SingleSignatureForSnark {
    /// The underlying Schnorr signature
    schnorr_signature: UniqueSchnorrSignature,
    /// The index(es) for which the signature is valid
    indices: Vec<LotteryIndex>,
}

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl SingleSignatureForSnark {
    /// Create and return a new instance of `SingleSignatureForSnark` for given
    /// `schnorr_signature` and `indexes`.
    pub(crate) fn new(
        schnorr_signature: UniqueSchnorrSignature,
        indices: Vec<LotteryIndex>,
    ) -> Self {
        Self {
            schnorr_signature,
            indices,
        }
    }

    /// Return `indices` of the single signature
    pub(crate) fn get_indices(&self) -> &[LotteryIndex] {
        &self.indices
    }

    /// Set `indices` of single signature to given `indices`
    pub(crate) fn set_indices(&mut self, indices: &[LotteryIndex]) {
        self.indices = indices.to_vec()
    }

    /// Return `schnorr_signature` of single signature
    pub(crate) fn get_schnorr_signature(&self) -> UniqueSchnorrSignature {
        self.schnorr_signature
    }
}

impl PartialEq for SingleSignatureForSnark {
    fn eq(&self, other: &Self) -> bool {
        self.schnorr_signature == other.schnorr_signature
    }
}

impl Eq for SingleSignatureForSnark {}
