use anyhow::Context;
use serde::{Deserialize, Serialize};

use crate::{
    LotteryIndex, MembershipDigest, StmResult, UniqueSchnorrSignature, VerificationKeyForSnark,
};

use super::{AggregateVerificationKeyForSnark, build_snark_message};

/// Single signature for the Snark proof system.
#[derive(Debug, Clone, Serialize, Deserialize, Hash)]
pub(crate) struct SingleSignatureForSnark {
    /// The underlying Schnorr signature
    schnorr_signature: UniqueSchnorrSignature,
    /// Winning lottery indices. Empty at signing time; populated later during aggregation.
    indices: Vec<LotteryIndex>,
}

impl SingleSignatureForSnark {
    /// Create and return a new instance of `SingleSignatureForSnark` for given `schnorr_signature`
    /// and `indices`.
    pub(crate) fn new(
        schnorr_signature: UniqueSchnorrSignature,
        indices: Vec<LotteryIndex>,
    ) -> Self {
        Self {
            schnorr_signature,
            indices,
        }
    }

    /// Verify a `SingleSignatureForSnark`. First try to build the message to verify using the
    /// provided `AggregateVerificationKeyForSnark` and the input `message`. Then verify the Schnorr
    /// signature for the built message and the provided `VerificationKeyForSnark`. The lottery
    /// eligibility check is not performed in this function, as it is expected to be done
    /// in the witness preparation.
    pub(crate) fn verify<D: MembershipDigest>(
        &self,
        verification_key: &VerificationKeyForSnark,
        message: &[u8],
        aggregate_verification_key: &AggregateVerificationKeyForSnark<D>,
    ) -> StmResult<()> {
        let message_to_verify = build_snark_message(
            &aggregate_verification_key.get_merkle_tree_commitment().root,
            message,
        )?;
        self.schnorr_signature
            .verify(&message_to_verify, verification_key)
            .with_context(|| "Schnorr signature verification failed for SNARK proof system.")?;

        Ok(())
    }

    /// Return `indices` of the single signature
    pub(crate) fn get_indices(&self) -> Vec<LotteryIndex> {
        self.indices.to_vec()
    }

    /// Set `indices` of single signature to given value
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    pub(crate) fn set_indices(&mut self, indices: &[LotteryIndex]) {
        self.indices = indices.to_vec();
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
