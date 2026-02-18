use anyhow::Context;
use serde::{Deserialize, Serialize};

use crate::{
    LotteryIndex, LotteryTargetValue, MembershipDigest, Parameters, StmResult,
    UniqueSchnorrSignature, VerificationKeyForSnark, signature_scheme::BaseFieldElement,
};

use super::{AggregateVerificationKeyForSnark, SnarkProofSigner};

/// Single signature for the Snark proof system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SingleSignatureForSnark {
    /// The underlying Schnorr signature
    schnorr_signature: UniqueSchnorrSignature,
    /// The minimum winning lottery index for which the signature is valid
    minimum_winning_lottery_index: LotteryIndex,
}

impl SingleSignatureForSnark {
    /// Create and return a new instance of `SingleSignatureForSnark` for given `schnorr_signature`
    /// and `minimum_winning_lottery_index`.
    pub(crate) fn new(
        schnorr_signature: UniqueSchnorrSignature,
        minimum_winning_lottery_index: LotteryIndex,
    ) -> Self {
        Self {
            schnorr_signature,
            minimum_winning_lottery_index,
        }
    }

    /// Verify a `SingleSignatureForSnark`. First try to build the message to verify using the
    /// provided `AggregateVerificationKeyForSnark` and the input `message`. Then verify the Schnorr
    /// signature for the built message and the provided `VerificationKeyForSnark`. Finally, verify
    /// that the lottery index associated with this signature actually won the lottery.
    pub(crate) fn verify<D: MembershipDigest>(
        &self,
        parameters: &Parameters,
        verification_key: &VerificationKeyForSnark,
        message: &[u8],
        lottery_target_value: &LotteryTargetValue,
        aggregate_verification_key: &AggregateVerificationKeyForSnark<D>,
    ) -> StmResult<()> {
        let message_to_verify = aggregate_verification_key
            .get_merkle_tree_commitment()
            .build_snark_message(message)?;
        self.schnorr_signature
            .verify(&message_to_verify, verification_key)
            .with_context(|| "Schnorr signature verification failed for SNARK proof system.")?;

        self.verify_winning_lottery_index::<D>(
            lottery_target_value.clone(),
            &message_to_verify,
            parameters.m,
        )?;

        Ok(())
    }

    /// Verifies that the lottery index associated with this signature actually won the lottery.
    fn verify_winning_lottery_index<D: MembershipDigest>(
        &self,
        lottery_target_value: LotteryTargetValue,
        message_to_verify: &[BaseFieldElement],
        m: u64,
    ) -> StmResult<()> {
        let lottery_prefix = SnarkProofSigner::<D>::compute_lottery_prefix(message_to_verify);
        SnarkProofSigner::<D>::verify_lottery_eligibility(
            &self.schnorr_signature,
            self.minimum_winning_lottery_index,
            m,
            lottery_prefix,
            lottery_target_value,
        )
    }

    /// Return `minimum_winning_lottery_index` of the single signature
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    pub(crate) fn get_minimum_winning_lottery_index(&self) -> LotteryIndex {
        self.minimum_winning_lottery_index
    }

    /// Set `minimum_winning_lottery_index` of single signature to given value
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    pub(crate) fn set_minimum_winning_lottery_index(
        &mut self,
        minimum_winning_lottery_index: LotteryIndex,
    ) {
        self.minimum_winning_lottery_index = minimum_winning_lottery_index;
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
