use rand_core::{CryptoRng, RngCore};

use crate::{
    BaseFieldElement, LotteryIndex, LotteryTargetValue, MembershipDigest, Parameters,
    SchnorrSigningKey, SignatureError, StmResult, UniqueSchnorrSignature, VerificationKeyForSnark,
    membership_commitment::MerkleTree, protocol::RegistrationEntryForSnark,
};

use super::SingleSignatureForSnark;

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub(crate) struct SnarkProofSigner<D: MembershipDigest> {
    parameters: Parameters,
    signing_key: SchnorrSigningKey,
    verification_key: VerificationKeyForSnark,
    lottery_target_value: LotteryTargetValue,
    key_registration_commitment: MerkleTree<D::SnarkHash, RegistrationEntryForSnark>,
}

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl<D: MembershipDigest> SnarkProofSigner<D> {
    /// Creates a new `SnarkProofSigner` with the given protocol parameters, keys,
    /// lottery target value, and Merkle tree commitment of the key registration.
    pub fn new(
        parameters: Parameters,
        signing_key: SchnorrSigningKey,
        verification_key: VerificationKeyForSnark,
        lottery_target_value: LotteryTargetValue,
        key_registration_commitment: MerkleTree<D::SnarkHash, RegistrationEntryForSnark>,
    ) -> Self {
        Self {
            parameters,
            signing_key,
            verification_key,
            lottery_target_value,
            key_registration_commitment,
        }
    }

    /// Generates a single signature for the SNARK proof system.
    ///
    /// Computes a unique Schnorr signature over `[merkle_root, msg]` and checks the
    /// lottery. Returns `SingleSignatureForSnark` if at least one lottery index is won,
    /// or `SignatureError::LotteryLost` otherwise.
    pub fn create_single_signature<R: RngCore + CryptoRng>(
        &self,
        message: &[u8],
        rng: &mut R,
    ) -> StmResult<SingleSignatureForSnark> {
        let merkle_tree_commitment = self.key_registration_commitment.to_merkle_tree_commitment();
        let message_to_sign = merkle_tree_commitment.build_snark_message(message)?;
        let signature = self.signing_key.sign(&message_to_sign, rng)?;

        // Create an empty list of indices if lottery is won and return signature
        if Self::check_lottery(
            &self.parameters,
            &message_to_sign,
            &signature,
            self.lottery_target_value,
        ) {
            let indices: Vec<LotteryIndex> = Vec::new();
            Ok(SingleSignatureForSnark::new(signature, indices))
        }
        // If lottery is lost, return an error
        else {
            Err((SignatureError::LotteryLost).into())
        }
    }

    /// Checks the lottery for all indices `0..m`.
    ///
    /// Computes the lottery prefix from the message, then iterates over each index
    /// to verify eligibility. Returns `true` if at least one index is won.
    pub(super) fn check_lottery(
        params: &Parameters,
        msg: &[BaseFieldElement],
        signature: &UniqueSchnorrSignature,
        lottery_target_value: LotteryTargetValue,
    ) -> bool {
        let lottery_prefix = Self::compute_lottery_prefix(msg);

        (0..params.m).any(|index| {
            Self::verify_lottery_eligibility(
                signature,
                index,
                params.m,
                lottery_prefix,
                lottery_target_value,
            )
            .is_ok()
        })
    }

    /// Placeholder for computing the lottery prefix based on the message.
    /// TODO: Replace with `Poseidon(DST_LOTTERY, merkle_root, msg)`.
    fn compute_lottery_prefix(
        _message_as_base_field_element: &[BaseFieldElement],
    ) -> BaseFieldElement {
        BaseFieldElement::get_one()
    }

    /// Placeholder for verifying lottery eligibility. Always returns Ok for now.
    /// TODO: Compute `ev = Poseidon(prefix, sigma.x, sigma.y, index)` and check `ev <= target`.
    fn verify_lottery_eligibility(
        _signature: &UniqueSchnorrSignature,
        _lottery_index: LotteryIndex,
        _m: u64,
        _prefix: BaseFieldElement,
        _target: LotteryTargetValue,
    ) -> StmResult<()> {
        Ok(())
    }

    /// Gets the lottery target value
    pub fn get_lottery_target_value(&self) -> LotteryTargetValue {
        self.lottery_target_value
    }
}
