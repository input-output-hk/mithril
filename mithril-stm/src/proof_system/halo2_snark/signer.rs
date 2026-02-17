use rand_core::{CryptoRng, RngCore};

use crate::{
    BaseFieldElement, LotteryIndex, LotteryTargetValue, MembershipDigest, Parameters,
    SchnorrSigningKey, SignatureError, StmResult, UniqueSchnorrSignature, VerificationKeyForSnark,
    membership_commitment::MerkleTree,
    protocol::RegistrationEntryForSnark,
    signature_scheme::{DST_LOTTERY, compute_poseidon_digest},
};

use super::SingleSignatureForSnark;

/// A signer for the SNARK proof system, responsible for generating signatures
/// that can be used in SNARK proofs.
#[derive(Debug, Clone)]
pub(crate) struct SnarkProofSigner<D: MembershipDigest> {
    parameters: Parameters,
    signing_key: SchnorrSigningKey,
    verification_key: VerificationKeyForSnark,
    lottery_target_value: LotteryTargetValue,
    key_registration_commitment: MerkleTree<D::SnarkHash, RegistrationEntryForSnark>,
}

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

    /// Computes the lottery prefix by hashing the message with the lottery DST.
    /// The prefix is computed by prepending `DST_LOTTERY` to the message and hashing the result
    /// using `compute_poseidon_digest`.
    fn compute_lottery_prefix(
        message_as_base_field_element: &[BaseFieldElement],
    ) -> BaseFieldElement {
        let mut prefix = vec![DST_LOTTERY];
        prefix.extend_from_slice(message_as_base_field_element);
        compute_poseidon_digest(&prefix)
    }

    /// Verifies if a lottery index is eligible based on the signature and target value.
    ///
    /// This function checks whether a given index wins the lottery by computing an
    /// evaluation value from the signature's commitment point and the index, then
    /// comparing it against the target value. An index is eligible if its
    /// evaluation value is less than or equal to the target.
    ///
    /// The evaluation is computed as:
    /// `ev = Poseidon(prefix, commitment_point_x, commitment_point_y, index)` where
    /// `(commitment_point_x, commitment_point_y)` are coordinates of signature's commitment point.
    fn verify_lottery_eligibility(
        signature: &UniqueSchnorrSignature,
        lottery_index: LotteryIndex,
        m: u64,
        prefix: BaseFieldElement,
        target: LotteryTargetValue,
    ) -> StmResult<()> {
        if lottery_index > m {
            return Err(SignatureError::IndexBoundFailed(lottery_index, m).into());
        }

        let lottery_index_as_base_field_element = BaseFieldElement::from(lottery_index);
        let (commitment_point_x, commitment_point_y) = signature.commitment_point.get_coordinates();
        let lottery_evaluation = compute_poseidon_digest(&[
            prefix,
            commitment_point_x,
            commitment_point_y,
            lottery_index_as_base_field_element,
        ]);

        if lottery_evaluation > target {
            return Err(SignatureError::LotteryLost.into());
        }

        Ok(())
    }

    /// Gets the lottery target value
    pub fn get_lottery_target_value(&self) -> LotteryTargetValue {
        self.lottery_target_value
    }

    /// Gets the verification key for SNARK.
    pub fn get_verification_key(&self) -> VerificationKeyForSnark {
        self.verification_key
    }
}
