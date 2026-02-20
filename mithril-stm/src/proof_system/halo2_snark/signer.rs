use rand_core::{CryptoRng, RngCore};

use crate::{
    BaseFieldElement, LotteryIndex, LotteryTargetValue, MembershipDigest, Parameters,
    SchnorrSigningKey, SignatureError, StmResult, UniqueSchnorrSignature, VerificationKeyForSnark,
    membership_commitment::MerkleTreeCommitment, protocol::RegistrationEntryForSnark,
};

use super::{SingleSignatureForSnark, compute_lottery_prefix, verify_lottery_eligibility};

/// A signer for the SNARK proof system, responsible for generating signatures
/// that can be used in SNARK proofs.
#[derive(Debug, Clone)]
pub(crate) struct SnarkProofSigner<D: MembershipDigest> {
    parameters: Parameters,
    signing_key: SchnorrSigningKey,
    verification_key: VerificationKeyForSnark,
    lottery_target_value: LotteryTargetValue,
    key_registration_commitment: MerkleTreeCommitment<D::SnarkHash, RegistrationEntryForSnark>,
}

impl<D: MembershipDigest> SnarkProofSigner<D> {
    /// Creates a new `SnarkProofSigner` with the given protocol parameters, keys,
    /// lottery target value, and Merkle tree commitment of the key registration.
    pub fn new(
        parameters: Parameters,
        signing_key: SchnorrSigningKey,
        verification_key: VerificationKeyForSnark,
        lottery_target_value: LotteryTargetValue,
        key_registration_commitment: MerkleTreeCommitment<D::SnarkHash, RegistrationEntryForSnark>,
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
    /// Computes a unique Schnorr signature over `[merkle_root, msg]` and checks the
    /// lottery. Returns `SingleSignatureForSnark` if at least one lottery index won,
    /// or `SignatureError::LotteryLost` otherwise.
    pub fn create_single_signature<R: RngCore + CryptoRng>(
        &self,
        message: &[u8],
        rng: &mut R,
    ) -> StmResult<SingleSignatureForSnark> {
        let message_to_sign = self.key_registration_commitment.build_snark_message(message)?;
        let signature = self.signing_key.sign(&message_to_sign, rng)?;

        let first_winning_index = Self::check_lottery(
            self.parameters.m,
            &message_to_sign,
            &signature,
            self.lottery_target_value,
        )?;

        Ok(SingleSignatureForSnark::new(signature, first_winning_index))
    }

    /// Checks the lottery for all indices `0..m`.
    ///
    /// Computes the lottery prefix from the message, then iterates over each index
    /// to verify eligibility. Returns the first winning index, or
    /// `SignatureError::LotteryLost` if no index is won.
    fn check_lottery(
        m: u64,
        msg: &[BaseFieldElement],
        signature: &UniqueSchnorrSignature,
        lottery_target_value: LotteryTargetValue,
    ) -> StmResult<LotteryIndex> {
        let lottery_prefix = compute_lottery_prefix(msg);

        (0..m)
            .find(|&index| {
                verify_lottery_eligibility(
                    signature,
                    index,
                    m,
                    lottery_prefix,
                    lottery_target_value,
                )
                .is_ok()
            })
            .ok_or_else(|| SignatureError::LotteryLost.into())
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

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        KeyRegistration, MembershipDigest, MithrilMembershipDigest, Parameters, RegistrationEntry,
        SignatureError, VerificationKeyForSnark, VerificationKeyProofOfPossessionForConcatenation,
        membership_commitment::{MerkleTreeCommitment, MerkleTreeSnarkLeaf},
        protocol::RegistrationEntryForSnark,
        signature_scheme::{
            BaseFieldElement, BlsSigningKey, SchnorrSigningKey, compute_poseidon_digest,
        },
    };

    use super::{SnarkProofSigner, compute_lottery_prefix, verify_lottery_eligibility};

    type D = MithrilMembershipDigest;

    /// Helper function to create a `SnarkProofSigner` with a single registered party and a custom lottery target.
    fn setup_signer_with_target(
        target: BaseFieldElement,
        rng: &mut ChaCha20Rng,
    ) -> SnarkProofSigner<D> {
        let params = Parameters {
            m: 10,
            k: 5,
            phi_f: 0.2,
        };
        let bls_sk = BlsSigningKey::generate(rng);
        let bls_vk = VerificationKeyProofOfPossessionForConcatenation::from(&bls_sk);
        let schnorr_sk = SchnorrSigningKey::generate(rng);
        let schnorr_vk = VerificationKeyForSnark::new_from_signing_key(schnorr_sk.clone());

        let mut key_reg = KeyRegistration::initialize();
        let entry = RegistrationEntry::new(
            bls_vk,
            1,
            #[cfg(feature = "future_snark")]
            Some(schnorr_vk),
        )
        .unwrap();
        key_reg.register_by_entry(&entry).unwrap();
        let closed_reg = key_reg.close_registration();

        let merkle_tree = closed_reg
            .to_merkle_tree::<<D as MembershipDigest>::SnarkHash, RegistrationEntryForSnark>()
            .to_merkle_tree_commitment();

        SnarkProofSigner::<D>::new(params, schnorr_sk, schnorr_vk, target, merkle_tree)
    }

    #[test]
    fn lottery_prefix_differs_for_one_bit_change() {
        type SnarkHash = <D as MembershipDigest>::SnarkHash;

        let root = [0u8; 32];
        let msg = [0u8; 32];

        let commitment =
            MerkleTreeCommitment::<SnarkHash, MerkleTreeSnarkLeaf>::from_bytes(&root).unwrap();
        let message_to_sign = commitment.build_snark_message(&msg).unwrap();
        let prefix1 = compute_lottery_prefix(&message_to_sign);

        // Flip bit 0 of the commitment root
        let mut root_flipped = root;
        root_flipped[0] ^= 1;
        let commitment_flipped =
            MerkleTreeCommitment::<SnarkHash, MerkleTreeSnarkLeaf>::from_bytes(&root_flipped)
                .unwrap();
        let message_to_sign_flipped = commitment_flipped.build_snark_message(&msg).unwrap();
        let prefix2 = compute_lottery_prefix(&message_to_sign_flipped);

        assert_ne!(prefix1, prefix2);
    }

    #[test]
    fn index_bound_check() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let msg = [BaseFieldElement::from(1u64)];
        let signature = sk.sign(&msg, &mut rng).unwrap();

        let prefix = BaseFieldElement::from(0u64);
        let target = BaseFieldElement::from(u64::MAX);
        let m = 10u64;

        let err = verify_lottery_eligibility(&signature, m + 1, m, prefix, target)
            .expect_err("Index above m should fail");
        assert!(
            matches!(
                err.downcast_ref::<SignatureError>(),
                Some(SignatureError::IndexBoundFailed(idx, bound)) if *idx == m + 1 && *bound == m
            ),
            "Expected IndexBoundFailed({}, {m}), got: {err:?}",
            m + 1
        );
    }

    #[test]
    fn evaluation_boundary() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let msg = [BaseFieldElement::from(42u64)];
        let signature = sk.sign(&msg, &mut rng).unwrap();

        let prefix = compute_lottery_prefix(&msg);
        let lottery_index = 5u64;
        let m = 10u64;

        // Compute the evaluation value manually
        let (cx, cy) = signature.commitment_point.get_coordinates();
        let ev = compute_poseidon_digest(&[prefix, cx, cy, BaseFieldElement::from(lottery_index)]);

        // With target = ev, the lottery should pass
        assert!(
            verify_lottery_eligibility(&signature, lottery_index, m, prefix, ev,).is_ok(),
            "Lottery should pass when evaluation equals target"
        );
    }

    #[test]
    fn target_zero_means_lottery_lost() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let target = BaseFieldElement::from(0u64);
        let signer = setup_signer_with_target(target, &mut rng);

        let msg = [0u8; 32];
        let err = signer
            .create_single_signature(&msg, &mut rng)
            .expect_err("Lottery with target = 0 should lose");
        assert!(
            matches!(
                err.downcast_ref::<SignatureError>(),
                Some(SignatureError::LotteryLost)
            ),
            "Expected LotteryLost, got: {err:?}"
        );
    }

    #[test]
    fn target_max_guarantees_win() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        // p - 1 is the maximum field element: 0 - 1 in the field wraps to p - 1
        let target = &BaseFieldElement::from(0u64) - &BaseFieldElement::from(1u64);
        let signer = setup_signer_with_target(target, &mut rng);

        let msg = [0u8; 32];
        assert!(
            signer.create_single_signature(&msg, &mut rng).is_ok(),
            "Lottery with target = p - 1 should always win"
        );
    }
}
