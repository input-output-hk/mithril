use anyhow::Context;
use serde::{Deserialize, Serialize};

use crate::{
    LotteryIndex, LotteryTargetValue, MembershipDigest, Parameters, StmResult,
    UniqueSchnorrSignature, VerificationKeyForSnark, signature_scheme::BaseFieldElement,
};

use super::{AggregateVerificationKeyForSnark, compute_lottery_prefix, verify_lottery_eligibility};

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
            *lottery_target_value,
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
        let lottery_prefix = compute_lottery_prefix(message_to_verify);
        verify_lottery_eligibility(
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

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        ClosedRegistrationEntry, KeyRegistration, MembershipDigest, MithrilMembershipDigest,
        Parameters, RegistrationEntry, SignatureError, VerificationKeyForSnark,
        VerificationKeyProofOfPossessionForConcatenation,
        proof_system::halo2_snark::SnarkProofSigner,
        protocol::RegistrationEntryForSnark,
        signature_scheme::{
            BaseFieldElement, BlsSigningKey, SchnorrSigningKey, compute_poseidon_digest,
        },
    };

    use super::{AggregateVerificationKeyForSnark, compute_lottery_prefix};

    type D = MithrilMembershipDigest;

    #[test]
    fn tampered_lottery_index_fails() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let params = Parameters {
            m: 10,
            k: 5,
            phi_f: 0.2,
        };

        // Setup single-party registration with target = p-1 (all indices win)
        let bls_sk = BlsSigningKey::generate(&mut rng);
        let bls_vk = VerificationKeyProofOfPossessionForConcatenation::from(&bls_sk);
        let schnorr_sk = SchnorrSigningKey::generate(&mut rng);
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
            .to_merkle_tree::<<D as MembershipDigest>::SnarkHash, RegistrationEntryForSnark>();
        let target = ClosedRegistrationEntry::from((entry, closed_reg.total_stake))
            .get_lottery_target_value()
            .unwrap();

        let signer =
            SnarkProofSigner::<D>::new(params, schnorr_sk, schnorr_vk, target, merkle_tree);
        let avk = AggregateVerificationKeyForSnark::<D>::from(&closed_reg);

        let msg = [0u8; 16];
        let mut sig = signer.create_single_signature(&msg, &mut rng).unwrap();
        let original_index = sig.get_minimum_winning_lottery_index();

        // Compute evaluation for the original winning index
        let message_to_verify = avk.get_merkle_tree_commitment().build_snark_message(&msg).unwrap();
        let prefix = compute_lottery_prefix(&message_to_verify);
        let (cx, cy) = sig.get_schnorr_signature().commitment_point.get_coordinates();
        let ev_original =
            compute_poseidon_digest(&[prefix, cx, cy, BaseFieldElement::from(original_index)]);

        // Find an index whose evaluation exceeds ev_original
        let tampered_index = (0..params.m)
            .find(|&i| {
                i != original_index && {
                    let ev = compute_poseidon_digest(&[prefix, cx, cy, BaseFieldElement::from(i)]);
                    ev > ev_original
                }
            })
            .expect("Should find at least one index with a larger evaluation");

        sig.set_minimum_winning_lottery_index(tampered_index);

        // Verify with target = ev_original: the tampered index's evaluation exceeds the target,
        // so the lottery check fails while the Schnorr signature remains valid.
        let err = sig
            .verify::<D>(
                &params,
                &signer.get_verification_key(),
                &msg,
                &ev_original,
                &avk,
            )
            .expect_err("Tampered lottery index should fail verification");
        assert!(
            matches!(
                err.downcast_ref::<SignatureError>(),
                Some(SignatureError::LotteryLost)
            ),
            "Expected LotteryLost, got: {err:?}"
        );
    }
}
