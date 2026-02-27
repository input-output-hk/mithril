use anyhow::Context;
use serde::{Deserialize, Serialize};

use crate::{
    LotteryIndex, LotteryTargetValue, MembershipDigest, Parameters, StmResult,
    UniqueSchnorrSignature, VerificationKeyForSnark, proof_system::halo2_snark::check_lottery,
};

use super::{AggregateVerificationKeyForSnark, build_snark_message};

/// Single signature for the Snark proof system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SingleSignatureForSnark {
    /// The underlying Schnorr signature
    schnorr_signature: UniqueSchnorrSignature,
    /// The minimum winning lottery index for which the signature is valid
    indices: Vec<LotteryIndex>,
}

impl SingleSignatureForSnark {
    /// Create and return a new instance of `SingleSignatureForSnark` for given `schnorr_signature`
    /// and `minimum_winning_lottery_index`.
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
        let message_to_verify = build_snark_message(
            &aggregate_verification_key.get_merkle_tree_commitment().root,
            message,
        )?;
        self.schnorr_signature
            .verify(&message_to_verify, verification_key)
            .with_context(|| "Schnorr signature verification failed for SNARK proof system.")?;

        check_lottery(
            parameters.m,
            &message_to_verify,
            &self.schnorr_signature,
            *lottery_target_value,
        )?;

        Ok(())
    }

    /// Return `indices` of the single signature
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    pub(crate) fn get_indices(&self) -> Vec<LotteryIndex> {
        self.indices.clone()
    }

    /// Set `indices` of single signature to given value
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    pub(crate) fn set_indices(&mut self, indices: Vec<LotteryIndex>) {
        self.indices = indices;
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
        signature_scheme::{BaseFieldElement, BlsSigningKey, SchnorrSigningKey},
    };

    use super::AggregateVerificationKeyForSnark;

    type D = MithrilMembershipDigest;

    #[test]
    fn lottery_check_enforced_during_verification() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let params = Parameters {
            m: 100,
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
            .to_merkle_tree::<<D as MembershipDigest>::SnarkHash, RegistrationEntryForSnark>()
            .to_merkle_tree_commitment();
        let target = ClosedRegistrationEntry::from((entry, closed_reg.total_stake))
            .get_lottery_target_value()
            .unwrap();

        let signer =
            SnarkProofSigner::<D>::new(params, schnorr_sk, schnorr_vk, target, merkle_tree);
        let avk = AggregateVerificationKeyForSnark::<D>::from(&closed_reg);

        let msg = [0u8; 32];
        let sig = signer.create_single_signature(&msg, &mut rng).unwrap();

        // Verification should pass with the original (generous) target
        sig.verify::<D>(&params, &signer.get_verification_key(), &msg, &target, &avk)
            .expect("Verification should pass with the original target");

        // Verification fails with target = 0: the Schnorr signature remains valid
        // but no lottery index can win, proving verify enforces the lottery.
        let zero_target = BaseFieldElement::from(0u64);
        let err = sig
            .verify::<D>(
                &params,
                &signer.get_verification_key(),
                &msg,
                &zero_target,
                &avk,
            )
            .expect_err("Valid Schnorr signature should still fail with target = 0");
        assert!(
            matches!(
                err.downcast_ref::<SignatureError>(),
                Some(SignatureError::LotteryLost)
            ),
            "Expected LotteryLost, got: {err:?}"
        );
    }
}
