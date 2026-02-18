mod aggregate_key;
mod eligibility;
mod signer;
mod single_signature;

pub(crate) use aggregate_key::AggregateVerificationKeyForSnark;
pub(crate) use signer::SnarkProofSigner;
pub(crate) use single_signature::SingleSignatureForSnark;

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        ClosedRegistrationEntry, KeyRegistration, MithrilMembershipDigest, Parameters,
        RegistrationEntry, SignatureError, VerificationKeyForSnark,
        VerificationKeyProofOfPossessionForConcatenation,
        protocol::RegistrationEntryForSnark,
        signature_scheme::{BlsSigningKey, SchnorrSigningKey},
    };

    use super::{AggregateVerificationKeyForSnark, SingleSignatureForSnark, SnarkProofSigner};

    type D = MithrilMembershipDigest;

    /// Setup a `SnarkProofSigner` and `AggregateVerificationKeyForSnark` from a given
    /// number of parties, protocol parameters, and a deterministic RNG.
    ///
    /// Returns the signer for the first registered party along with the aggregate
    /// verification key derived from the full registration.
    fn setup_snark_signer(
        params: Parameters,
        nparties: usize,
        rng: &mut ChaCha20Rng,
    ) -> (SnarkProofSigner<D>, AggregateVerificationKeyForSnark<D>) {
        let mut key_reg = KeyRegistration::initialize();

        let mut first_schnorr_sk = None;
        let mut first_schnorr_vk = None;
        let mut first_entry = None;

        for i in 0..nparties {
            let bls_sk = BlsSigningKey::generate(rng);
            let bls_vk = VerificationKeyProofOfPossessionForConcatenation::from(&bls_sk);
            let schnorr_sk = SchnorrSigningKey::generate(rng);
            let schnorr_vk = VerificationKeyForSnark::new_from_signing_key(schnorr_sk.clone());

            let entry = RegistrationEntry::new(
                bls_vk,
                1,
                #[cfg(feature = "future_snark")]
                Some(schnorr_vk),
            )
            .unwrap();
            key_reg.register_by_entry(&entry).unwrap();

            if i == 0 {
                first_schnorr_sk = Some(schnorr_sk);
                first_schnorr_vk = Some(schnorr_vk);
                first_entry = Some(entry);
            }
        }

        let closed_reg = key_reg.close_registration();
        let entry = first_entry.unwrap();

        let merkle_tree = closed_reg
            .to_merkle_tree::<<D as crate::MembershipDigest>::SnarkHash, RegistrationEntryForSnark>(
            );
        let lottery_target_value = ClosedRegistrationEntry::from((entry, closed_reg.total_stake))
            .get_lottery_target_value()
            .unwrap();

        let snark_signer = SnarkProofSigner::<D>::new(
            params,
            first_schnorr_sk.unwrap(),
            first_schnorr_vk.unwrap(),
            lottery_target_value,
            merkle_tree,
        );

        let aggregate_key = AggregateVerificationKeyForSnark::<D>::from(&closed_reg);

        (snark_signer, aggregate_key)
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        fn sign_then_verify_roundtrip(
            nparties in 2_usize..10,
            m in 10_u64..20,
            k in 1_u64..5,
            msg in any::<[u8; 16]>(),
            seed in any::<[u8; 32]>(),
        ) {
            let params = Parameters { m, k, phi_f: 0.2 };
            let mut rng = ChaCha20Rng::from_seed(seed);
            let (signer, avk) = setup_snark_signer(params, nparties, &mut rng);

            let sig = signer.create_single_signature(&msg, &mut rng).unwrap();

            sig.verify::<D>(
                &params,
                &signer.get_verification_key(),
                &msg,
                &signer.get_lottery_target_value(),
                &avk,
            ).unwrap();

            // Check winning index is in bounds
            assert!(sig.get_minimum_winning_lottery_index() < m);

            // Check winning index is minimal, no smaller index should win the lottery
            let message_to_verify = avk
                .get_merkle_tree_commitment()
                .build_snark_message(&msg)
                .unwrap();
            let lottery_prefix =
                SnarkProofSigner::<D>::compute_lottery_prefix(&message_to_verify);
            for i in 0..sig.get_minimum_winning_lottery_index() {
                let err = SnarkProofSigner::<D>::verify_lottery_eligibility(
                    &sig.get_schnorr_signature(),
                    i,
                    m,
                    lottery_prefix,
                    signer.get_lottery_target_value(),
                )
                .expect_err("Index below minimum should not win the lottery");
                assert!(
                    matches!(
                        err.downcast_ref::<SignatureError>(),
                        Some(SignatureError::LotteryLost)
                    ),
                    "Expected LotteryLost for index {i}, got: {err:?}"
                );
            }
        }

        #[test]
        fn wrong_message_fails_verification(
            nparties in 2_usize..10,
            m in 10_u64..20,
            k in 1_u64..5,
            msg1 in any::<[u8; 16]>(),
            msg2 in any::<[u8; 16]>(),
            seed in any::<[u8; 32]>(),
        ) {
            prop_assume!(msg1 != msg2);
            let params = Parameters { m, k, phi_f: 0.2 };
            let mut rng = ChaCha20Rng::from_seed(seed);
            let (signer, avk) = setup_snark_signer(params, nparties, &mut rng);

            let sig = signer.create_single_signature(&msg1, &mut rng).unwrap();

            sig.verify::<D>(
                &params,
                &signer.get_verification_key(),
                &msg2,
                &signer.get_lottery_target_value(),
                &avk,
            )
            .expect_err("Verification with wrong message should fail");
        }

        #[test]
        fn wrong_verification_key_fails(
            nparties in 2_usize..10,
            m in 10_u64..20,
            k in 1_u64..5,
            msg in any::<[u8; 16]>(),
            seed in any::<[u8; 32]>(),
        ) {
            let params = Parameters { m, k, phi_f: 0.2 };
            let mut rng = ChaCha20Rng::from_seed(seed);
            let (signer, avk) = setup_snark_signer(params, nparties, &mut rng);
            let sig = signer.create_single_signature(&msg, &mut rng).unwrap();

            let wrong_sk = SchnorrSigningKey::generate(&mut rng);
            let wrong_vk = VerificationKeyForSnark::new_from_signing_key(wrong_sk);

            sig.verify::<D>(
                &params,
                &wrong_vk,
                &msg,
                &signer.get_lottery_target_value(),
                &avk,
            )
            .expect_err("Verification with wrong key should fail");
        }

        #[test]
        fn serde_roundtrip(
            nparties in 2_usize..10,
            m in 10_u64..20,
            k in 1_u64..5,
            msg in any::<[u8; 16]>(),
            seed in any::<[u8; 32]>(),
        ) {
            let params = Parameters { m, k, phi_f: 0.2 };
            let mut rng = ChaCha20Rng::from_seed(seed);
            let (signer, _) = setup_snark_signer(params, nparties, &mut rng);

            let sig = signer.create_single_signature(&msg, &mut rng).unwrap();

            let serialized = serde_json::to_string(&sig).unwrap();
            let deserialized: SingleSignatureForSnark =
                serde_json::from_str(&serialized).unwrap();

            // Check both fields explicitly
            assert_eq!(
                sig.get_schnorr_signature(),
                deserialized.get_schnorr_signature()
            );
            assert_eq!(
                sig.get_minimum_winning_lottery_index(),
                deserialized.get_minimum_winning_lottery_index()
            );
        }
    }
}
