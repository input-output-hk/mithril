use std::collections::{BTreeMap, btree_map::Entry};

use crate::{
    AggregationError, ClosedKeyRegistration, LotteryIndex, MembershipDigest, Parameters,
    RegistrationEntryForSnark, Signer, SingleSignature, StmResult,
};

use super::AggregateVerificationKeyForSnark;

/// Clerk for managing the SNARK proof system.
///
/// Responsible for computing the SNARK aggregate verification key from
/// a closed key registration. This is the SNARK counterpart of the
/// `ConcatenationClerk`.
#[derive(Debug, Clone)]
pub struct SnarkClerk {
    /// The closed key registration associated with this clerk.
    pub(crate) closed_key_registration: ClosedKeyRegistration,
    /// Protocol parameters
    pub(crate) parameters: Parameters,
}

impl SnarkClerk {
    /// Create a `SnarkClerk` from a closed key registration and protocol parameters.
    pub fn new_clerk_from_closed_key_registration(
        parameters: &Parameters,
        closed_key_registration: &ClosedKeyRegistration,
    ) -> Self {
        Self {
            parameters: *parameters,
            closed_key_registration: closed_key_registration.clone(),
        }
    }

    /// Create a `SnarkClerk` by extracting the parameters and closed key registration from `Signer`
    pub fn new_clerk_from_signer<D: MembershipDigest>(signer: &Signer<D>) -> Self {
        Self {
            parameters: signer.parameters,
            closed_key_registration: signer.closed_key_registration.clone(),
        }
    }

    /// Compute the `AggregateVerificationKeyForSnark` from the closed key registration.
    pub fn compute_aggregate_verification_key_for_snark<D: MembershipDigest>(
        &self,
    ) -> AggregateVerificationKeyForSnark<D> {
        AggregateVerificationKeyForSnark::from(&self.closed_key_registration)
    }

    /// Look up and convert the registration entry for a signer into its SNARK representation.
    pub fn get_snark_registration_entry(
        &self,
        signer_index: LotteryIndex,
    ) -> StmResult<Option<RegistrationEntryForSnark>> {
        let closed_registration_entry = self
            .closed_key_registration
            .get_registration_entry_for_index(&signer_index)?;
        Ok(closed_registration_entry.into())
    }

    /// Deduplicate signatures by lottery index and select exactly `k` winners.
    ///
    /// When multiple signatures claim the same lottery index, the one with the smallest Schnorr
    /// signature (by `Ord` on `UniqueSchnorrSignature`) is kept. After all signatures have been
    /// processed, the map is trimmed to the `k` smallest lottery indices.
    ///
    /// Returns `AggregationError::NotEnoughSignatures` if fewer than `k` unique winning indices
    /// can be collected.
    pub(crate) fn select_valid_signatures_for_k_indices(
        parameters: &Parameters,
        signatures: &[SingleSignature],
    ) -> StmResult<BTreeMap<LotteryIndex, SingleSignature>> {
        let mut unique_index_signature_map: BTreeMap<LotteryIndex, SingleSignature> =
            BTreeMap::new();

        for signature in signatures {
            let (Some(snark_indices), Some(snark_signature)) = (
                signature.get_snark_signature_indices(),
                signature.snark_signature.as_ref(),
            ) else {
                continue;
            };

            for index in snark_indices {
                match unique_index_signature_map.entry(index) {
                    Entry::Occupied(mut existing) => {
                        if existing.get().snark_signature.as_ref().is_some_and(|s| {
                            s.get_schnorr_signature() > snark_signature.get_schnorr_signature()
                        }) {
                            existing.insert(signature.clone());
                        }
                    }
                    Entry::Vacant(vacant) => {
                        vacant.insert(signature.clone());
                    }
                }
            }
        }

        let count = unique_index_signature_map.len() as u64;
        if count < parameters.k {
            return Err(AggregationError::NotEnoughSignatures(count, parameters.k).into());
        }

        while unique_index_signature_map.len() as u64 > parameters.k {
            unique_index_signature_map.pop_last();
        }

        Ok(unique_index_signature_map)
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};
    use std::collections::HashSet;

    use crate::{
        AggregationError, Initializer, KeyRegistration, MithrilMembershipDigest, Parameters,
        RegistrationEntry, Signer, SingleSignature,
        proof_system::{
            SnarkClerk,
            halo2_snark::{
                AggregateVerificationKeyForSnark, build_snark_message,
                compute_winning_lottery_indices,
            },
        },
    };

    type D = MithrilMembershipDigest;

    fn setup_signers_and_clerk(
        params: Parameters,
        nparties: usize,
        rng: &mut ChaCha20Rng,
    ) -> (Vec<Signer<D>>, SnarkClerk) {
        let mut key_reg = KeyRegistration::initialize();
        let mut initializers = Vec::with_capacity(nparties);

        for i in 0..nparties {
            let stake = (i as u64 + 1) * 10;
            let init = Initializer::new(params, stake, rng);
            let entry = RegistrationEntry::new(
                init.get_verification_key_proof_of_possession_for_concatenation(),
                init.stake,
                #[cfg(feature = "future_snark")]
                init.schnorr_verification_key,
            )
            .unwrap();
            key_reg.register_by_entry(&entry).unwrap();
            initializers.push(init);
        }

        let closed_reg = key_reg.close_registration(&params).unwrap();
        let signers: Vec<Signer<D>> = initializers
            .into_iter()
            .map(|init| init.try_create_signer::<D>(&closed_reg).unwrap())
            .collect();

        let clerk = SnarkClerk::new_clerk_from_signer(&signers[0]);
        (signers, clerk)
    }

    // Collect signatures and populate their SNARK winning indices.
    fn collect_signatures_with_indices(
        signers: &[Signer<D>],
        clerk: &SnarkClerk,
        message: &[u8],
    ) -> Vec<SingleSignature> {
        let avk = clerk.compute_aggregate_verification_key_for_snark::<D>();
        let message_to_sign = build_snark_message(&avk.get_merkle_tree_commitment().root, message)
            .expect("build_snark_message should succeed");

        signers
            .iter()
            .filter_map(|signer| {
                let mut sig = signer.create_single_signature(message).ok()?;
                let snark_sig = sig.snark_signature.as_ref()?;
                let reg_entry =
                    clerk.get_snark_registration_entry(sig.signer_index).ok().flatten()?;
                let indices = compute_winning_lottery_indices(
                    clerk.parameters.m,
                    &message_to_sign,
                    &snark_sig.get_schnorr_signature(),
                    reg_entry.1,
                )
                .ok()?;
                sig.set_snark_signature_indices(&indices);
                Some(sig)
            })
            .collect()
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]

        #[test]
        fn compute_snark_avk(
            seed in any::<[u8; 32]>(),
            number_of_parties in 1_usize..10,
            m in 1_u64..20,
            k in 1_u64..10,
            phi_f in 10_u64..100,
        ) {
            let parameters = Parameters { m, k, phi_f: phi_f as f64 / 100f64 };
            let mut rng = ChaCha20Rng::from_seed(seed);

            let (signers, clerk_from_signer) =
                setup_signers_and_clerk(parameters, number_of_parties, &mut rng);

            let clerk_from_registration = SnarkClerk::new_clerk_from_closed_key_registration(
                &parameters,
                &signers[0].closed_key_registration,
            );

            let avk_from_registration: AggregateVerificationKeyForSnark<D> =
                clerk_from_registration.compute_aggregate_verification_key_for_snark();
            let avk_from_signer: AggregateVerificationKeyForSnark<D> =
                clerk_from_signer.compute_aggregate_verification_key_for_snark();

            let expected_total_stake: u64 = (1..=number_of_parties as u64).map(|i| i * 10).sum();
            prop_assert_eq!(avk_from_registration.get_total_stake(), expected_total_stake);
            prop_assert_eq!(&avk_from_registration, &avk_from_signer);

            let bytes = avk_from_registration.to_bytes();
            let deserialized = AggregateVerificationKeyForSnark::<D>::from_bytes(&bytes)
                .expect("deserialization should succeed");
            prop_assert_eq!(&avk_from_registration, &deserialized);

            let avk_second: AggregateVerificationKeyForSnark<D> =
                clerk_from_registration.compute_aggregate_verification_key_for_snark();
            prop_assert_eq!(&avk_from_registration, &avk_second);
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]

        #[test]
        fn dedup_selects_k_unique_indices(
            seed in any::<[u8; 32]>(),
            msg in any::<[u8; 32]>(),
            nparties in 1_usize..10,
            m in 1_u64..20,
            k in 1_u64..10,
            phi_f in 1_u32..100,
            num_false_msgs in 0_usize..3,
        ) {
            let params = Parameters { m, k, phi_f: (phi_f as f64) / 100.0f64 };
            let mut rng = ChaCha20Rng::from_seed(seed);

            let (signers, clerk) = setup_signers_and_clerk(params, nparties, &mut rng);

            // Collect valid signatures with SNARK indices
            let mut all_sigs = collect_signatures_with_indices(&signers, &clerk, &msg);

            // Also create signatures for false messages
            for _ in 0..num_false_msgs {
                let mut false_msg = vec![0u8; 32];
                rng.fill_bytes(&mut false_msg);
                if false_msg[..msg.len()] == msg[..] {
                    false_msg[0] = msg[0].wrapping_add(1);
                }
                let false_sigs =
                    collect_signatures_with_indices(&signers, &clerk, &false_msg);
                all_sigs.extend(false_sigs);
            }

            let dedup_result =
                SnarkClerk::select_valid_signatures_for_k_indices(&params, &all_sigs);

            match dedup_result {
                Ok(unique_index_map) => {
                    prop_assert!(
                        !unique_index_map.is_empty(),
                        "Should have at least one entry"
                    );

                    // All lottery indices should be unique
                    let count = unique_index_map.len() as u64;
                    prop_assert!(
                        count >= k,
                        "Should have at least k unique indices, got {count} < {k}"
                    );
                    prop_assert_eq!(
                        count, k,
                        "Early stopping should collect exactly k indices"
                    );

                    // Each selected signature should have a SNARK component
                    for (lottery_index, sig) in &unique_index_map {
                        prop_assert!(
                            sig.snark_signature.is_some(),
                            "Signature at lottery index {lottery_index} should have a SNARK component"
                        );

                        // The lottery index should be within valid range
                        prop_assert!(
                            *lottery_index < m,
                            "Lottery index {lottery_index} should be less than m={m}"
                        );
                    }

                    // Verify that unique signers are tracked correctly
                    let signer_indices: HashSet<_> = unique_index_map
                        .values()
                        .map(|sig| sig.signer_index)
                        .collect();
                    prop_assert!(
                        !signer_indices.is_empty(),
                        "Should have at least one distinct signer"
                    );
                }
                Err(error) => {
                    prop_assert!(
                        matches!(
                            error.downcast_ref::<AggregationError>(),
                            Some(AggregationError::NotEnoughSignatures(..))
                        ),
                        "Expected NotEnoughSignatures, got: {error:?}"
                    );
                }
            }
        }
    }
}
