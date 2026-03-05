use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{
    AggregationError, ClosedKeyRegistration, LotteryIndex, MembershipDigest, Parameters,
    RegistrationEntryForSnark, Signer, StmResult,
};

use super::{AggregateVerificationKeyForSnark, witness::SignatureRegistrationEntry};

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

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl SnarkClerk {
    /// Create a new `SnarkClerk` from a closed registration instance.
    pub fn new_clerk_from_closed_key_registration(
        parameters: &Parameters,
        closed_key_registration: &ClosedKeyRegistration,
    ) -> Self {
        Self {
            parameters: *parameters,
            closed_key_registration: closed_key_registration.clone(),
        }
    }

    /// Create a `SnarkClerk` from a signer.
    pub fn new_clerk_from_signer<D: MembershipDigest>(signer: &Signer<D>) -> Self {
        Self {
            parameters: signer.parameters,
            closed_key_registration: signer.closed_key_registration.clone(),
        }
    }

    /// Compute the `AggregateVerificationKeyForSnark` related to the used registration.
    pub fn compute_aggregate_verification_key_for_snark<D: MembershipDigest>(
        &self,
    ) -> AggregateVerificationKeyForSnark<D> {
        AggregateVerificationKeyForSnark::from(&self.closed_key_registration)
    }

    /// Get the SNARK registration entry for a given signer index.
    pub fn get_snark_registration_entry(
        &self,
        signer_index: LotteryIndex,
    ) -> StmResult<Option<RegistrationEntryForSnark>> {
        let closed_registration_entry = self
            .closed_key_registration
            .get_registration_entry_for_index(&signer_index)?;
        Ok(closed_registration_entry.into())
    }

    /// Modifications:
    /// Function inputs: remove `msg`, replace `sigs: &[SingleSignatureWithRegisteredParty]` with
    /// `signatures: &[SignatureRegistrationEntry]`, remove avk.
    /// Return value: `StmResult<Vec<SignatureRegistrationEntry>`.
    /// Remove signature verification loop -> already done in aggregation step.
    /// Rename `sig_reg` as `entry` for the first loop, since we iterate over entries now.
    /// Rename `sig_reg` as `entry` for the second loop, since we iterate over entries now.
    ///
    /// Note: schnorr sig, scalar field element, projective point are updated to satisfy `Hash`,
    /// `Ord`, `PartialOrd`.
    pub(crate) fn select_valid_signatures_for_k_indices(
        parameters: &Parameters,
        signatures: &[SignatureRegistrationEntry],
    ) -> StmResult<Vec<SignatureRegistrationEntry>> {
        let mut sig_by_index: BTreeMap<LotteryIndex, &SignatureRegistrationEntry> = BTreeMap::new();
        let mut removal_idx_by_vk: HashMap<&SignatureRegistrationEntry, Vec<LotteryIndex>> =
            HashMap::new();

        for entry in signatures.iter() {
            for index in entry.get_signature().get_indices().iter() {
                let mut insert_this_sig = false;
                if let Some(&previous_entry) = sig_by_index.get(index) {
                    let entry_to_remove_index = if entry.get_signature().get_schnorr_signature()
                        < previous_entry.get_signature().get_schnorr_signature()
                    {
                        insert_this_sig = true;
                        previous_entry
                    } else {
                        entry
                    };

                    if let Some(indexes) = removal_idx_by_vk.get_mut(entry_to_remove_index) {
                        indexes.push(*index);
                    } else {
                        removal_idx_by_vk.insert(entry_to_remove_index, vec![*index]);
                    }
                } else {
                    insert_this_sig = true;
                }

                if insert_this_sig {
                    sig_by_index.insert(*index, entry);
                }
            }
        }

        let mut dedup_sigs: HashSet<SignatureRegistrationEntry> = HashSet::new();
        let mut count: u64 = 0;

        for (_, &entry) in sig_by_index.iter() {
            if dedup_sigs.contains(entry) {
                continue;
            }
            let mut deduped_entry = entry.clone();
            if let Some(indexes) = removal_idx_by_vk.get(entry) {
                let indices = deduped_entry
                    .get_signature()
                    .get_indices()
                    .iter()
                    .copied()
                    .filter(|i| !indexes.contains(i))
                    .collect::<Vec<LotteryIndex>>();
                deduped_entry.set_indices(&indices);
            }

            let size: Result<u64, _> = deduped_entry.get_signature().get_indices().len().try_into();
            if let Ok(size) = size {
                if dedup_sigs.contains(&deduped_entry) {
                    panic!(
                        "Invariant violation: duplicate signature encountered in deduplication set, which should not be possible."
                    );
                }
                dedup_sigs.insert(deduped_entry);
                count += size;

                if count >= parameters.k {
                    return Ok(dedup_sigs.into_iter().collect());
                }
            }
        }
        Err(AggregationError::NotEnoughSignatures(count, parameters.k).into())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};

    use crate::{
        ClosedKeyRegistration, Initializer, KeyRegistration, MithrilMembershipDigest, Parameters,
        RegistrationEntry, Signer,
        proof_system::{
            SnarkClerk,
            halo2_snark::{
                AggregateVerificationKeyForSnark, build_snark_message,
                compute_winning_lottery_indices,
            },
        },
    };

    use super::{AggregationError, SignatureRegistrationEntry};

    type D = MithrilMembershipDigest;

    // NOTE: `phi_f` is hardcoded to `0.5` in these tests but has no real effect on the SNARK
    // witness because `LotteryTargetValue` is currently set to `p-1` in
    // `ClosedRegistrationEntry::from(RegistrationEntry, Stake)`, making every signer win every
    // lottery index regardless of `phi_f`. These tests will become more meaningful once ticket
    // 3068 is merged, which replaces the `p-1` default with the actual
    // `compute_lottery_target_value(stake, total_stake)` computation (currently commented out in
    // `closed_registration_entry.rs`).
    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        fn test_dedup(
            seed in any::<[u8; 32]>(),
            msg in any::<[u8; 32]>(),
            nparties in 1_usize..10,
            m in 1_u64..20,
            k in 1_u64..10,
            num_invalid_sigs_per_party in 0_usize..3,
        ) {
            let params = Parameters { m, k, phi_f: 0.5 };
            let mut rng = ChaCha20Rng::from_seed(seed);

            // False messages
            let mut false_msgs = Vec::new();
            for _ in 0..num_invalid_sigs_per_party {
                let mut false_msg = vec![0u8; 32];
                rng.fill_bytes(&mut false_msg);
                if false_msg == msg {
                    false_msg[0] = msg[0].wrapping_add(1);
                }
                false_msgs.push(false_msg);
            }

            let mut key_registration = KeyRegistration::initialize();
            let mut initializers = Vec::new();

            for i in 0..nparties {
                let stake = (i as u64 + 1) * 10;
                let initializer = Initializer::new(params, stake, &mut rng);
                key_registration
                    .register_by_entry(&initializer.clone().try_into().unwrap())
                    .unwrap();
                initializers.push(initializer);
            }

            let closed_registration: ClosedKeyRegistration = key_registration.close_registration(&params).unwrap();

            let signers: Vec<Signer<D>> = initializers
                .into_iter()
                .map(|init| init.try_create_signer(&closed_registration).unwrap())
                .collect();

            let clerk = SnarkClerk::new_clerk_from_signer::<D>(&signers[0]);
            let avk: AggregateVerificationKeyForSnark<D> =
                clerk.compute_aggregate_verification_key_for_snark();
            let message_to_sign =
                build_snark_message(&avk.get_merkle_tree_commitment().root, &msg).unwrap();

            // Build SignatureRegistrationEntry list: sign with valid + invalid messages,
            // verify, compute winning indices, and retain only valid entries.
            let mut sig_reg_list: Vec<SignatureRegistrationEntry> = Vec::new();

            for signer in &signers {
                let all_msgs: Vec<&[u8]> = false_msgs
                    .iter()
                    .map(|m| m.as_slice())
                    .chain(std::iter::once(msg.as_slice()))
                    .collect();

                for sign_msg in all_msgs {
                    let single_sig = signer.create_single_signature(sign_msg);
                    if let Ok(sig) = single_sig
                        && let Some(snark_sig) = sig.snark_signature.clone()
                        && let Ok(Some(reg_entry)) =
                            clerk.get_snark_registration_entry(sig.signer_index)
                    {
                        let mut entry =
                            SignatureRegistrationEntry::new(snark_sig, reg_entry);
                        // Verify and compute indices (mirrors aggregate_signatures flow)
                        if entry
                            .get_signature()
                            .verify(&reg_entry.0, sign_msg, &avk)
                            .is_ok()
                            && let Ok(indices) = compute_winning_lottery_indices(
                                params.m,
                                &message_to_sign,
                                &entry.get_signature().get_schnorr_signature(),
                                reg_entry.1,
                            )
                        {
                            entry.set_indices(&indices);
                            sig_reg_list.push(entry);
                        }
                    }
                }
            }

            let dedup_result =
                SnarkClerk::select_valid_signatures_for_k_indices(&params, &sig_reg_list);

            match dedup_result {
                Ok(valid_sigs) => {
                    assert!(!valid_sigs.is_empty(), "Should have at least one valid signature");

                    // Check that indices are unique and at least k unique indices are collected
                    let mut unique_indices = HashSet::new();
                    let mut count: u64 = 0;
                    for entry in valid_sigs.iter() {
                        for index in entry.get_signature().get_indices().iter() {
                            count += 1;
                            unique_indices.insert(*index);
                        }
                    }
                    assert_eq!(
                        unique_indices.len() as u64, count,
                        "Indices should be unique"
                    );
                    assert!(count >= k, "Should have at least k unique indices");
                }
                Err(error) => {
                    assert!(
                        matches!(
                            error.downcast_ref::<AggregationError>(),
                            Some(AggregationError::NotEnoughSignatures(..))
                        ),
                        "Expected NotEnoughSignatures, got: {:?}",
                        error
                    );
                }
            }
        }
    }
    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        fn compute_snark_avk(
            seed in any::<[u8; 32]>(),
            number_of_parties in 1_usize..10,
            m in 1_u64..20,
            k in 1_u64..10,
            phi_f in 0.1_f64..1.0,
        ) {
            let parameters = Parameters { m, k, phi_f };
            let mut rng = ChaCha20Rng::from_seed(seed);

            let mut key_registration = KeyRegistration::initialize();
            let mut initializers = Vec::new();

            for i in 0..number_of_parties {
                let stake = (i as u64 + 1) * 10;
                let initializer = Initializer::new(parameters, stake, &mut rng);
                let entry = RegistrationEntry::new(
                    initializer.get_verification_key_proof_of_possession_for_concatenation(),
                    initializer.stake,
                    #[cfg(feature = "future_snark")]
                    initializer.schnorr_verification_key,
                )
                .unwrap();
                key_registration.register_by_entry(&entry).unwrap();
                initializers.push(initializer);
            }

            let closed_registration = key_registration.close_registration(&parameters).unwrap();

            let signers: Vec<_> = initializers
                .into_iter()
                .map(|init| init.try_create_signer::<D>(&closed_registration).unwrap())
                .collect();

            let clerk_from_registration =
                SnarkClerk::new_clerk_from_closed_key_registration(&parameters, &closed_registration);
            let clerk_from_signer = SnarkClerk::new_clerk_from_signer::<D>(&signers[0]);

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
}
