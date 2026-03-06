use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{
    AggregationError, ClosedKeyRegistration, LotteryIndex, MembershipDigest, Parameters,
    RegistrationEntryForSnark, Signer, StmResult, proof_system::SingleSignatureForSnark,
};

use super::AggregateVerificationKeyForSnark;

/// The `SnarkClerk` is responsible for managing the proof system related to
/// SNARK signatures.
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
    /// `signatures: &[SingleSignatureForSnark]`, remove avk.
    /// Return value: `StmResult<Vec<SingleSignatureForSnark>`.
    /// Remove signature verification loop -> already done in aggregation step.
    /// Rename `sig_reg` as `signature` for the first loop, since we iterate over signatures now.
    /// Rename `sig_reg` as `signature` for the second loop, since we iterate over signatures now.
    ///
    /// Note: schnorr sig, scalar field element, projective point are updated to satisfy `Hash`,
    /// `Ord`, `PartialOrd`.
    pub(crate) fn select_valid_signatures_for_k_indices(
        parameters: &Parameters,
        signatures: &[SingleSignatureForSnark],
    ) -> StmResult<Vec<SingleSignatureForSnark>> {
        let mut sig_by_index: BTreeMap<LotteryIndex, &SingleSignatureForSnark> = BTreeMap::new();
        let mut removal_idx_by_vk: HashMap<&SingleSignatureForSnark, Vec<LotteryIndex>> =
            HashMap::new();

        for signature in signatures.iter() {
            for index in signature.get_indices().iter() {
                let mut insert_this_sig = false;
                if let Some(&previous_sig) = sig_by_index.get(index) {
                    let sig_to_remove_index = if signature.get_schnorr_signature()
                        < previous_sig.get_schnorr_signature()
                    {
                        insert_this_sig = true;
                        previous_sig
                    } else {
                        signature
                    };

                    if let Some(indexes) = removal_idx_by_vk.get_mut(sig_to_remove_index) {
                        indexes.push(*index);
                    } else {
                        removal_idx_by_vk.insert(sig_to_remove_index, vec![*index]);
                    }
                } else {
                    insert_this_sig = true;
                }

                if insert_this_sig {
                    sig_by_index.insert(*index, signature);
                }
            }
        }

        let mut dedup_sigs: HashSet<SingleSignatureForSnark> = HashSet::new();
        let mut count: u64 = 0;

        for (_, &signature) in sig_by_index.iter() {
            if dedup_sigs.contains(signature) {
                continue;
            }
            let mut deduped_sig = signature.clone();
            if let Some(indexes) = removal_idx_by_vk.get(signature) {
                let indices = deduped_sig
                    .get_indices()
                    .into_iter()
                    .filter(|i| !indexes.contains(i))
                    .collect::<Vec<LotteryIndex>>();
                deduped_sig.set_indices(&indices);
            }

            let size: Result<u64, _> = deduped_sig.get_indices().len().try_into();
            if let Ok(size) = size {
                if dedup_sigs.contains(&deduped_sig) {
                    panic!(
                        "Invariant violation: duplicate signature encountered in deduplication set, which should not be possible."
                    );
                }
                dedup_sigs.insert(deduped_sig);
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

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        Initializer, KeyRegistration, LotteryIndex, LotteryTargetValue, MithrilMembershipDigest,
        Parameters, RegistrationEntry, Signer,
        proof_system::{
            AggregateVerificationKeyForSnark, SingleSignatureForSnark, SnarkClerk,
            halo2_snark::{build_snark_message, compute_winning_lottery_indices},
        },
    };

    type D = MithrilMembershipDigest;

    #[test]
    fn deduplicate_indices() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let parameters = Parameters {
            m: 100,
            k: 20,
            phi_f: 0.50,
        };

        let message = [0u8; 32];

        let mut initializers = Vec::new();
        let mut key_reg = KeyRegistration::initialize();

        let stakes = [10, 8, 13, 10];

        for &stake in &stakes {
            let init = Initializer::new(parameters, stake, &mut rng);
            initializers.push(init.clone());
            let entry = RegistrationEntry::try_from(init).unwrap();
            key_reg.register_by_entry(&entry).unwrap();
        }

        let closed_key_reg = key_reg.close_registration();

        let mut signatures: Vec<(SingleSignatureForSnark, LotteryTargetValue)> = Vec::new();
        for init in initializers {
            let signer: Signer<D> = init.clone().try_create_signer(&closed_key_reg).unwrap();
            let signature = signer.create_single_signature(&message).unwrap();
            let snark_sig = signature.snark_signature.unwrap();
            let lottery_target_value = signer.get_lottery_target_value().unwrap();
            signatures.push((snark_sig, lottery_target_value));
        }

        let clerk =
            SnarkClerk::new_clerk_from_closed_key_registration(&parameters, &closed_key_reg);

        let avk: AggregateVerificationKeyForSnark<D> =
            clerk.compute_aggregate_verification_key_for_snark();
        let message_to_sign =
            build_snark_message(&avk.get_merkle_tree_commitment().root, &message).unwrap();

        let mut signatures_with_indices: Vec<SingleSignatureForSnark> = Vec::new();
        for (sig, lottery_target_value) in signatures.clone() {
            let indices = compute_winning_lottery_indices(
                parameters.m,
                &message_to_sign,
                &sig.get_schnorr_signature(),
                lottery_target_value,
            )
            .unwrap();
            let mut new_sig = sig.clone();
            new_sig.set_indices(&indices);
            signatures_with_indices.push(new_sig);
        }

        let deduped_sigs = SnarkClerk::select_valid_signatures_for_k_indices(
            &parameters,
            &signatures_with_indices,
        )
        .unwrap();

        let all_indices: Vec<LotteryIndex> =
            deduped_sigs.iter().flat_map(|s| s.get_indices()).collect();
        let unique_indices: HashSet<LotteryIndex> = all_indices.iter().copied().collect();
        assert_eq!(
            all_indices.len(),
            unique_indices.len(),
            "Duplicate indices found in deduplicated signatures"
        );

        assert!(
            all_indices.len() as u64 >= parameters.k,
            "Expected at least k={} indices, got {}",
            parameters.k,
            all_indices.len()
        );
    }
}
