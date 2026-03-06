use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{
    AggregationError, ClosedKeyRegistration, LotteryIndex, MembershipDigest, Parameters,
    RegistrationEntryForSnark, Signer, StmResult,
};

use super::{AggregateVerificationKeyForSnark, witness::SignatureRegistrationEntry};

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
                    .into_iter()
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
