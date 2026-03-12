use std::collections::{BTreeMap, btree_map::Entry};

use crate::{
    AggregationError, ClosedKeyRegistration, LotteryIndex, MembershipDigest, Parameters,
    RegistrationEntryForSnark, Signer, SingleSignature, StmResult,
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

    /// Deduplicates signatures by lottery index, keeping the one with the smallest schnorr
    /// signature for each index. Returns exactly `k` entries (sorted by lottery index).
    /// # Error
    /// Returns `AggregationError::NotEnoughSignatures` if fewer than `k` unique indices exist.
    pub(crate) fn select_valid_signatures_for_k_indices(
        parameters: &Parameters,
        signatures: &[SingleSignature],
    ) -> StmResult<BTreeMap<LotteryIndex, SingleSignature>> {
        let mut unique_index_signature_map: BTreeMap<LotteryIndex, SingleSignature> =
            BTreeMap::new();

        for signature in signatures {
            let (Some(indices), Some(snark_sig)) = (
                signature.get_snark_signature_indices(),
                signature.snark_signature.as_ref(),
            ) else {
                continue;
            };

            for index in indices {
                match unique_index_signature_map.entry(index) {
                    Entry::Occupied(mut existing) => {
                        if existing.get().snark_signature.as_ref().is_some_and(|s| {
                            s.get_schnorr_signature() > snark_sig.get_schnorr_signature()
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
