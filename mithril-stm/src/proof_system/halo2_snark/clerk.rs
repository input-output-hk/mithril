use std::collections::{BTreeMap, btree_map::Entry};

use crate::{
    AggregationError, ClosedKeyRegistration, LotteryIndex, MembershipDigest, Parameters,
    RegistrationEntryForSnark, Signer, SingleSignature, StmResult,
};

use super::AggregateVerificationKeyForSnark;

/// Orchestrator for SNARK proof construction.
/// The `SnarkClerk` holds the closed key registration and protocol parameters needed to validate
/// signatures, compute aggregate verification keys, look up signer registration entries, and select
/// the `k` winning lottery indices that feed into the SNARK witness.
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
    /// signature (by `Ord` on `UniqueSchnorrSignature`) is kept. If more than `k` unique indices
    /// remain after deduplication, the highest indices are discarded (the `BTreeMap` is trimmed
    /// from the back).
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
