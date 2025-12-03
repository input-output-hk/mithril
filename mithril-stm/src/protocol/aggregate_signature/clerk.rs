use anyhow::{Context, anyhow};
use blake2::digest::{Digest, FixedOutput};
use std::collections::{BTreeMap, HashMap, HashSet};

// #[cfg(feature = "future_proof_system")]
// use anyhow::anyhow;

// #[cfg(feature = "future_proof_system")]
// use crate::AggregationError;

use super::{AggregateSignature, AggregateSignatureType, AggregateVerificationKey};
use crate::{
    AggregationError, ClosedKeyRegistration, Index, Parameters, Signer, SingleSignature,
    SingleSignatureWithRegisteredParty, Stake, StmResult, VerificationKey,
    proof_system::ConcatenationProof,
};

/// `Clerk` can verify and aggregate `SingleSignature`s and verify `AggregateSignature`s.
/// Clerks can only be generated with the registration closed.
/// This avoids that a Merkle Tree is computed before all parties have registered.
#[derive(Debug, Clone)]
pub struct Clerk<D: Clone + Digest> {
    pub(crate) closed_reg: ClosedKeyRegistration<D>,
    pub(crate) params: Parameters,
}

impl<D: Digest + Clone + FixedOutput + Send + Sync> Clerk<D> {
    /// Create a new `Clerk` from a closed registration instance.
    pub fn new_clerk_from_closed_key_registration(
        params: &Parameters,
        closed_reg: &ClosedKeyRegistration<D>,
    ) -> Self {
        Self {
            params: *params,
            closed_reg: closed_reg.clone(),
        }
    }

    /// Create a new `Clerk` from a closed registration instance.
    #[deprecated(
        since = "0.5.0",
        note = "Use `new_clerk_from_closed_key_registration` instead"
    )]
    pub fn from_registration(params: &Parameters, closed_reg: &ClosedKeyRegistration<D>) -> Self {
        Self::new_clerk_from_closed_key_registration(params, closed_reg)
    }

    /// Create a Clerk from a signer.
    pub fn new_clerk_from_signer(signer: &Signer<D>) -> Self {
        let closed_reg = signer
            .get_closed_key_registration()
            .clone()
            .expect("Core signer does not include closed registration. Clerk, and so, the Stm certificate cannot be built without closed registration!")
            ;

        Self {
            params: signer.get_parameters(),
            closed_reg,
        }
    }

    /// Create a Clerk from a signer.
    #[deprecated(since = "0.5.0", note = "Use `new_clerk_from_signer` instead")]
    pub fn from_signer(signer: &Signer<D>) -> Self {
        Self::new_clerk_from_signer(signer)
    }

    /// Aggregate a set of signatures.
    #[deprecated(since = "0.5.3", note = "Use `aggregate_signatures_with_type` instead")]
    pub fn aggregate_signatures(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
    ) -> StmResult<AggregateSignature<D>> {
        self.aggregate_signatures_with_type(sigs, msg, AggregateSignatureType::default())
    }

    /// Aggregate a set of signatures with a given proof type.
    pub fn aggregate_signatures_with_type(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
        aggregate_signature_type: AggregateSignatureType,
    ) -> StmResult<AggregateSignature<D>> {
        match aggregate_signature_type {
            AggregateSignatureType::Concatenation => Ok(AggregateSignature::Concatenation(
                ConcatenationProof::aggregate_signatures(self, sigs, msg).with_context(|| {
                    format!(
                        "Signatures failed to aggregate for type {}",
                        AggregateSignatureType::Concatenation
                    )
                })?,
            )),
            #[cfg(feature = "future_proof_system")]
            AggregateSignatureType::Future => Err(anyhow!(
                AggregationError::UnsupportedProofSystem(aggregate_signature_type)
            )),
        }
    }

    /// Aggregate a set of signatures for their corresponding indices.
    ///
    /// This function first deduplicates the repeated signatures, and if there are enough signatures, it collects the merkle tree indexes of unique signatures.
    /// The list of merkle tree indexes is used to create a batch proof, to prove that all signatures are from eligible signers.
    ///
    /// It returns an instance of `AggregateSignature`.
    #[deprecated(since = "0.5.0", note = "Use `aggregate_signatures` instead")]
    #[allow(deprecated)]
    pub fn aggregate(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
    ) -> StmResult<AggregateSignature<D>> {
        Self::aggregate_signatures(self, sigs, msg)
    }

    /// Compute the `AggregateVerificationKey` related to the used registration.
    pub fn compute_aggregate_verification_key(&self) -> AggregateVerificationKey<D> {
        AggregateVerificationKey::from(&self.closed_reg)
    }

    /// Compute the `AggregateVerificationKey` related to the used registration.
    #[deprecated(
        since = "0.5.0",
        note = "Use `compute_aggregate_verification_key` instead"
    )]
    pub fn compute_avk(&self) -> AggregateVerificationKey<D> {
        Self::compute_aggregate_verification_key(self)
    }

    /// Get the (VK, stake) of a party given its index.
    pub fn get_registered_party_for_index(
        &self,
        party_index: &Index,
    ) -> Option<(VerificationKey, Stake)> {
        self.closed_reg
            .reg_parties
            .get(*party_index as usize)
            .map(|&r| r.into())
    }

    /// Get the (VK, stake) of a party given its index.
    #[deprecated(since = "0.5.0", note = "Use `get_registered_party_for_index` instead")]
    pub fn get_reg_party(&self, party_index: &Index) -> Option<(VerificationKey, Stake)> {
        Self::get_registered_party_for_index(self, party_index)
    }

    /// Given a slice of `sig_reg_list`, this function returns a new list of `sig_reg_list` with only valid indices.
    /// In case of conflict (having several signatures for the same index)
    /// it selects the smallest signature (i.e. takes the signature with the smallest scalar).
    /// The function selects at least `self.k` indexes.
    ///  # Error
    /// If there is no sufficient signatures, then the function fails.
    // todo: We need to agree on a criteria to dedup (by default we use a BTreeMap that guarantees keys order)
    // todo: not good, because it only removes index if there is a conflict (see benches)
    pub fn select_valid_signatures_for_k_indices(
        &self,
        msg: &[u8],
        sigs: &[SingleSignatureWithRegisteredParty],
    ) -> StmResult<Vec<SingleSignatureWithRegisteredParty>> {
        let mut sig_by_index: BTreeMap<Index, &SingleSignatureWithRegisteredParty> =
            BTreeMap::new();
        let mut removal_idx_by_vk: HashMap<&SingleSignatureWithRegisteredParty, Vec<Index>> =
            HashMap::new();

        for sig_reg in sigs.iter() {
            if sig_reg
                .sig
                .basic_verify(
                    &self.params,
                    &sig_reg.reg_party.0,
                    &sig_reg.reg_party.1,
                    msg,
                    &self.closed_reg.total_stake,
                )
                .is_err()
            {
                continue;
            }
            for index in sig_reg.sig.indexes.iter() {
                let mut insert_this_sig = false;
                if let Some(&previous_sig) = sig_by_index.get(index) {
                    let sig_to_remove_index = if sig_reg.sig.sigma < previous_sig.sig.sigma {
                        insert_this_sig = true;
                        previous_sig
                    } else {
                        sig_reg
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
                    sig_by_index.insert(*index, sig_reg);
                }
            }
        }

        let mut dedup_sigs: HashSet<SingleSignatureWithRegisteredParty> = HashSet::new();
        let mut count: u64 = 0;

        for (_, &sig_reg) in sig_by_index.iter() {
            if dedup_sigs.contains(sig_reg) {
                continue;
            }
            let mut deduped_sig = sig_reg.clone();
            if let Some(indexes) = removal_idx_by_vk.get(sig_reg) {
                deduped_sig.sig.indexes = deduped_sig
                    .sig
                    .indexes
                    .clone()
                    .into_iter()
                    .filter(|i| !indexes.contains(i))
                    .collect();
            }

            let size: Result<u64, _> = deduped_sig.sig.indexes.len().try_into();
            if let Ok(size) = size {
                if dedup_sigs.contains(&deduped_sig) {
                    panic!("Should not reach!");
                }
                dedup_sigs.insert(deduped_sig);
                count += size;

                if count >= self.params.k {
                    return Ok(dedup_sigs.into_iter().collect());
                }
            }
        }
        Err(anyhow!(AggregationError::NotEnoughSignatures(
            count,
            self.params.k
        )))
    }
}
