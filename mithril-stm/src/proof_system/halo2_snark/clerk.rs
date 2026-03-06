use crate::{
    ClosedKeyRegistration, LotteryIndex, MembershipDigest, Parameters, RegistrationEntryForSnark,
    Signer, StmResult,
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

    pub fn select_valid_signatures_for_k_indices<D: MembershipDigest>(
        params: &Parameters,
        msg: &[u8],
        sigs: &[SingleSignatureWithRegisteredParty],
        avk: &AggregateVerificationKeyForConcatenation<D>,
    ) -> StmResult<Vec<SingleSignatureWithRegisteredParty>> {
        let mut sig_by_index: BTreeMap<LotteryIndex, &SingleSignatureWithRegisteredParty> =
            BTreeMap::new();
        let mut removal_idx_by_vk: HashMap<&SingleSignatureWithRegisteredParty, Vec<LotteryIndex>> =
            HashMap::new();

        for sig_reg in sigs.iter() {
            if sig_reg
                .sig
                .concatenation_signature
                .verify(
                    params,
                    &sig_reg.reg_party.get_verification_key_for_concatenation(),
                    &sig_reg.reg_party.get_stake(),
                    avk,
                    msg,
                )
                .is_err()
            {
                continue;
            }
            for index in sig_reg.sig.get_concatenation_signature_indices().iter() {
                let mut insert_this_sig = false;
                if let Some(&previous_sig) = sig_by_index.get(index) {
                    let sig_to_remove_index = if sig_reg.sig.get_concatenation_signature_sigma()
                        < previous_sig.sig.get_concatenation_signature_sigma()
                    {
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
                let indices = deduped_sig
                    .sig
                    .get_concatenation_signature_indices()
                    .into_iter()
                    .filter(|i| !indexes.contains(i))
                    .collect::<Vec<LotteryIndex>>();
                deduped_sig.sig.set_concatenation_signature_indices(&indices);
            }

            let size: Result<u64, _> =
                deduped_sig.sig.get_concatenation_signature_indices().len().try_into();
            if let Ok(size) = size {
                if dedup_sigs.contains(&deduped_sig) {
                    panic!(
                        "Invariant violation: duplicate signature encountered in deduplication set, which should not be possible."
                    );
                }
                dedup_sigs.insert(deduped_sig);
                count += size;

                if count >= params.k {
                    return Ok(dedup_sigs.into_iter().collect());
                }
            }
        }
        Err(anyhow!(AggregationError::NotEnoughSignatures(
            count, params.k
        )))
    }
}
