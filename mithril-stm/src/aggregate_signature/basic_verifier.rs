use std::collections::{BTreeMap, HashMap, HashSet};

use crate::bls_multi_signature::{BlsSignature, BlsVerificationKey};
use crate::key_registration::RegisteredParty;
use crate::merkle_tree::MerkleTreeLeaf;
use crate::{
    AggregationError, CoreVerifierError, Index, Parameters, SingleSignature,
    SingleSignatureWithRegisteredParty, Stake,
};

/// Full node verifier including the list of eligible signers and the total stake of the system.
pub struct BasicVerifier {
    /// List of registered parties.
    pub eligible_parties: Vec<RegisteredParty>,
    /// Total stake of registered parties.
    pub total_stake: Stake,
}

impl BasicVerifier {
    /// Setup a basic verifier for given list of signers.
    ///     * Collect the unique signers in a hash set,
    ///     * Calculate the total stake of the eligible signers,
    ///     * Sort the eligible signers.
    pub fn new(public_signers: &[(BlsVerificationKey, Stake)]) -> Self {
        let mut total_stake: Stake = 0;
        let mut unique_parties = HashSet::new();
        for signer in public_signers.iter() {
            let (res, overflow) = total_stake.overflowing_add(signer.1);
            if overflow {
                panic!("Total stake overflow");
            }
            total_stake = res;
            unique_parties.insert(MerkleTreeLeaf(signer.0, signer.1));
        }

        let mut eligible_parties: Vec<_> = unique_parties.into_iter().collect();
        eligible_parties.sort_unstable();
        BasicVerifier {
            eligible_parties,
            total_stake,
        }
    }

    /// Setup a basic verifier for given list of signers.
    ///     * Collect the unique signers in a hash set,
    ///     * Calculate the total stake of the eligible signers,
    ///     * Sort the eligible signers.
    #[deprecated(since = "0.5.0", note = "Use `new` instead")]
    pub fn setup(public_signers: &[(BlsVerificationKey, Stake)]) -> Self {
        Self::new(public_signers)
    }

    /// Preliminary verification that checks whether indices are unique and the quorum is achieved.
    pub(crate) fn preliminary_verify(
        total_stake: &Stake,
        signatures: &[SingleSignatureWithRegisteredParty],
        parameters: &Parameters,
        msg: &[u8],
    ) -> Result<(), CoreVerifierError> {
        let mut nr_indices = 0;
        let mut unique_indices = HashSet::new();

        for sig_reg in signatures {
            sig_reg
                .sig
                .check_indices(parameters, &sig_reg.reg_party.1, msg, total_stake)?;
            for &index in &sig_reg.sig.indexes {
                unique_indices.insert(index);
                nr_indices += 1;
            }
        }

        if nr_indices != unique_indices.len() {
            return Err(CoreVerifierError::IndexNotUnique);
        }
        if (nr_indices as u64) < parameters.k {
            return Err(CoreVerifierError::NoQuorum(nr_indices as u64, parameters.k));
        }

        Ok(())
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
        total_stake: &Stake,
        params: &Parameters,
        msg: &[u8],
        sigs: &[SingleSignatureWithRegisteredParty],
    ) -> Result<Vec<SingleSignatureWithRegisteredParty>, AggregationError> {
        let mut sig_by_index: BTreeMap<Index, &SingleSignatureWithRegisteredParty> =
            BTreeMap::new();
        let mut removal_idx_by_vk: HashMap<&SingleSignatureWithRegisteredParty, Vec<Index>> =
            HashMap::new();

        for sig_reg in sigs.iter() {
            if sig_reg
                .sig
                .basic_verify(
                    params,
                    &sig_reg.reg_party.0,
                    &sig_reg.reg_party.1,
                    msg,
                    total_stake,
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

                if count >= params.k {
                    return Ok(dedup_sigs.into_iter().collect());
                }
            }
        }
        Err(AggregationError::NotEnoughSignatures(count, params.k))
    }

    /// Given a slice of `sig_reg_list`, this function returns a new list of `sig_reg_list` with only valid indices.
    /// In case of conflict (having several signatures for the same index)
    /// it selects the smallest signature (i.e. takes the signature with the smallest scalar).
    /// The function selects at least `self.k` indexes.
    ///  # Error
    /// If there is no sufficient signatures, then the function fails.
    // todo: We need to agree on a criteria to dedup (by default we use a BTreeMap that guarantees keys order)
    // todo: not good, because it only removes index if there is a conflict (see benches)
    #[deprecated(
        since = "0.5.0",
        note = "Use `select_valid_signatures_for_k_indices` instead"
    )]
    pub fn dedup_sigs_for_indices(
        total_stake: &Stake,
        params: &Parameters,
        msg: &[u8],
        sigs: &[SingleSignatureWithRegisteredParty],
    ) -> Result<Vec<SingleSignatureWithRegisteredParty>, AggregationError> {
        Self::select_valid_signatures_for_k_indices(total_stake, params, msg, sigs)
    }

    /// Collect and return `Vec<BlsSignature>, Vec<BlsVerificationKey>` which will be used
    /// by the aggregate verification.
    pub(crate) fn collect_signatures_verification_keys(
        sig_reg_list: &[SingleSignatureWithRegisteredParty],
    ) -> (Vec<BlsSignature>, Vec<BlsVerificationKey>) {
        let sigs = sig_reg_list
            .iter()
            .map(|sig_reg| sig_reg.sig.sigma)
            .collect::<Vec<BlsSignature>>();
        let vks = sig_reg_list
            .iter()
            .map(|sig_reg| sig_reg.reg_party.0)
            .collect::<Vec<BlsVerificationKey>>();

        (sigs, vks)
    }

    /// Core verification
    ///
    /// Verify a list of signatures with respect to given message with given parameters.
    pub fn verify(
        &self,
        signatures: &[SingleSignature],
        parameters: &Parameters,
        msg: &[u8],
    ) -> Result<(), CoreVerifierError> {
        let sig_reg_list = signatures
            .iter()
            .map(|sig| SingleSignatureWithRegisteredParty {
                sig: sig.clone(),
                reg_party: self.eligible_parties[sig.signer_index as usize],
            })
            .collect::<Vec<SingleSignatureWithRegisteredParty>>();

        let unique_sigs = Self::select_valid_signatures_for_k_indices(
            &self.total_stake,
            parameters,
            msg,
            &sig_reg_list,
        )?;

        Self::preliminary_verify(&self.total_stake, &unique_sigs, parameters, msg)?;

        let (sigs, vks) = Self::collect_signatures_verification_keys(&unique_sigs);

        BlsSignature::verify_aggregate(msg.to_vec().as_slice(), &vks, &sigs)?;

        Ok(())
    }
}
