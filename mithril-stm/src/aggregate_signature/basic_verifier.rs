use std::collections::{BTreeMap, HashMap, HashSet};
use std::time::Instant;

use rug::integer::IsPrime;

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

        let mut basic_verif_time = 0;
        let now = Instant::now();
        for sig_reg in sigs.iter() {
            let now = Instant::now();
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
            basic_verif_time += now.elapsed().as_millis();
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
        // println!("First loop took: {:?}ms.", now.elapsed().as_millis());
        // println!("Basic verif took: {:?}ms.", basic_verif_time);


        let mut dedup_sigs: HashSet<SingleSignatureWithRegisteredParty> = HashSet::new();
        let mut count: u64 = 0;
        let now = Instant::now();
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
                    // println!("Second loop took: {:?}micros.", now.elapsed().as_micros());
                    return Ok(dedup_sigs.into_iter().collect());
                }
            }
        }
        // println!("Second loop took: {:?}micros.", now.elapsed().as_micros());
        Err(AggregationError::NotEnoughSignatures(count, params.k))
    }

    /// Modification of the function select_valid_signatures_for_k_indices to try to improve readability.
    /// This function follows the same structure of iterating first over the indices inside all the signatures to select k indices,
    /// then over the selected signatures to modify the list of indices to fit the selected ones.
    pub fn reworked_select_valid_signatures_for_k_indices(
        total_stake: &Stake,
        params: &Parameters,
        msg: &[u8],
        sigs: &[SingleSignatureWithRegisteredParty],
    ) -> Result<Vec<SingleSignatureWithRegisteredParty>, AggregationError> {
        let now = Instant::now();
        let (idx_by_mtidx, btm) = Self::get_k_indices(total_stake, params, msg, sigs);
        // println!("First function took: {:?}ms.", now.elapsed().as_millis());

        let now = Instant::now();
        let result = Self::valid_signatures_from_k_indices(params, idx_by_mtidx, btm);
        // println!("Second function took: {:?}micros.", now.elapsed().as_micros());

        result
    }

    pub fn reworked_select_valid_signatures_for_k_indices_opti(
        total_stake: &Stake,
        params: &Parameters,
        msg: &[u8],
        sigs: &[SingleSignatureWithRegisteredParty],
    ) -> Result<Vec<SingleSignatureWithRegisteredParty>, AggregationError> {
        let now = Instant::now();
        let btm = Self::get_k_indices_opti(total_stake, params, msg, sigs);
        // println!("First function OPTI? took: {:?}ms.\n", now.elapsed().as_millis());

        let now = Instant::now();
        let result = Self::valid_signatures_from_k_indices_opti(params, btm);
        // println!("Second function took: {:?}micros.", now.elapsed().as_micros());

        result
    }

    /// Iterates over the indices list in each signature to select the tuple (index, signature) with the smallest corresponding sigma.
    /// Uses a BTreeMap to keep track of the correspondance between merkle tree index and signature
    /// and a HashMap to connect the selected indices to the merkle tree index of the signature.
    /// TODO: Merge those two as the MT index is already in the signature
    pub fn get_k_indices<'a>(
        total_stake: &Stake,
        params: &Parameters,
        msg: &[u8],
        sigs: &'a [SingleSignatureWithRegisteredParty],
    ) -> (
        HashMap<Index, Index>,
        BTreeMap<Index, &'a SingleSignatureWithRegisteredParty>,
    ) {
        let mut sig_by_mt_index: BTreeMap<Index, &SingleSignatureWithRegisteredParty> =
            BTreeMap::new();
        let mut indices_by_mt_idx: HashMap<Index, Index> = HashMap::new();

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
                if let Some(mt_idx) = indices_by_mt_idx.get(index) {
                    if let Some(prev_sig) = sig_by_mt_index.get(mt_idx) {
                        if prev_sig.sig.sigma < sig_reg.sig.sigma {
                            continue;
                        } else {
                            indices_by_mt_idx.insert(*index, sig_reg.sig.signer_index);
                            sig_by_mt_index.insert(sig_reg.sig.signer_index, sig_reg);
                        }
                    }
                } else {
                    // Should we test for k indices here?
                    indices_by_mt_idx.insert(*index, sig_reg.sig.signer_index);
                    sig_by_mt_index.insert(sig_reg.sig.signer_index, sig_reg);
                }
            }
        }
        (indices_by_mt_idx, sig_by_mt_index)
    }

        /// Iterates over the indices list in each signature to select the tuple (index, signature) with the smallest corresponding sigma.
    /// Uses a BTreeMap to keep track of the correspondance between merkle tree index and signature
    /// and a HashMap to connect the selected indices to the merkle tree index of the signature.
    /// TODO: Merge those two as the MT index is already in the signature
    pub fn get_k_indices_opti<'a>(
        total_stake: &Stake,
        params: &Parameters,
        msg: &[u8],
        sigs: &'a [SingleSignatureWithRegisteredParty],
    // ) -> BTreeMap<Index, &'a SingleSignatureWithRegisteredParty>
    ) -> HashMap<Index, &'a SingleSignatureWithRegisteredParty>
    {
        // let mut indices_by_sig: BTreeMap<Index, &SingleSignatureWithRegisteredParty> =
        //     BTreeMap::new();
        let mut indices_by_sig: HashMap<Index, &SingleSignatureWithRegisteredParty> =
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
                if let Some(&prev_sig) = indices_by_sig.get(index) {
                    // if let Some(prev_sig) = sig_by_mt_index.get(mt_idx) {
                        if prev_sig.sig.sigma < sig_reg.sig.sigma {
                            continue;
                        } else {
                            indices_by_sig.insert(*index, sig_reg);
                        }
                    } else {
                    // Should we test for k indices here?
                    indices_by_sig.insert(*index, sig_reg);
                }
            }
        }
        indices_by_sig
    }

    /// Takes a list of tuples (selected index, MT index) and outputs a list of signatures with a total of k valid indices
    /// First iterates over the tuples to create a vector of selected indices for each signature.
    /// Then modify and aggregates the signatures starting with the signature with the most valid indices.
    /// This has the effect of minimizing the number of different signature used.
    pub fn valid_signatures_from_k_indices(
        params: &Parameters,
        list_k_valid_indices: HashMap<Index, Index>,
        sig_by_mt_index: BTreeMap<Index, &SingleSignatureWithRegisteredParty>,
    ) -> Result<Vec<SingleSignatureWithRegisteredParty>, AggregationError> {
        let mut valid_idx_for_mt_idx: HashMap<u64, Vec<u64>> = HashMap::new();
        let mut valid_idx_for_sig: HashMap<&SingleSignatureWithRegisteredParty, Vec<u64>> = HashMap::new();

        // Uses the HashMap of tuples (index, MT index) to create a vector of indices connected to one signature
        for (&valid_idx, &mt_idx) in list_k_valid_indices.iter() {
            if let Some(val) = valid_idx_for_mt_idx.get_mut(&mt_idx) {
                val.push(valid_idx);
            } else {
                valid_idx_for_mt_idx.insert(mt_idx, vec![valid_idx]);
            }
        }

        // sort the HashMap according to the number of selected indices of each signature
        let mut vec_valid_idx_per_mt_idx = valid_idx_for_mt_idx.into_iter().collect::<Vec<_>>();
        vec_valid_idx_per_mt_idx.sort_by(|(_, v1), (_, v2)| v2.len().cmp(&v1.len()));

        let mut uniques_sig: Vec<SingleSignatureWithRegisteredParty> = Vec::new();
        let mut count_idx = 0;
        // Modify and aggregate the signatures
        for (mt_idx, indices) in vec_valid_idx_per_mt_idx.into_iter() {
            let mut single_sig = if let Some(sig) = sig_by_mt_index.get(&mt_idx) {
                (*sig).clone()
            } else {
                // Change the error
                return Err(AggregationError::NotEnoughSignatures(0, params.k));
            };
            if indices.len() > (params.k - count_idx) as usize {
                single_sig.sig.indexes = indices[0..(params.k - count_idx) as usize].to_vec();
                count_idx += params.k - count_idx;
            } else {
                count_idx += indices.len() as u64;
                single_sig.sig.indexes = indices;
            }
            uniques_sig.push(single_sig);

            if count_idx >= params.k {
                return Ok(uniques_sig);
            }
        }

        Err(AggregationError::NotEnoughSignatures(count_idx, params.k))
    }

    pub fn valid_signatures_from_k_indices_opti(
        params: &Parameters,
        sig_by_mt_index: HashMap<Index, &SingleSignatureWithRegisteredParty>,
    ) -> Result<Vec<SingleSignatureWithRegisteredParty>, AggregationError> {
        // let mut valid_idx_for_mt_idx: HashMap<u64, Vec<u64>> = HashMap::new();
        let mut valid_idx_for_sig: HashMap<&SingleSignatureWithRegisteredParty, Vec<u64>> = HashMap::new();

        // Uses the HashMap of tuples (index, MT index) to create a vector of indices connected to one signature
        for (&valid_idx, &sig) in sig_by_mt_index.iter() {
            if let Some(val) = valid_idx_for_sig.get_mut(sig) {
                val.push(valid_idx);
            } else {
                // valid_idx_for_mt_idx.insert(mt_idx, vec![valid_idx]);
                valid_idx_for_sig.insert(sig, vec![valid_idx]);

            }
        }

        // sort the HashMap according to the number of selected indices of each signature
        let mut vec_valid_idx_per_mt_idx = valid_idx_for_sig.into_iter().collect::<Vec<_>>();
        vec_valid_idx_per_mt_idx.sort_by(|(_, v1), (_, v2)| v2.len().cmp(&v1.len()));

        let mut uniques_sig: Vec<SingleSignatureWithRegisteredParty> = Vec::new();
        let mut count_idx = 0;
        // Modify and aggregate the signatures
        for (sig, indices) in vec_valid_idx_per_mt_idx.into_iter() {
            let mut single_sig = (*sig).clone();
            if indices.len() > (params.k - count_idx) as usize {
                single_sig.sig.indexes = indices[0..(params.k - count_idx) as usize].to_vec();
                count_idx += params.k - count_idx;
            } else {
                count_idx += indices.len() as u64;
                single_sig.sig.indexes = indices;
            }
            uniques_sig.push(single_sig);

            if count_idx >= params.k {
                return Ok(uniques_sig);
            }
        }

        Err(AggregationError::NotEnoughSignatures(count_idx, params.k))
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
