#![allow(dead_code)]
#![allow(clippy::extra_unused_type_parameters)]

use alba::centralized_telescope::Telescope;
use alba::centralized_telescope::proof::Proof;
use alba::utils::types::Element;
use blake2::Blake2bVar;
use blake2::digest::{Digest, FixedOutput, Update, VariableOutput};
use sha2::Sha256;
use std::collections::{BTreeMap, HashMap, HashSet};

const DATA_LENGTH: usize = 48;
pub(crate) type Data = [u8; DATA_LENGTH];
pub(crate) type P = Proof<Data, Sha256>;

use crate::aggregate_signature::telescope_stm::telescope_proof::TelescopeProof;
use crate::{
    AggregateVerificationKey, AggregationError, ClosedKeyReg, Index, Parameters, SingleSignature,
    SingleSignatureWithRegisteredParty, Stake,
};

#[derive(Debug, Clone)]
struct TelescopeClerk<D: Clone + Digest> {
    /// Closed key registration
    pub closed_reg: ClosedKeyReg<D>,
    /// Mithril STM parameters
    pub stm_parameters: Parameters,
    /// The main centralized Telescope struct
    pub telescope: Telescope,
}

impl<D: Digest + Clone + FixedOutput> TelescopeClerk<D> {
    /// Create a new `Clerk`
    pub fn new(
        stm_parameters: &Parameters,
        closed_reg: &ClosedKeyReg<D>,
        telescope: &Telescope,
    ) -> Self {
        Self {
            closed_reg: closed_reg.clone(),
            stm_parameters: *stm_parameters,
            telescope: *telescope,
        }
    }

    /// Create Sterling Proof
    pub(crate) fn aggregate<E>(&self, sigs: &[SingleSignature], msg: &[u8]) -> TelescopeProof<D>
    where
        E: AsRef<[u8]> + Clone,
    {
        let (unique_signatures, index_count) = self.collect_prover_signatures(sigs, msg);
        let (clerk_handler, prover_set) =
            ClerkHandler::new(&unique_signatures, index_count as usize);

        let telescope_proof: P = self.telescope.prove(&prover_set).unwrap();

        let (proof_index_sequence, proof_signatures) =
            clerk_handler.decode_proof(&telescope_proof.element_sequence);

        let mt_index_list: Vec<usize> = proof_signatures.keys().map(|&key| key as usize).collect();

        let batch_proof = self
            .closed_reg
            .merkle_tree
            .compute_merkle_tree_batch_path(mt_index_list);

        TelescopeProof {
            signatures: proof_signatures.values().cloned().collect(),
            batch_proof,
            retry_counter: telescope_proof.retry_counter,
            search_counter: telescope_proof.search_counter,
            index_sequence: proof_index_sequence,
        }
    }
    /// Collect unique signatures
    fn collect_prover_signatures(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
    ) -> (Vec<SingleSignatureWithRegisteredParty>, u64) {
        // Collect signatures and their reg party
        let sig_reg_list = sigs
            .iter()
            .map(|sig| SingleSignatureWithRegisteredParty {
                sig: sig.clone(),
                reg_party: self.closed_reg.reg_parties[sig.signer_index as usize],
            })
            .collect::<Vec<SingleSignatureWithRegisteredParty>>();

        let avk = AggregateVerificationKey::from(&self.closed_reg);
        let msgp = avk.get_merkle_tree_batch_commitment().concatenate_with_message(msg);

        // Get unique signatures
        ClerkHandler::dedup_sigs_for_indices(
            &self.closed_reg.total_stake,
            &self.stm_parameters,
            &msgp,
            &sig_reg_list,
        )
        .unwrap()
    }
}

/// Helper struct for Sterling aggregation
pub struct ClerkHandler {
    /// Signer index mapped to its StmSigRegParty
    signer_sigreg_map: BTreeMap<Index, SingleSignatureWithRegisteredParty>,
    /// Map of the hash of the lottery index to itself
    lottery_hash_index_map: BTreeMap<Data, Index>,
}

impl ClerkHandler {
    /// create a new handler
    fn new<E>(
        unique_signatures: &[SingleSignatureWithRegisteredParty],
        size: usize,
    ) -> (Self, Vec<Element<E>>)
    where
        E: AsRef<[u8]> + Clone + for<'a> TryFrom<&'a [u8]>,
    {
        let mut signer_sigreg_map: BTreeMap<Index, SingleSignatureWithRegisteredParty> =
            BTreeMap::new();
        let mut lottery_hash_index_map: BTreeMap<Data, Index> = BTreeMap::new();
        let mut prover_set = Vec::with_capacity(size);

        for sr in unique_signatures {
            let signer_index = sr.sig.signer_index;
            signer_sigreg_map.insert(signer_index, sr.clone());

            for i in &sr.sig.indexes {
                let raw_data = Self::generate_raw_data(i);
                lottery_hash_index_map.insert(raw_data, *i);

                // Convert raw_data from &[u8] to E safely
                if let Ok(data) = raw_data.as_slice().try_into() {
                    prover_set.push(Element::new(data, Some(sr.sig.signer_index)))
                };
            }
        }

        (
            Self {
                signer_sigreg_map,
                lottery_hash_index_map,
            },
            prover_set,
        )
    }

    /// Decode proof elements
    fn decode_proof<E>(
        &self,
        proof_element_sequence: &[Element<E>],
    ) -> (
        Vec<(Index, Index)>,
        BTreeMap<Index, SingleSignatureWithRegisteredParty>,
    )
    where
        E: AsRef<[u8]> + Clone,
    {
        let mut proof_index_sequence: Vec<(Index, Index)> = Vec::new();
        let mut valid_signer_indexes: HashSet<u64> = HashSet::new();
        let mut valid_indices: HashSet<Index> = HashSet::new();

        for e in proof_element_sequence {
            if let Some(unique_index) = self.lottery_hash_index_map.get(e.as_ref()) {
                if let Some(signer_index) = e.index {
                    proof_index_sequence.push((*unique_index, signer_index));
                    valid_signer_indexes.insert(signer_index);
                    valid_indices.insert(*unique_index);
                }
            }
        }

        let mut proof_signatures = self.signer_sigreg_map.clone();
        proof_signatures.retain(|index, sig| {
            if valid_signer_indexes.contains(index) {
                sig.sig.indexes.retain(|idx| valid_indices.contains(idx));
                !sig.sig.indexes.is_empty()
            } else {
                false
            }
        });

        (proof_index_sequence, proof_signatures)
    }

    /// create element data by hashing the index
    pub(crate) fn generate_raw_data(input: &Index) -> Data {
        let mut digest_buf = [0u8; 48];
        let data_buf = input.to_be_bytes();
        let mut hasher = Blake2bVar::new(48).expect("Failed to construct hasher");
        hasher.update(&data_buf);
        hasher.finalize_variable(&mut digest_buf).expect("Hashing failed");
        digest_buf
    }

    /// Filter unique indices
    fn dedup_sigs_for_indices(
        total_stake: &Stake,
        params: &Parameters,
        msg: &[u8],
        sigs: &[SingleSignatureWithRegisteredParty],
    ) -> Result<(Vec<SingleSignatureWithRegisteredParty>, u64), AggregationError> {
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
            }
        }

        if count < params.k {
            return Err(AggregationError::NotEnoughSignatures(count, params.k));
        }
        Ok((dedup_sigs.into_iter().collect(), count))
    }
}
