//! Sterling: Integrate centralized Telescope to Mithril-STM

use crate::key_reg::{ClosedKeyReg, RegParty};
use crate::merkle_tree::{BatchPath, MerkleTreeCommitmentBatchCompat};
use crate::multi_sig::{Signature, VerificationKey};
use crate::stm::{Index, Stake, StmParameters, StmSig, StmSigRegParty};
use crate::AggregationError;
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::*;
use alba::utils::types::Element;
use blake2::digest::{Digest, FixedOutput, Update, VariableOutput};
use blake2::Blake2bVar;
use sha2::Sha256;
use std::collections::{BTreeMap, HashMap, HashSet};

const DATA_LENGTH: usize = 48;
type Data = [u8; DATA_LENGTH];
type P = Proof<Data, Sha256>;

/// Initialization parameters for Sterling
pub struct SterlingInitializer {
    /// Adversarial stake percentage
    pub adversarial_stake: f64,
    /// Liveness parameter
    pub liveness: f64,
    /// STM protocol parameters
    pub stm_parameters: StmParameters,
}
impl SterlingInitializer {
    /// New initializer
    pub fn init_telescope(&self, soundness_param: f64, completeness_param: f64) -> Telescope {
        let m_f64 = self.stm_parameters.m as f64;
        let set_size = (m_f64 * (1.0 - self.adversarial_stake) * self.liveness).ceil();
        let lower_bound = self.stm_parameters.k;
        Telescope::create(
            soundness_param,
            completeness_param,
            set_size as u64,
            lower_bound,
        )
    }
}

/// Aggregator
#[derive(Debug, Clone)]
pub struct SterlingClerk<D: Clone + Digest> {
    /// Closed key registration
    pub closed_reg: ClosedKeyReg<D>,
    /// Mithril STM parameters
    pub stm_parameters: StmParameters,
    /// The main centralized Telescope struct
    pub telescope: Telescope,
}
impl<D: Digest + Clone + FixedOutput> SterlingClerk<D> {
    /// Create a new `Clerk` from a closed registration instance.
    pub fn from_registration(
        stm_parameters: &StmParameters,
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
    pub fn aggregate<E>(&self, sigs: &[StmSig], msg: &[u8]) -> SterlingProof<D>
    where
        E: AsRef<[u8]> + Clone,
    {
        let (unique_signatures, index_count) = self.collect_prover_signatures(sigs, msg);
        let (clerk_handler, prover_set) =
            SterlingClerkHandler::new(&unique_signatures, index_count as usize);

        let telescope_proof: P = self.telescope.prove(&prover_set).unwrap();

        let (proof_index_sequence, proof_signatures) =
            clerk_handler.decode_proof(&telescope_proof.element_sequence);

        let mt_index_list: Vec<usize> = proof_signatures.keys().map(|&key| key as usize).collect();

        let batch_proof = self.closed_reg.merkle_tree.get_batched_path(mt_index_list);

        SterlingProof {
            signatures: proof_signatures.values().cloned().collect(),
            batch_proof,
            retry_counter: telescope_proof.retry_counter,
            search_counter: telescope_proof.search_counter,
            index_sequence: proof_index_sequence,
        }
    }

    /// Collect unique signatures
    pub fn collect_prover_signatures(
        &self,
        sigs: &[StmSig],
        msg: &[u8],
    ) -> (Vec<StmSigRegParty>, u64) {
        // Collect signatures and their reg party
        let sig_reg_list = sigs
            .iter()
            .map(|sig| StmSigRegParty {
                sig: sig.clone(),
                reg_party: self.closed_reg.reg_parties[sig.signer_index as usize],
            })
            .collect::<Vec<StmSigRegParty>>();

        let avk = SterlingAVK::from(&self.closed_reg);
        let msgp = avk.mt_commitment.concat_with_msg(msg);

        // Get unique signatures
        SterlingClerkHandler::dedup_sigs_for_indices(
            &self.closed_reg.total_stake,
            &self.stm_parameters,
            &msgp,
            &sig_reg_list,
        )
        .unwrap()
    }

    /// Compute the `StmAggrVerificationKey` related to the used registration.
    pub fn compute_avk(&self) -> SterlingAVK<D> {
        SterlingAVK::from(&self.closed_reg)
    }
}

/// Helper struct for Sterling aggregation
pub struct SterlingClerkHandler {
    /// Signer index mapped to its StmSigRegParty
    pub signer_sigreg_map: BTreeMap<Index, StmSigRegParty>,
    /// Map of the hash of the lottery index to itself
    pub lottery_hash_index_map: BTreeMap<Data, Index>,
}
impl SterlingClerkHandler {
    /// create a new handler
    pub fn new<E>(unique_signatures: &[StmSigRegParty], size: usize) -> (Self, Vec<Element<E>>)
    where
        E: AsRef<[u8]> + Clone + for<'a> TryFrom<&'a [u8]>,
    {
        let mut signer_sigreg_map: BTreeMap<Index, StmSigRegParty> = BTreeMap::new();
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
                    prover_set.push(Element {
                        data,
                        index: Some(signer_index),
                    });
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
    pub fn decode_proof<E>(
        &self,
        proof_element_sequence: &[Element<E>],
    ) -> (Vec<(Index, Index)>, BTreeMap<Index, StmSigRegParty>)
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
    pub fn generate_raw_data(input: &Index) -> Data {
        let mut digest_buf = [0u8; 48];
        let data_buf = input.to_be_bytes();
        let mut hasher = Blake2bVar::new(48).expect("Failed to construct hasher");
        hasher.update(&data_buf);
        hasher
            .finalize_variable(&mut digest_buf)
            .expect("Hashing failed");
        digest_buf
    }

    /// Filter unique indices
    pub fn dedup_sigs_for_indices(
        total_stake: &Stake,
        params: &StmParameters,
        msg: &[u8],
        sigs: &[StmSigRegParty],
    ) -> Result<(Vec<StmSigRegParty>, u64), AggregationError> {
        let mut sig_by_index: BTreeMap<Index, &StmSigRegParty> = BTreeMap::new();
        let mut removal_idx_by_vk: HashMap<&StmSigRegParty, Vec<Index>> = HashMap::new();

        for sig_reg in sigs.iter() {
            if sig_reg
                .sig
                .verify_core(
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

        let mut dedup_sigs: HashSet<StmSigRegParty> = HashSet::new();
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

/// STM-Telescope proof.
pub struct SterlingProof<D: Clone + Digest + FixedOutput> {
    /// StmSignatures of alba proof
    pub signatures: Vec<StmSigRegParty>,
    /// The list of unique merkle tree nodes that covers path for all signatures.
    pub batch_proof: BatchPath<D>,
    /// Numbers of retries done to find the proof
    pub retry_counter: u64,
    /// Index of the searched subtree to find the proof
    pub search_counter: u64,
    /// Sequence of elements from prover's set
    pub index_sequence: Vec<(Index, Index)>,
}
impl<D: Clone + Digest + FixedOutput> SterlingProof<D> {
    /// Verify indices
    pub fn verify_indices(
        &self,
        msgp: &[u8],
        avk: &SterlingAVK<D>,
        stm_parameters: &StmParameters,
    ) -> bool {
        let mut unique_indices: HashSet<Index> = HashSet::new();
        let mut nr_indices = 0;

        // Check winning lotteries, collect unique indices and map signer index with its lottery index list.
        let signer_indices_map: BTreeMap<&Index, &Vec<Index>> = self
            .signatures
            .iter()
            .filter_map(|sig_reg| {
                if sig_reg
                    .sig
                    .check_indices(stm_parameters, &sig_reg.reg_party.1, msgp, &avk.total_stake)
                    .is_ok()
                {
                    unique_indices.extend(&sig_reg.sig.indexes);
                    nr_indices += sig_reg.sig.indexes.len();
                    Some((&sig_reg.sig.signer_index, &sig_reg.sig.indexes))
                } else {
                    None
                }
            })
            .collect();

        // Check uniqueness
        if nr_indices != unique_indices.len() {
            return false;
        }

        // Verify proof index sequence against signer index lists
        if self
            .index_sequence
            .iter()
            .any(|(proof_element_as_index, signer_index)| {
                signer_indices_map
                    .get(signer_index)
                    .is_none_or(|index_list| !index_list.contains(proof_element_as_index))
            })
        {
            return false;
        }
        true
    }

    /// Batch proof and multi sig verification
    pub fn verify_multi_sig(&self, avk: &SterlingAVK<D>, msgp: &[u8]) -> bool {
        let leaves: Vec<RegParty> = self.signatures.iter().map(|r| r.reg_party).collect();

        // Verify batch proof
        if avk.mt_commitment.check(&leaves, &self.batch_proof).is_err() {
            return false;
        }

        // Verify aggregated signatures
        let (sigs, vks) = Self::collect_sigs_vks(&self.signatures);
        if Signature::verify_aggregate(msgp, &vks, &sigs).is_err() {
            return false;
        }
        true
    }

    /// Verify
    pub fn verify(
        &self,
        telescope: &Telescope,
        msg: &[u8],
        avk: &SterlingAVK<D>,
        stm_parameters: &StmParameters,
    ) {
        let msgp = avk.mt_commitment.concat_with_msg(msg);

        if !self.verify_indices(&msgp, avk, stm_parameters) {
            println!("Indices check failed!");
        }
        if !self.verify_multi_sig(avk, &msgp) {
            println!("Multi signature check failed!");
        }

        // Construct telescope proof element sequence
        let element_sequence: Vec<Element<Data>> = self
            .index_sequence
            .iter()
            .map(|(proof_element, element_index)| {
                let element_data = SterlingClerkHandler::generate_raw_data(proof_element);
                Element {
                    data: element_data,
                    index: Some(*element_index),
                }
            })
            .collect();

        let proof: P = Proof::from(self.retry_counter, self.search_counter, element_sequence);

        println!("{}", telescope.verify(&proof));
    }

    /// Collect and return `Vec<Signature>, Vec<VerificationKey>` which will be used
    /// by the aggregate verification.
    fn collect_sigs_vks(sig_reg_list: &[StmSigRegParty]) -> (Vec<Signature>, Vec<VerificationKey>) {
        let sigs = sig_reg_list
            .iter()
            .map(|sig_reg| sig_reg.sig.sigma)
            .collect::<Vec<Signature>>();
        let vks = sig_reg_list
            .iter()
            .map(|sig_reg| sig_reg.reg_party.0)
            .collect::<Vec<VerificationKey>>();

        (sigs, vks)
    }
}

/// Sterling aggregate key, containing the merkle tree commitment and the total stake of the system.
#[derive(Debug, Clone)]
pub struct SterlingAVK<D: Clone + Digest + FixedOutput> {
    mt_commitment: MerkleTreeCommitmentBatchCompat<D>,
    total_stake: Stake,
}
impl<D: Clone + Digest + FixedOutput> From<&ClosedKeyReg<D>> for SterlingAVK<D> {
    fn from(reg: &ClosedKeyReg<D>) -> Self {
        Self {
            mt_commitment: reg.merkle_tree.to_commitment_batch_compat(),
            total_stake: reg.total_stake,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::key_reg::KeyReg;
    use crate::stm::{StmInitializer, StmSigner};
    use blake2::Blake2b;
    use digest::consts::U32;
    use rand::Rng;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    type D = Blake2b<U32>;

    #[test]
    fn test_stm_telescope() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let sentence = "ALBA Rocks!";
        let msg = sentence.as_bytes();

        let soundness_param = 10.0;
        let completeness_param = 10.0;
        let params = StmParameters {
            k: 100,  // 2300,
            m: 3000, // 20000,
            phi_f: 0.9,
        };
        let initializer = SterlingInitializer {
            adversarial_stake: 0.55,
            liveness: 0.95,
            stm_parameters: params,
        };
        let telescope = initializer.init_telescope(soundness_param, completeness_param);

        let nb_elements: u64 = 1_000;

        let stakes: Vec<u64> = (0..nb_elements).map(|_| rng.gen_range(1..=9999)).collect();

        let mut key_reg = KeyReg::init();

        let mut ps: Vec<StmInitializer> = Vec::with_capacity(nb_elements as usize);
        for stake in stakes {
            let p = StmInitializer::setup(params, stake, &mut rng);
            key_reg.register(p.stake, p.verification_key()).unwrap();
            ps.push(p);
        }

        let closed_reg = key_reg.close();

        let ps = ps
            .into_iter()
            .map(|p| p.new_signer(closed_reg.clone()).unwrap())
            .collect::<Vec<StmSigner<D>>>();

        let sigs = ps
            .iter()
            .filter_map(|p| p.sign(msg))
            .collect::<Vec<StmSig>>();

        let clerk = SterlingClerk::from_registration(&params, &closed_reg, &telescope);
        let sterling_proof = clerk.aggregate::<Data>(&sigs, msg);

        sterling_proof.verify(&telescope, msg, &clerk.compute_avk(), &params);
    }
}
