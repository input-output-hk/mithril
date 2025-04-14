//! Sterling: Integrate centralized Telescope to Mithril-STM

#![allow(dead_code)]
#![allow(clippy::extra_unused_type_parameters)]

use crate::bls_multi_signature::{Signature, VerificationKey};
use crate::key_reg::{ClosedKeyReg, RegParty};
use crate::merkle_tree::{BatchPath, MerkleTreeCommitmentBatchCompat};
use crate::stm::{Index, Stake, StmAggrVerificationKey, StmParameters, StmSig, StmSigRegParty};
use crate::stm_telescope::utils::{compute_k_adv, compute_k_hon, compute_m};
use crate::{AggregationError, StmAggregateSignatureError};
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::*;
use alba::utils::types::Element;
use blake2::digest::{Digest, FixedOutput, Update, VariableOutput};
use blake2::Blake2bVar;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::collections::{BTreeMap, HashMap, HashSet};

const DATA_LENGTH: usize = 48;
pub type Data = [u8; DATA_LENGTH];
pub type P = Proof<Data, Sha256>;

/// Parameters to initialize STM and Telescope
pub struct SterlingInitializer {
    pub adv_percentage: f64,
    pub hon_percentage: f64,
    pub f: f64,
    pub soundness_param: f64,
    pub completeness_param: f64,
}

impl Default for SterlingInitializer {
    fn default() -> Self {
        Self {
            adv_percentage: 0.05,
            hon_percentage: 0.95,
            f: 0.2,
            soundness_param: 128.0,
            completeness_param: 32.0,
        }
    }
}

impl SterlingInitializer {
    pub fn generate_parameters(&self, constant: f64) -> (StmParameters, Telescope) {
        let m = compute_m(self.adv_percentage, self.soundness_param, self.f, constant);
        let khon = compute_k_hon(m, self.hon_percentage, self.f, self.completeness_param);
        let kadv = compute_k_adv(self.soundness_param, m, self.adv_percentage, self.f);

        let stm_parameters = StmParameters {
            m: m as u64,
            k: khon,
            phi_f: self.f,
        };
        let telescope =
            Telescope::create(self.soundness_param, self.completeness_param, khon, kadv);

        (stm_parameters, telescope)
    }

    pub fn stm_from_telescope(&self, telescope: Telescope, constant: f64) -> StmParameters {
        let m = compute_m(self.adv_percentage, self.soundness_param, self.f, constant);
        StmParameters {
            m: m as u64,
            k: telescope.get_set_size(),
            phi_f: self.f,
        }
    }

    pub fn telescope_from_stm(&self, stm: StmParameters) -> Telescope {
        let kadv = compute_k_adv(
            self.soundness_param,
            stm.m as f64,
            self.adv_percentage,
            stm.phi_f,
        );
        Telescope::create(self.soundness_param, self.completeness_param, stm.k, kadv)
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
    /// Create a new `Clerk`
    pub fn new(
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
    pub(crate) fn aggregate<E>(
        &self,
        sigs: &[StmSig],
        msg: &[u8],
    ) -> Result<SterlingProof<D>, AggregationError>
    where
        E: AsRef<[u8]> + Clone,
    {
        let (unique_signatures, index_count) = self.collect_prover_signatures(sigs, msg)?;
        let (clerk_handler, prover_set) =
            SterlingClerkHandler::new(&unique_signatures, index_count as usize);

        let telescope_proof: P =
            self.telescope
                .prove(&prover_set)
                .map(Ok)
                .unwrap_or_else(|| {
                    Err(AggregationError::General(
                        "Telescope could prove the set".to_string(),
                    ))
                })?;

        let (proof_index_sequence, proof_signatures) =
            clerk_handler.decode_proof(&telescope_proof.element_sequence);

        let mt_index_list: Vec<usize> = proof_signatures.keys().map(|&key| key as usize).collect();

        let batch_proof = self.closed_reg.merkle_tree.get_batched_path(mt_index_list);

        Ok(SterlingProof {
            signatures: proof_signatures.values().cloned().collect(),
            batch_proof,
            retry_counter: telescope_proof.retry_counter,
            search_counter: telescope_proof.search_counter,
            index_sequence: proof_index_sequence,
        })
    }

    /// Collect unique signatures
    fn collect_prover_signatures(
        &self,
        sigs: &[StmSig],
        msg: &[u8],
    ) -> Result<(Vec<StmSigRegParty>, u64), AggregationError> {
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
    }

    /// Compute the `SterlingAVK` related to the used registration.
    fn compute_avk(&self) -> SterlingAVK<D> {
        SterlingAVK::from(&self.closed_reg)
    }
}

/// Helper struct for Sterling aggregation
struct SterlingClerkHandler {
    /// Signer index mapped to its StmSigRegParty
    signer_sigreg_map: BTreeMap<Index, StmSigRegParty>,
    /// Map of the hash of the lottery index to itself
    lottery_hash_index_map: BTreeMap<Data, Index>,
}
impl SterlingClerkHandler {
    /// create a new handler
    fn new<E>(unique_signatures: &[StmSigRegParty], size: usize) -> (Self, Vec<Element<E>>)
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
    fn generate_raw_data(input: &Index) -> Data {
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
    fn dedup_sigs_for_indices(
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
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "BatchPath<D>: Serialize",
    deserialize = "BatchPath<D>: Deserialize<'de>"
))]
pub struct SterlingProof<D: Clone + Digest + FixedOutput> {
    /// StmSignatures of alba proof
    pub(crate) signatures: Vec<StmSigRegParty>,
    /// The list of unique merkle tree nodes that covers path for all signatures.
    pub(crate) batch_proof: BatchPath<D>,
    /// Numbers of retries done to find the proof
    retry_counter: u64,
    /// Index of the searched subtree to find the proof
    search_counter: u64,
    /// Sequence of elements from prover's set
    index_sequence: Vec<(Index, Index)>,
}
impl<D: Clone + Digest + FixedOutput> SterlingProof<D> {
    /// Verify indices
    fn verify_indices(
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
    fn verify_multi_sig(&self, avk: &SterlingAVK<D>, msgp: &[u8]) -> bool {
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
    ) -> Result<(), AggregationError> {
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
        if !telescope.verify(&proof) {
            return Err(AggregationError::General(
                "Telescope proof verification failed".to_string(),
            ));
        }

        Ok(())
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

    /// Convert multi signature to bytes
    /// # Layout
    /// * TBD
    pub fn to_bytes(&self) -> Vec<u8> {
        /* let mut out = Vec::new();
        out.extend_from_slice(&u64::try_from(self.signatures.len()).unwrap().to_be_bytes());
        out.extend_from_slice(
            &u64::try_from(self.signatures[0].to_bytes().len())
                .unwrap()
                .to_be_bytes(),
        );
        for sig_reg in &self.signatures {
            out.extend_from_slice(&sig_reg.to_bytes());
        }
        let proof = &self.batch_proof;
        out.extend_from_slice(&proof.to_bytes());

        out*/
        todo!("Implement to_bytes for SterlingProof")
    }

    ///Extract a `SterlingProof` from a byte slice.
    pub fn from_bytes(bytes: &[u8]) -> Result<SterlingProof<D>, StmAggregateSignatureError<D>> {
        /* let mut u64_bytes = [0u8; 8];

        u64_bytes.copy_from_slice(&bytes[..8]);
        let size = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| StmAggregateSignatureError::SerializationError)?;

        u64_bytes.copy_from_slice(&bytes[8..16]);
        let sig_reg_size = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| StmAggregateSignatureError::SerializationError)?;

        let mut sig_reg_list = Vec::with_capacity(size);
        for i in 0..size {
            let sig_reg = StmSigRegParty::from_bytes::<D>(
                &bytes[16 + (sig_reg_size * i)..16 + (sig_reg_size * (i + 1))],
            )?;
            sig_reg_list.push(sig_reg);
        }

        let offset = 16 + sig_reg_size * size;
        let batch_proof = BatchPath::from_bytes(&bytes[offset..])?;

        Ok(StmAggrSigConcatenationProof {
            signatures: sig_reg_list,
            batch_proof,
        }) */
        todo!("Implement from_bytes for SterlingProof")
    }
}

/// Sterling aggregate key, containing the merkle tree commitment and the total stake of the system.
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

impl<D: Clone + Digest + FixedOutput> From<StmAggrVerificationKey<D>> for SterlingAVK<D> {
    fn from(stm_aggr_vk: StmAggrVerificationKey<D>) -> Self {
        Self {
            mt_commitment: stm_aggr_vk.mt_commitment,
            total_stake: stm_aggr_vk.total_stake,
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
    use proptest::prelude::Rng;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    type D = Blake2b<U32>;

    #[test]
    fn test_params() {
        let initializer = SterlingInitializer {
            adv_percentage: 0.05,
            hon_percentage: 0.95,
            f: 0.9,
            soundness_param: 128.0,
            completeness_param: 32.0,
        };

        let (stm, telescope) = initializer.generate_parameters(1.0);

        let stm_parameters = initializer.stm_from_telescope(telescope, 1.0);
        println!("m: {}, khon: {}, phi: {}", stm.k, stm.m, stm.phi_f);
        println!(
            "m: {}, khon: {}, phi: {}",
            stm_parameters.k, stm_parameters.m, stm_parameters.phi_f
        );

        let telescope_parameters = initializer.telescope_from_stm(stm);
        println!(
            "set size: {}, params: {:?}",
            telescope.get_set_size(),
            telescope.get_params()
        );
        println!(
            "set size: {}, params: {:?}",
            telescope_parameters.get_set_size(),
            telescope_parameters.get_params()
        );
    }

    #[test]
    fn test_stm_telescope() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let sentence = "ALBA Rocks!";
        let msg = sentence.as_bytes();

        let initializer = SterlingInitializer {
            adv_percentage: 0.05,
            hon_percentage: 0.95,
            f: 0.9,
            soundness_param: 128.0,
            completeness_param: 32.0,
        };

        let (stm_parameters, telescope) = initializer.generate_parameters(1.0);

        let nb_elements: u64 = 1_000;

        let stakes: Vec<u64> = (0..nb_elements).map(|_| rng.gen_range(1..=9999)).collect();

        let mut key_reg = KeyReg::init();

        let mut ps: Vec<StmInitializer> = Vec::with_capacity(nb_elements as usize);
        for stake in stakes {
            let p = StmInitializer::setup(stm_parameters, stake, &mut rng);
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

        let clerk = SterlingClerk::new(&stm_parameters, &closed_reg, &telescope);
        let sterling_proof = clerk.aggregate::<Data>(&sigs, msg).unwrap();

        sterling_proof
            .verify(&telescope, msg, &clerk.compute_avk(), &stm_parameters)
            .unwrap();
    }
}
