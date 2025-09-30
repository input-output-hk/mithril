#![allow(dead_code)]
#![allow(clippy::extra_unused_type_parameters)]

use crate::aggregate_signature::telescope_stm::telescope_clerk::{ClerkHandler, Data, P};
use crate::bls_multi_signature::{BlsSignature, BlsVerificationKey};
use crate::key_registration::RegisteredParty;
use crate::merkle_tree::MerkleBatchPath;
use crate::{AggregateVerificationKey, Index, Parameters, SingleSignatureWithRegisteredParty};
use alba::centralized_telescope::Telescope;
use alba::centralized_telescope::proof::Proof;
use alba::utils::types::Element;
use blake2::digest::{Digest, FixedOutput};
use std::collections::{BTreeMap, HashSet};

/// STM-Telescope proof.
pub struct TelescopeProof<D: Clone + Digest + FixedOutput> {
    /// StmSignatures of alba proof
    pub(crate) signatures: Vec<SingleSignatureWithRegisteredParty>,
    /// The list of unique merkle tree nodes that covers path for all signatures.
    pub(crate) batch_proof: MerkleBatchPath<D>,
    /// Numbers of retries done to find the proof
    pub(crate) retry_counter: u64,
    /// Index of the searched subtree to find the proof
    pub(crate) search_counter: u64,
    /// Sequence of elements from prover's set
    pub(crate) index_sequence: Vec<(Index, Index)>,
}

impl<D: Clone + Digest + FixedOutput> TelescopeProof<D> {
    /// Verify indices
    fn verify_indices(
        &self,
        msgp: &[u8],
        avk: &AggregateVerificationKey<D>,
        stm_parameters: &Parameters,
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
                    .check_indices(
                        stm_parameters,
                        &sig_reg.reg_party.1,
                        msgp,
                        &avk.get_total_stake(),
                    )
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
    fn verify_multi_sig(&self, avk: &AggregateVerificationKey<D>, msgp: &[u8]) -> bool {
        let leaves: Vec<RegisteredParty> = self.signatures.iter().map(|r| r.reg_party).collect();

        // Verify batch proof
        if avk
            .get_merkle_tree_batch_commitment()
            .verify_leaves_membership_from_batch_path(&leaves, &self.batch_proof)
            .is_err()
        {
            return false;
        }

        // Verify aggregated signatures
        let (sigs, vks) = Self::collect_sigs_vks(&self.signatures);
        if BlsSignature::verify_aggregate(msgp, &vks, &sigs).is_err() {
            return false;
        }
        true
    }

    /// Verify
    fn verify(
        &self,
        telescope: &Telescope,
        msg: &[u8],
        avk: &AggregateVerificationKey<D>,
        stm_parameters: &Parameters,
    ) {
        let msgp = avk.get_merkle_tree_batch_commitment().concatenate_with_message(msg);

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
                let element_data = ClerkHandler::generate_raw_data(proof_element);
                Element {
                    data: element_data,
                    index: Some(*element_index),
                }
            })
            .collect();

        let proof: P = Proof::from(self.retry_counter, self.search_counter, element_sequence);

        println!("{:?}", telescope.verify(&proof).unwrap());
    }

    /// Collect and return `Vec<Signature>, Vec<VerificationKey>` which will be used
    /// by the aggregate verification.
    fn collect_sigs_vks(
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
}
