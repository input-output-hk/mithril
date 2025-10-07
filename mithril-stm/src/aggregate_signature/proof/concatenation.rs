use blake2::digest::{Digest, FixedOutput};

use serde::{Deserialize, Serialize};

use crate::aggregate_signature::clerk::Clerk;
use crate::bls_multi_signature::{BlsSignature, BlsVerificationKey};
use crate::key_registration::RegisteredParty;
use crate::merkle_tree::MerkleBatchPath;
use crate::{
    AggregateVerificationKey, AggregationError, BasicVerifier, Parameters, SingleSignature,
    SingleSignatureWithRegisteredParty, StmAggregateSignatureError,
};

/// `ConcatenationProof` uses the "concatenation" proving system (as described in Section 4.3 of the original paper.)
/// This means that the aggregated signature contains a vector with all individual signatures.
/// BatchPath is also a part of the aggregate signature which covers path for all signatures.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "MerkleBatchPath<D>: Serialize",
    deserialize = "MerkleBatchPath<D>: Deserialize<'de>"
))]
pub struct ConcatenationProof<D: Clone + Digest + FixedOutput> {
    pub(crate) signatures: Vec<SingleSignatureWithRegisteredParty>,
    /// The list of unique merkle tree nodes that covers path for all signatures.
    pub batch_proof: MerkleBatchPath<D>,
}

impl<D: Clone + Digest + FixedOutput + Send + Sync> ConcatenationProof<D> {
    /// Aggregate a set of signatures for their corresponding indices.
    ///
    /// This function first deduplicates the repeated signatures, and if there are enough signatures, it collects the merkle tree indexes of unique signatures.
    /// The list of merkle tree indexes is used to create a batch proof, to prove that all signatures are from eligible signers.
    ///
    /// It returns an instance of `ConcatenationProof`.
    pub fn aggregate_signatures(
        clerk: &Clerk<D>,
        sigs: &[SingleSignature],
        msg: &[u8],
    ) -> Result<ConcatenationProof<D>, AggregationError> {
        let sig_reg_list = sigs
            .iter()
            .map(|sig| SingleSignatureWithRegisteredParty {
                sig: sig.clone(),
                reg_party: clerk.closed_reg.reg_parties[sig.signer_index as usize],
            })
            .collect::<Vec<SingleSignatureWithRegisteredParty>>();

        let avk = AggregateVerificationKey::from(&clerk.closed_reg);
        let msgp = avk.get_merkle_tree_batch_commitment().concatenate_with_message(msg);
        let mut unique_sigs = BasicVerifier::select_valid_signatures_for_k_indices(
            &clerk.closed_reg.total_stake,
            &clerk.params,
            &msgp,
            &sig_reg_list,
        )?;

        unique_sigs.sort_unstable();

        let mt_index_list = unique_sigs
            .iter()
            .map(|sig_reg| sig_reg.sig.signer_index as usize)
            .collect::<Vec<usize>>();

        let batch_proof = clerk
            .closed_reg
            .merkle_tree
            .compute_merkle_tree_batch_path(mt_index_list);

        Ok(Self {
            signatures: unique_sigs,
            batch_proof,
        })
    }

    /// Verify all checks from signatures, except for the signature verification itself.
    ///
    /// Indices and quorum are checked by `BasicVerifier::preliminary_verify` with `msgp`.
    /// It collects leaves from signatures and checks the batch proof.
    /// After batch proof is checked, it collects and returns the signatures and
    /// verification keys to be used by aggregate verification.
    fn preliminary_verify(
        &self,
        msg: &[u8],
        avk: &AggregateVerificationKey<D>,
        parameters: &Parameters,
    ) -> Result<(Vec<BlsSignature>, Vec<BlsVerificationKey>), StmAggregateSignatureError<D>> {
        let msgp = avk.get_merkle_tree_batch_commitment().concatenate_with_message(msg);
        BasicVerifier::preliminary_verify(
            &avk.get_total_stake(),
            &self.signatures,
            parameters,
            &msgp,
        )?;

        let leaves = self
            .signatures
            .iter()
            .map(|r| r.reg_party)
            .collect::<Vec<RegisteredParty>>();

        avk.get_merkle_tree_batch_commitment()
            .verify_leaves_membership_from_batch_path(&leaves, &self.batch_proof)?;

        Ok(BasicVerifier::collect_signatures_verification_keys(
            &self.signatures,
        ))
    }

    /// Verify concatenation proof, by checking that
    /// * each signature contains only valid indices,
    /// * the lottery is indeed won by each one of them,
    /// * the merkle tree path is valid,
    /// * the aggregate signature validates with respect to the aggregate verification key
    ///   (aggregation is computed using functions `MSP.BKey` and `MSP.BSig` as described in Section 2.4 of the paper).
    pub fn verify(
        &self,
        msg: &[u8],
        avk: &AggregateVerificationKey<D>,
        parameters: &Parameters,
    ) -> Result<(), StmAggregateSignatureError<D>> {
        let msgp = avk.get_merkle_tree_batch_commitment().concatenate_with_message(msg);
        let (sigs, vks) = self.preliminary_verify(msg, avk, parameters)?;

        BlsSignature::verify_aggregate(msgp.as_slice(), &vks, &sigs)?;
        Ok(())
    }

    /// Batch verify a set of signatures, with different messages and avks.
    pub fn batch_verify(
        stm_signatures: &[Self],
        msgs: &[Vec<u8>],
        avks: &[AggregateVerificationKey<D>],
        parameters: &[Parameters],
    ) -> Result<(), StmAggregateSignatureError<D>> {
        let batch_size = stm_signatures.len();
        assert_eq!(
            batch_size,
            msgs.len(),
            "Number of messages should correspond to size of the batch"
        );
        assert_eq!(
            batch_size,
            avks.len(),
            "Number of avks should correspond to size of the batch"
        );
        assert_eq!(
            batch_size,
            parameters.len(),
            "Number of parameters should correspond to size of the batch"
        );

        let mut aggr_sigs = Vec::with_capacity(batch_size);
        let mut aggr_vks = Vec::with_capacity(batch_size);
        for (idx, sig_group) in stm_signatures.iter().enumerate() {
            sig_group.preliminary_verify(&msgs[idx], &avks[idx], &parameters[idx])?;
            let grouped_sigs: Vec<BlsSignature> =
                sig_group.signatures.iter().map(|sig_reg| sig_reg.sig.sigma).collect();
            let grouped_vks: Vec<BlsVerificationKey> = sig_group
                .signatures
                .iter()
                .map(|sig_reg| sig_reg.reg_party.0)
                .collect();

            let (aggr_vk, aggr_sig) = BlsSignature::aggregate(&grouped_vks, &grouped_sigs).unwrap();
            aggr_sigs.push(aggr_sig);
            aggr_vks.push(aggr_vk);
        }

        let concat_msgs: Vec<Vec<u8>> = msgs
            .iter()
            .zip(avks.iter())
            .map(|(msg, avk)| avk.get_merkle_tree_batch_commitment().concatenate_with_message(msg))
            .collect();

        BlsSignature::batch_verify_aggregates(&concat_msgs, &aggr_vks, &aggr_sigs)?;
        Ok(())
    }

    /// Convert concatenation proof to bytes
    /// # Layout
    /// * Number of the pairs of Signatures and Registered Parties (SigRegParty) (as u64)
    /// * Pairs of Signatures and Registered Parties (prefixed with their size as u64)
    /// * Batch proof
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend_from_slice(&u64::try_from(self.signatures.len()).unwrap().to_be_bytes());
        for sig_reg in &self.signatures {
            out.extend_from_slice(&u64::try_from(sig_reg.to_bytes().len()).unwrap().to_be_bytes());
            out.extend_from_slice(&sig_reg.to_bytes());
        }
        let proof = &self.batch_proof;
        out.extend_from_slice(&proof.to_bytes());

        out
    }

    ///Extract a concatenation proof from a byte slice.
    pub fn from_bytes(
        bytes: &[u8],
    ) -> Result<ConcatenationProof<D>, StmAggregateSignatureError<D>> {
        let mut bytes_index = 0;

        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(
            bytes
                .get(bytes_index..bytes_index + 8)
                .ok_or(StmAggregateSignatureError::SerializationError)?,
        );
        let total_sigs = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| StmAggregateSignatureError::SerializationError)?;
        bytes_index += 8;

        let mut sig_reg_list = Vec::with_capacity(total_sigs);
        for _ in 0..total_sigs {
            u64_bytes.copy_from_slice(
                bytes
                    .get(bytes_index..bytes_index + 8)
                    .ok_or(StmAggregateSignatureError::SerializationError)?,
            );
            let sig_reg_size = usize::try_from(u64::from_be_bytes(u64_bytes))
                .map_err(|_| StmAggregateSignatureError::SerializationError)?;
            let sig_reg = SingleSignatureWithRegisteredParty::from_bytes::<D>(
                bytes
                    .get(bytes_index + 8..bytes_index + 8 + sig_reg_size)
                    .ok_or(StmAggregateSignatureError::SerializationError)?,
            )?;
            bytes_index += 8 + sig_reg_size;
            sig_reg_list.push(sig_reg);
        }

        let batch_proof = MerkleBatchPath::from_bytes(
            bytes
                .get(bytes_index..)
                .ok_or(StmAggregateSignatureError::SerializationError)?,
        )?;

        Ok(ConcatenationProof {
            signatures: sig_reg_list,
            batch_proof,
        })
    }
}
