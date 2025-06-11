use blake2::digest::{Digest, FixedOutput};

use serde::{Deserialize, Serialize};

use crate::bls_multi_signature::{Signature, VerificationKey};
use crate::key_reg::RegParty;
use crate::merkle_tree::BatchPath;
use crate::{
    CoreVerifier, StmAggrVerificationKey, StmAggregateSignatureError, StmParameters, StmSigRegParty,
};

/// `StmMultiSig` uses the "concatenation" proving system (as described in Section 4.3 of the original paper.)
/// This means that the aggregated signature contains a vector with all individual signatures.
/// BatchPath is also a part of the aggregate signature which covers path for all signatures.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "BatchPath<D>: Serialize",
    deserialize = "BatchPath<D>: Deserialize<'de>"
))]
pub struct StmAggrSig<D: Clone + Digest + FixedOutput> {
    pub(crate) signatures: Vec<StmSigRegParty>,
    /// The list of unique merkle tree nodes that covers path for all signatures.
    pub batch_proof: BatchPath<D>,
}

impl<D: Clone + Digest + FixedOutput + Send + Sync> StmAggrSig<D> {
    /// Verify all checks from signatures, except for the signature verification itself.
    ///
    /// Indices and quorum are checked by `CoreVerifier::preliminary_verify` with `msgp`.
    /// It collects leaves from signatures and checks the batch proof.
    /// After batch proof is checked, it collects and returns the signatures and
    /// verification keys to be used by aggregate verification.
    fn preliminary_verify(
        &self,
        msg: &[u8],
        avk: &StmAggrVerificationKey<D>,
        parameters: &StmParameters,
    ) -> Result<(Vec<Signature>, Vec<VerificationKey>), StmAggregateSignatureError<D>> {
        let msgp = avk.get_mt_commitment().concat_with_msg(msg);
        CoreVerifier::preliminary_verify(
            &avk.get_total_stake(),
            &self.signatures,
            parameters,
            &msgp,
        )?;

        let leaves = self
            .signatures
            .iter()
            .map(|r| r.reg_party)
            .collect::<Vec<RegParty>>();

        avk.get_mt_commitment().check(&leaves, &self.batch_proof)?;

        Ok(CoreVerifier::collect_sigs_vks(&self.signatures))
    }

    /// Verify aggregate signature, by checking that
    /// * each signature contains only valid indices,
    /// * the lottery is indeed won by each one of them,
    /// * the merkle tree path is valid,
    /// * the aggregate signature validates with respect to the aggregate verification key
    ///   (aggregation is computed using functions `MSP.BKey` and `MSP.BSig` as described in Section 2.4 of the paper).
    pub fn verify(
        &self,
        msg: &[u8],
        avk: &StmAggrVerificationKey<D>,
        parameters: &StmParameters,
    ) -> Result<(), StmAggregateSignatureError<D>> {
        let msgp = avk.get_mt_commitment().concat_with_msg(msg);
        let (sigs, vks) = self.preliminary_verify(msg, avk, parameters)?;

        Signature::verify_aggregate(msgp.as_slice(), &vks, &sigs)?;
        Ok(())
    }

    /// Batch verify a set of signatures, with different messages and avks.
    #[cfg(feature = "batch-verify-aggregates")]
    pub fn batch_verify(
        stm_signatures: &[Self],
        msgs: &[Vec<u8>],
        avks: &[StmAggrVerificationKey<D>],
        parameters: &[StmParameters],
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
            let grouped_sigs: Vec<Signature> = sig_group
                .signatures
                .iter()
                .map(|sig_reg| sig_reg.sig.sigma)
                .collect();
            let grouped_vks: Vec<VerificationKey> = sig_group
                .signatures
                .iter()
                .map(|sig_reg| sig_reg.reg_party.0)
                .collect();

            let (aggr_vk, aggr_sig) = Signature::aggregate(&grouped_vks, &grouped_sigs).unwrap();
            aggr_sigs.push(aggr_sig);
            aggr_vks.push(aggr_vk);
        }

        let concat_msgs: Vec<Vec<u8>> = msgs
            .iter()
            .zip(avks.iter())
            .map(|(msg, avk)| avk.get_mt_commitment().concat_with_msg(msg))
            .collect();

        Signature::batch_verify_aggregates(&concat_msgs, &aggr_vks, &aggr_sigs)?;
        Ok(())
    }

    /// Convert multi signature to bytes
    /// # Layout
    /// * Number of the pairs of Signatures and Registered Parties (SigRegParty) (as u64)
    /// * Size of a pair of Signature and Registered Party
    /// * Pairs of Signatures and Registered Parties
    /// * Batch proof
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
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

        out
    }

    ///Extract a `StmAggrSig` from a byte slice.
    pub fn from_bytes(bytes: &[u8]) -> Result<StmAggrSig<D>, StmAggregateSignatureError<D>> {
        let mut u64_bytes = [0u8; 8];

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

        Ok(StmAggrSig {
            signatures: sig_reg_list,
            batch_proof,
        })
    }
}
