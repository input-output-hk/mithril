use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use anyhow::{Context, anyhow};
use serde::{Deserialize, Serialize};

use crate::{
    AggregateVerificationKey, Index, MembershipDigest, Parameters, SignatureError, Stake,
    StmResult, VerificationKey, is_lottery_won, signature_scheme::BlsSignature,
};

/// Single signature for the concatenation proof system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SingleSignatureForConcatenation {
    /// The underlying BLS signature
    pub sigma: BlsSignature,
    /// The index(es) for which the signature is valid
    pub indexes: Vec<Index>,
}

impl SingleSignatureForConcatenation {
    /// Verify a `SingleSignatureForConcatenation` by validating the underlying BLS signature and checking
    /// that the lottery was won for all indexes.
    pub fn verify<D: MembershipDigest>(
        &self,
        params: &Parameters,
        pk: &VerificationKey,
        stake: &Stake,
        avk: &AggregateVerificationKey<D>,
        msg: &[u8],
    ) -> StmResult<()> {
        let msgp = avk.get_merkle_tree_batch_commitment().concatenate_with_message(msg);
        self.sigma
            .verify(&msgp, pk)
            .with_context(|| "Single signature verification failed.")?;
        self.check_indices(params, stake, &msgp, &avk.get_total_stake())
            .with_context(|| "Single signature verification failed.")?;
        Ok(())
    }

    /// Verify that all indices of single signature are valid by cehcking bounds and lottery win.
    pub(crate) fn check_indices(
        &self,
        params: &Parameters,
        stake: &Stake,
        msg: &[u8],
        total_stake: &Stake,
    ) -> StmResult<()> {
        for &index in &self.indexes {
            if index > params.m {
                return Err(anyhow!(SignatureError::IndexBoundFailed(index, params.m)));
            }

            let ev = self.sigma.evaluate_dense_mapping(msg, index);

            if !is_lottery_won(params.phi_f, ev, *stake, *total_stake) {
                return Err(anyhow!(SignatureError::LotteryLost));
            }
        }
        Ok(())
    }
    /// Convert a `SingleSignatureForConcatenation` into bytes
    ///
    /// # Layout
    /// * Number of valid indices (as u64)
    /// * Winning indices for the signature
    /// * BLS Signature
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend_from_slice(&(self.indexes.len() as u64).to_be_bytes());

        for index in &self.indexes {
            output.extend_from_slice(&index.to_be_bytes());
        }

        output.extend_from_slice(&self.sigma.to_bytes());
        output
    }

    /// Extract a `SingleSignatureForConcatenation` from a byte slice.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<SingleSignatureForConcatenation> {
        let mut u64_bytes = [0u8; 8];

        u64_bytes.copy_from_slice(bytes.get(0..8).ok_or(SignatureError::SerializationError)?);
        let nr_indexes = u64::from_be_bytes(u64_bytes) as usize;

        let mut indexes = Vec::new();
        for i in 0..nr_indexes {
            u64_bytes.copy_from_slice(
                bytes
                    .get(8 + i * 8..16 + i * 8)
                    .ok_or(SignatureError::SerializationError)?,
            );
            indexes.push(u64::from_be_bytes(u64_bytes));
        }

        let offset = 8 + nr_indexes * 8;
        let sigma = BlsSignature::from_bytes(
            bytes
                .get(offset..offset + 48)
                .ok_or(SignatureError::SerializationError)?,
        )?;

        Ok(SingleSignatureForConcatenation { sigma, indexes })
    }
}

impl Hash for SingleSignatureForConcatenation {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.sigma.to_bytes(), state)
    }
}

impl PartialEq for SingleSignatureForConcatenation {
    fn eq(&self, other: &Self) -> bool {
        self.sigma == other.sigma
    }
}

impl Eq for SingleSignatureForConcatenation {}

impl PartialOrd for SingleSignatureForConcatenation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for SingleSignatureForConcatenation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.sigma.cmp(&other.sigma)
    }
}
