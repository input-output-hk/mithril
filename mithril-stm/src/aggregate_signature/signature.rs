use std::collections::HashMap;
use std::hash::Hash;

use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};
use strum::{Display, EnumDiscriminants};

use crate::error::StmAggregateSignatureError;
use crate::merkle_tree::MerkleBatchPath;
use crate::{AggregateVerificationKey, Parameters, SingleSignatureWithRegisteredParty};

use super::ConcatenationProof;

/// A STM aggregate signature.
#[derive(Debug, Clone, Serialize, Deserialize, EnumDiscriminants)]
#[strum(serialize_all = "PascalCase")]
#[strum_discriminants(derive(Serialize, Hash, Display))] // TODO: replace strum with custom implementation
#[strum_discriminants(name(AggregateSignatureType))]
#[serde(bound(
    serialize = "MerkleBatchPath<D>: Serialize",
    deserialize = "MerkleBatchPath<D>: Deserialize<'de>"
))]
#[serde(untagged)]
pub enum AggregateSignature<D: Clone + Digest + FixedOutput + Send + Sync> {
    /// Concatenation proof system.
    Concatenation(ConcatenationProof<D>),
}

impl<D: Clone + Digest + FixedOutput + Send + Sync> AggregateSignature<D> {
    /// Verify an aggregate signature
    pub fn verify(
        &self,
        msg: &[u8],
        avk: &AggregateVerificationKey<D>,
        parameters: &Parameters,
    ) -> Result<(), StmAggregateSignatureError<D>> {
        match self {
            AggregateSignature::Concatenation(stm_aggr_sig) => {
                stm_aggr_sig.verify(msg, avk, parameters)
            }
        }
    }

    /// Batch verify a set of aggregate signatures
    pub fn batch_verify(
        stm_signatures: &[Self],
        msgs: &[Vec<u8>],
        avks: &[AggregateVerificationKey<D>],
        parameters: &[Parameters],
    ) -> Result<(), StmAggregateSignatureError<D>> {
        let stm_signatures: HashMap<AggregateSignatureType, Vec<Self>> =
            stm_signatures.iter().fold(HashMap::new(), |mut acc, sig| {
                acc.entry(sig.into()).or_default().push(sig.clone());
                acc
            });
        stm_signatures
            .into_iter()
            .try_for_each(
                |(stm_aggr_sig_type, stm_aggr_sigs)| match stm_aggr_sig_type {
                    AggregateSignatureType::Concatenation => ConcatenationProof::batch_verify(
                        &stm_aggr_sigs
                            .into_iter()
                            .filter_map(|s| match s {
                                Self::Concatenation(stm_aggr_sig) => Some(stm_aggr_sig),
                            })
                            .collect::<Vec<_>>(),
                        msgs,
                        avks,
                        parameters,
                    ),
                },
            )
            .map_err(|_| StmAggregateSignatureError::BatchInvalid)
    }

    /// Convert an aggregate signature to bytes
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            AggregateSignature::Concatenation(stm_aggr_sig) => stm_aggr_sig.to_bytes(),
        }
    }

    /// Extract an aggregate signature from a byte slice.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, StmAggregateSignatureError<D>> {
        // TODO: Move first byte of concatenation proof here to identify the proof system
        Ok(Self::Concatenation(ConcatenationProof::from_bytes(bytes)?))
    }

    /// Extract the list of signatures.
    pub fn signatures(&self) -> Vec<SingleSignatureWithRegisteredParty> {
        match self {
            AggregateSignature::Concatenation(stm_aggr_sig) => stm_aggr_sig.signatures.clone(),
        }
    }

    /// Extract the list of unique merkle tree nodes that covers path for all signatures.
    // TODO: transfer this function to the concatenation proof
    pub fn batch_proof(&self) -> MerkleBatchPath<D> {
        match self {
            AggregateSignature::Concatenation(stm_aggr_sig) => stm_aggr_sig.batch_proof.clone(),
        }
    }

    /// Extract the list of unique merkle tree nodes that covers path for all signatures. (test only)
    // TODO: transfer this function to the concatenation proof
    #[cfg(test)]
    pub(crate) fn set_batch_proof(&mut self, batch_proof: MerkleBatchPath<D>) {
        match self {
            AggregateSignature::Concatenation(stm_aggr_sig) => {
                stm_aggr_sig.batch_proof = batch_proof
            }
        }
    }
}
