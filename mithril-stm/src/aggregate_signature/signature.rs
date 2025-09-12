use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;

use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};

use crate::error::StmAggregateSignatureError;
use crate::merkle_tree::MerkleBatchPath;
use crate::{AggregateVerificationKey, Parameters, SingleSignatureWithRegisteredParty};

use super::ConcatenationProof;

/// The type of STM aggregate signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AggregateSignatureType {
    /// Concatenation proof system.
    Concatenation,
    /// Future proof system. Not suitable for production.
    #[cfg(feature = "future_proof_system")]
    Future,
}

impl AggregateSignatureType {
    /// The prefix byte used in the byte representation of the aggregate signature type.
    ///
    /// IMPORTANT: This value is used in serialization/deserialization. Changing it will break compatibility.
    pub fn to_bytes_encoding_prefix(&self) -> u8 {
        match self {
            AggregateSignatureType::Concatenation => 0,
            #[cfg(feature = "future_proof_system")]
            AggregateSignatureType::Future => 255,
        }
    }

    /// Create an aggregate signature type from a prefix byte.
    ///
    /// IMPORTANT: This value is used in serialization/deserialization. Changing it will break compatibility.
    pub fn from_bytes_encoding_prefix(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(AggregateSignatureType::Concatenation),
            #[cfg(feature = "future_proof_system")]
            255 => Some(AggregateSignatureType::Future),
            _ => None,
        }
    }
}

impl<D: Clone + Digest + FixedOutput + Send + Sync> From<&AggregateSignature<D>>
    for AggregateSignatureType
{
    fn from(aggr_sig: &AggregateSignature<D>) -> Self {
        match aggr_sig {
            AggregateSignature::Concatenation(_) => AggregateSignatureType::Concatenation,
            #[cfg(feature = "future_proof_system")]
            AggregateSignature::Future => AggregateSignatureType::Future,
        }
    }
}

impl Display for AggregateSignatureType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AggregateSignatureType::Concatenation => write!(f, "Concatenation"),
            #[cfg(feature = "future_proof_system")]
            AggregateSignatureType::Future => write!(f, "Future"),
        }
    }
}

/// An STM aggregate signature.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "MerkleBatchPath<D>: Serialize",
    deserialize = "MerkleBatchPath<D>: Deserialize<'de>"
))]
pub enum AggregateSignature<D: Clone + Digest + FixedOutput + Send + Sync> {
    /// A future proof system.
    #[cfg(feature = "future_proof_system")]
    Future,

    /// Concatenation proof system.
    // The 'untagged' attribute is required for backward compatibility.
    // It implies that this variant is placed at the end of the enum.
    // It will be removed when the support for JSON hex encoding is dropped in the calling crates.
    #[serde(untagged)]
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
            AggregateSignature::Concatenation(concatenation_proof) => {
                concatenation_proof.verify(msg, avk, parameters)
            }
            #[cfg(feature = "future_proof_system")]
            AggregateSignature::Future => Err(StmAggregateSignatureError::UnsupportedProofSystem(
                self.into(),
            )),
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
            .try_for_each(|(aggregate_signature_type, aggregate_signatures)| {
                match aggregate_signature_type {
                    AggregateSignatureType::Concatenation => {
                        let aggregate_signatures_length = aggregate_signatures.len();
                        let concatenation_proofs = aggregate_signatures
                            .into_iter()
                            .filter_map(|s| s.to_concatenation_proof().cloned())
                            .collect::<Vec<_>>();
                        if concatenation_proofs.len() != aggregate_signatures_length {
                            return Err(StmAggregateSignatureError::BatchInvalid);
                        }

                        ConcatenationProof::batch_verify(
                            &concatenation_proofs,
                            msgs,
                            avks,
                            parameters,
                        )
                    }
                    #[cfg(feature = "future_proof_system")]
                    AggregateSignatureType::Future => {
                        Err(StmAggregateSignatureError::UnsupportedProofSystem(
                            aggregate_signature_type,
                        ))
                    }
                }
            })
            .map_err(|_| StmAggregateSignatureError::BatchInvalid)
    }

    /// Convert an aggregate signature to bytes
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut aggregate_signature_bytes = Vec::new();
        let aggregate_signature_type: AggregateSignatureType = self.into();
        aggregate_signature_bytes
            .extend_from_slice(&[aggregate_signature_type.to_bytes_encoding_prefix()]);

        let mut proof_bytes = match self {
            AggregateSignature::Concatenation(concatenation_proof) => {
                concatenation_proof.to_bytes()
            }
            #[cfg(feature = "future_proof_system")]
            AggregateSignature::Future => vec![],
        };
        aggregate_signature_bytes.append(&mut proof_bytes);

        aggregate_signature_bytes
    }

    /// Extract an aggregate signature from a byte slice.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, StmAggregateSignatureError<D>> {
        let proof_type_byte = bytes.get(0).ok_or(StmAggregateSignatureError::SerializationError)?;
        let proof_bytes = &bytes[1..];
        let proof_type = AggregateSignatureType::from_bytes_encoding_prefix(*proof_type_byte)
            .ok_or(StmAggregateSignatureError::SerializationError)?;
        match proof_type {
            AggregateSignatureType::Concatenation => Ok(AggregateSignature::Concatenation(
                ConcatenationProof::from_bytes(proof_bytes)?,
            )),
            #[cfg(feature = "future_proof_system")]
            AggregateSignatureType::Future => Ok(AggregateSignature::Future),
        }
    }

    /// If the aggregate signature is a concatenation proof, return it.
    pub fn to_concatenation_proof(&self) -> Option<&ConcatenationProof<D>> {
        match self {
            AggregateSignature::Concatenation(proof) => Some(proof),
            #[cfg(feature = "future_proof_system")]
            AggregateSignature::Future => None,
        }
    }

    /// Extract the list of signatures.
    // TODO: transfer this function to the concatenation proof ? Some proofs might not fully carry this information
    pub fn signatures(&self) -> Vec<SingleSignatureWithRegisteredParty> {
        match self {
            AggregateSignature::Concatenation(concatenation_proof) => {
                concatenation_proof.signatures.clone()
            }
            #[cfg(feature = "future_proof_system")]
            AggregateSignature::Future(concatenation_proof) => {
                concatenation_proof.signatures.clone()
            }
        }
    }

    /// Extract the list of unique merkle tree nodes that covers path for all signatures.
    // TODO: transfer this function to the concatenation proof
    pub fn batch_proof(&self) -> MerkleBatchPath<D> {
        match self {
            AggregateSignature::Concatenation(concatenation_proof) => {
                concatenation_proof.batch_proof.clone()
            }
            #[cfg(feature = "future_proof_system")]
            AggregateSignature::Future(concatenation_proof) => {
                concatenation_proof.batch_proof.clone()
            }
        }
    }

    /// Extract the list of unique merkle tree nodes that covers path for all signatures. (test only)
    // TODO: transfer this function to the concatenation proof
    #[cfg(test)]
    pub(crate) fn set_batch_proof(&mut self, batch_proof: MerkleBatchPath<D>) {
        match self {
            AggregateSignature::Concatenation(concatenation_proof) => {
                concatenation_proof.batch_proof = batch_proof
            }
            #[cfg(feature = "future_proof_system")]
            AggregateSignature::Future(concatenation_proof) => {
                concatenation_proof.batch_proof = batch_proof
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod aggregate_signature_type {
        use super::*;

        #[test]
        fn golden_bytes_encoding_prefix() {
            assert_eq!(
                0u8,
                AggregateSignatureType::Concatenation.to_bytes_encoding_prefix()
            );
            assert_eq!(
                AggregateSignatureType::from_bytes_encoding_prefix(0u8),
                Some(AggregateSignatureType::Concatenation)
            );
        }
    }
}
