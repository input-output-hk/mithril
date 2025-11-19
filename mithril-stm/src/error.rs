//! Crate specific errors
use anyhow::anyhow;
use blst::BLST_ERROR;

use crate::StmResult;
use crate::aggregate_signature::AggregateSignatureType;
use crate::bls_multi_signature::{
    BlsSignature, BlsVerificationKey, BlsVerificationKeyProofOfPossession,
};

/// Error types for multi signatures.
#[derive(Debug, thiserror::Error, Eq, PartialEq)]
pub enum MultiSignatureError {
    /// Invalid Single signature
    #[error("Invalid single signature")]
    SignatureInvalid(BlsSignature),

    /// Invalid aggregate signature
    #[error("Invalid aggregated signature")]
    AggregateSignatureInvalid,

    /// This error occurs when the the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,

    /// Incorrect proof of possession
    #[error("Key with invalid PoP")]
    KeyInvalid(Box<BlsVerificationKeyProofOfPossession>),

    /// At least one signature in the batch is invalid
    #[error("One signature in the batch is invalid")]
    BatchInvalid,

    /// Single signature is the infinity
    #[error("Single signature is the infinity")]
    SignatureInfinity(BlsSignature),

    /// Verification key is the infinity
    #[error("Verification key is the infinity")]
    VerificationKeyInfinity(Box<BlsVerificationKey>),
}

/// Error types related to merkle trees.
#[derive(Debug, Clone, thiserror::Error)]
pub enum MerkleTreeError {
    /// Serialization error
    #[error("Serialization of a merkle tree failed")]
    SerializationError,

    /// Invalid merkle path
    #[error("Path does not verify against root")]
    PathInvalid(Vec<u8>),

    /// Invalid merkle batch path
    #[error("Batch path does not verify against root")]
    BatchPathInvalid(Vec<u8>),
}

/// Errors which can be output by Mithril single signature verification.
#[derive(Debug, Clone, thiserror::Error)]
pub enum StmSignatureError {
    /// There is an index out of bounds
    #[error("Received index, {0}, is higher than what the security parameter allows, {1}.")]
    IndexBoundFailed(u64, u64),

    /// The lottery was actually lost for the signature
    #[error("Lottery for this epoch was lost.")]
    LotteryLost,

    /// This error occurs when the the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,
}

/// Error types for aggregation.
#[derive(Debug, Clone, thiserror::Error)]
pub enum AggregationError {
    /// Not enough signatures were collected, got this many instead.
    #[error("Not enough signatures. Got only {0} out of {1}.")]
    NotEnoughSignatures(u64, u64),

    #[error("Unsupported proof system: {0}")]
    UnsupportedProofSystem(AggregateSignatureType),

    /// There is a duplicate index
    #[error("Indices are not unique.")]
    IndexNotUnique,
}

/// Errors which can be output by Mithril aggregate verification.
#[derive(Debug, Clone, thiserror::Error)]
pub enum StmAggregateSignatureError {
    /// This error occurs when the the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,

    /// Batch verification of STM aggregate signatures failed
    #[error("Batch verification of STM aggregate signatures failed")]
    BatchInvalid,

    /// The proof system used in the aggregate signature is not supported
    #[error("Unsupported proof system: {0}")]
    UnsupportedProofSystem(AggregateSignatureType),
}

/// Errors which can be outputted by key registration.
#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq)]
pub enum RegisterError {
    /// This key has already been registered by a participant
    #[error("This key has already been registered.")]
    KeyRegistered(Box<BlsVerificationKey>),

    /// The supplied key is not valid
    #[error("The verification of correctness of the supplied key is invalid.")]
    KeyInvalid(Box<BlsVerificationKeyProofOfPossession>),

    /// Serialization error
    #[error("Serialization error")]
    SerializationError,

    /// UnregisteredInitializer error
    #[error("Initializer not registered. Cannot participate as a signer.")]
    UnregisteredInitializer,
}

pub(crate) fn blst_error_to_stm_error(
    e: BLST_ERROR,
    sig: Option<BlsSignature>,
    key: Option<BlsVerificationKey>,
) -> StmResult<()> {
    match e {
        BLST_ERROR::BLST_SUCCESS => Ok(()),
        BLST_ERROR::BLST_PK_IS_INFINITY => {
            if let Some(s) = sig {
                return Err(anyhow!(MultiSignatureError::SignatureInfinity(s)));
            }
            if let Some(vk) = key {
                return Err(anyhow!(MultiSignatureError::VerificationKeyInfinity(
                    Box::new(vk)
                )));
            }
            Err(anyhow!(MultiSignatureError::SerializationError))
        }
        BLST_ERROR::BLST_VERIFY_FAIL => {
            if let Some(s) = sig {
                Err(anyhow!(MultiSignatureError::SignatureInvalid(s)))
            } else {
                Err(anyhow!(MultiSignatureError::AggregateSignatureInvalid))
            }
        }
        _ => Err(anyhow!(MultiSignatureError::SerializationError)),
    }
}
