//! Crate specific errors

use crate::merkle_tree::Path;
use blake2::digest::Digest;
use {
    crate::multi_sig::{Signature, VerificationKey, VerificationKeyPoP},
    blst::BLST_ERROR,
};

/// Error types for multi signatures.
#[derive(Debug, thiserror::Error, Eq, PartialEq)]
pub enum MultiSignatureError {
    /// Invalid Single signature
    #[error("Invalid single signature")]
    SignatureInvalid(Signature),

    /// Invalid aggregate signature
    #[error("Invalid aggregated signature")]
    AggregateSignatureInvalid,

    /// This error occurs when the the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,

    /// Incorrect proof of possession
    #[error("Key with invalid PoP")]
    KeyInvalid(Box<VerificationKeyPoP>),
}

/// Errors which can be output by Mithril verification.
#[derive(Debug, Clone, thiserror::Error)]
pub enum StmSignatureError<D: Digest> {
    /// No quorum was found
    #[error("No Quorum was found.")]
    NoQuorum,

    /// The IVK is invalid after aggregating the keys
    #[error("Aggregated key does not correspond to the expected key.")]
    IvkInvalid(VerificationKey),

    /// Mu is not the sum of the signatures
    #[error("Aggregated signature does not correspond to the expected signature.")]
    SumInvalid(Signature),

    /// There is an index out of bounds
    #[error("Received index, {0}, is higher than what the security parameter allows, {1}.")]
    IndexBoundFailed(u64, u64),

    /// There is a duplicate index
    #[error("Indeces are not unique.")]
    IndexNotUnique,

    /// The path is not valid for the Merkle Tree
    #[error("The path of the Merkle Tree is invalid.")]
    PathInvalid(Path<D>),

    /// MSP.Eval was computed incorrectly
    #[error("The claimed evaluation of function phi is incorrect.")]
    EvalInvalid([u8; 64]),

    /// The lottery was actually lost for the signature
    #[error("Lottery for this epoch was lost.")]
    LotteryLost,

    /// A party submitted an invalid signature
    #[error("A provided signature is invalid")]
    SingleSignatureInvalid(Signature),

    /// The aggregated signature is invalid
    #[error("Aggregate signature is invalid")]
    SignatureInvalid,

    /// This error occurs when the the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,
}

/// Error types for aggregation.
#[derive(Debug, Clone, thiserror::Error)]
pub enum AggregationError{
    /// Not enough signatures were collected, got this many instead.
    #[error("Not enough signatures. Got only {0} out of {1}.")]
    NotEnoughSignatures(u64, u64),

    /// This error happens when we try to convert a u64 to a usize and it does not fit
    #[error("Invalid usize conversion")]
    UsizeConversionInvalid,

}


/// Error types related to merkle trees.
#[derive(Debug, Clone, thiserror::Error)]
pub enum MerkleTreeError<D: Digest> {
    /// Serialization error
    #[error("Serialization of a merkle tree failed")]
    SerializationError,

    /// Invalid merkle path
    #[error("Path does not verify against root")]
    PathInvalid(Path<D>),
}

/// Errors which can be outputted by key registration.
#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq)]
pub enum RegisterError {
    /// This key has already been registered by a participant
    #[error("This key has already been registered.")]
    KeyRegistered(Box<VerificationKey>),

    /// The supplied key is not valid
    #[error("The verification of correctness of the supplied key is invalid.")]
    KeyInvalid(Box<VerificationKeyPoP>),

    /// Serialization error
    #[error("Serialization error")]
    SerializationError,

    /// UnregisteredInitializer error
    #[error("Initializer not registered. Cannot participate as a signer.")]
    UnregisteredInitializer,
}

impl<D: Digest> From<RegisterError> for StmSignatureError<D> {
    fn from(e: RegisterError) -> Self {
        match e {
            RegisterError::SerializationError => Self::SerializationError,
            RegisterError::KeyInvalid(e) => Self::IvkInvalid(e.vk),
            RegisterError::KeyRegistered(_) => unreachable!(),
            RegisterError::UnregisteredInitializer => unreachable!(),
        }
    }
}

impl<D: Digest> From<MerkleTreeError<D>> for StmSignatureError<D> {
    fn from(e: MerkleTreeError<D>) -> Self {
        match e {
            MerkleTreeError::PathInvalid(e) => Self::PathInvalid(e),
            MerkleTreeError::SerializationError => Self::SerializationError,
        }
    }
}

impl<D: Digest> From<MultiSignatureError> for StmSignatureError<D> {
    fn from(e: MultiSignatureError) -> Self {
        match e {
            MultiSignatureError::SerializationError => Self::SerializationError,
            MultiSignatureError::KeyInvalid(e) => Self::IvkInvalid(e.vk),
            MultiSignatureError::SignatureInvalid(e) => Self::SingleSignatureInvalid(e),
            MultiSignatureError::AggregateSignatureInvalid => Self::SignatureInvalid,
        }
    }
}

impl From<MultiSignatureError> for RegisterError {
    fn from(e: MultiSignatureError) -> Self {
        match e {
            MultiSignatureError::SerializationError => Self::SerializationError,
            MultiSignatureError::KeyInvalid(e) => Self::KeyInvalid(e),
            MultiSignatureError::SignatureInvalid(_) => unreachable!(),
            MultiSignatureError::AggregateSignatureInvalid => unreachable!(),
        }
    }
}




/// If verifying a single signature, the signature should be provided. If verifying a multi-sig,
/// no need to provide the signature
pub(crate) fn blst_err_to_atms(
    e: BLST_ERROR,
    sig: Option<Signature>,
) -> Result<(), MultiSignatureError> {
    match e {
        BLST_ERROR::BLST_SUCCESS => Ok(()),
        BLST_ERROR::BLST_VERIFY_FAIL => {
            if let Some(s) = sig {
                Err(MultiSignatureError::SignatureInvalid(s))
            } else {
                Err(MultiSignatureError::AggregateSignatureInvalid)
            }
        }
        _ => Err(MultiSignatureError::SerializationError),
    }
}
