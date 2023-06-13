//! Crate specific errors

use crate::merkle_tree::{BatchPath, Path};
use crate::AggregationError::NotEnoughSignatures;
use crate::RegisterError::SerializationError;
use blake2::digest::{Digest, FixedOutput};
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

    /// At least one signature in the batch is invalid
    #[error("One signature in the batch is invalid")]
    BatchInvalid,
}

/// Errors which can be output by Mithril single signature verification.
#[derive(Debug, Clone, thiserror::Error)]
pub enum StmSignatureError {
    /// There is an index out of bounds
    #[error("Received index, {0}, is higher than what the security parameter allows, {1}.")]
    IndexBoundFailed(u64, u64),

    /// MSP.Eval was computed incorrectly
    #[error("The claimed evaluation of function phi is incorrect.")]
    EvalInvalid([u8; 64]),

    /// The lottery was actually lost for the signature
    #[error("Lottery for this epoch was lost.")]
    LotteryLost,

    /// A party submitted an invalid signature
    #[error("A provided signature is invalid")]
    SignatureInvalid(Signature),

    /// Batch verification of STM signatures failed
    #[error("Batch verification of STM signatures failed")]
    BatchInvalid,

    /// This error occurs when the the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,
}

/// Errors which can be output by Mithril aggregate verification.
#[derive(Debug, Clone, thiserror::Error)]
pub enum StmAggregateSignatureError<D: Digest + FixedOutput> {
    /// The IVK is invalid after aggregating the keys
    #[error("Aggregated key does not correspond to the expected key.")]
    IvkInvalid(Box<VerificationKey>),

    /// This error occurs when the the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,

    /// Invalid merkle batch path
    #[error("Batch path does not verify against root")]
    PathInvalid(BatchPath<D>),

    /// Batch verification of STM aggregate signatures failed
    #[error("Batch verification of STM aggregate signatures failed")]
    BatchInvalid,

    #[error("Core verification error: {0}")]
    CoreVerificationError(CoreVerifierError),
}

/// Errors which can be output by Mithril aggregate verification.
#[derive(Debug, Clone, thiserror::Error)]
pub enum CoreVerifierError {
    /// No quorum was found
    #[error("No Quorum was found.")]
    NoQuorum,

    /// There is a duplicate index
    #[error("Indices are not unique.")]
    IndexNotUnique,

    /// The aggregated signature is invalid
    #[error("Aggregate signature is invalid")]
    AggregateSignatureInvalid,

    /// One of the aggregated signatures is invalid
    #[error("Individual signature is invalid: {0}")]
    IndividualSignatureInvalid(StmSignatureError),
}

/// Errors which can be output by StmSigRegParty .
#[derive(Debug, Clone, thiserror::Error)]
pub enum StmSigRegPartyError {
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

    /// This error happens when we try to convert a u64 to a usize and it does not fit
    #[error("Invalid usize conversion")]
    UsizeConversionInvalid,
}

/// Error types related to merkle trees.
#[derive(Debug, Clone, thiserror::Error)]
pub enum MerkleTreeError<D: Digest + FixedOutput> {
    /// Serialization error
    #[error("Serialization of a merkle tree failed")]
    SerializationError,

    /// Invalid merkle path
    #[error("Path does not verify against root")]
    PathInvalid(Path<D>),

    /// Invalid merkle batch path
    #[error("Batch path does not verify against root")]
    BatchPathInvalid(BatchPath<D>),
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

impl From<MultiSignatureError> for StmSignatureError {
    fn from(e: MultiSignatureError) -> Self {
        match e {
            MultiSignatureError::SerializationError => Self::SerializationError,
            MultiSignatureError::SignatureInvalid(e) => Self::SignatureInvalid(e),
            MultiSignatureError::BatchInvalid => unreachable!(),
            MultiSignatureError::KeyInvalid(_) => unreachable!(),
            MultiSignatureError::AggregateSignatureInvalid => unreachable!(),
        }
    }
}

impl<D: Digest + FixedOutput> From<MerkleTreeError<D>> for StmAggregateSignatureError<D> {
    fn from(e: MerkleTreeError<D>) -> Self {
        match e {
            MerkleTreeError::BatchPathInvalid(e) => Self::PathInvalid(e),
            MerkleTreeError::SerializationError => Self::SerializationError,
            MerkleTreeError::PathInvalid(_e) => unreachable!(),
        }
    }
}

impl<D: Digest + FixedOutput> From<MultiSignatureError> for StmAggregateSignatureError<D> {
    fn from(e: MultiSignatureError) -> Self {
        match e {
            MultiSignatureError::AggregateSignatureInvalid => {
                Self::CoreVerificationError(CoreVerifierError::from(e))
            }
            MultiSignatureError::BatchInvalid => Self::BatchInvalid,
            MultiSignatureError::SerializationError => unreachable!(),
            MultiSignatureError::KeyInvalid(_) => unreachable!(),
            MultiSignatureError::SignatureInvalid(_) => {
                Self::CoreVerificationError(CoreVerifierError::from(e))
            }
        }
    }
}

impl<D: Digest + FixedOutput> From<CoreVerifierError> for StmAggregateSignatureError<D> {
    fn from(e: CoreVerifierError) -> Self {
        match e {
            CoreVerifierError::IndexNotUnique => Self::CoreVerificationError(e),
            CoreVerifierError::NoQuorum => Self::CoreVerificationError(e),
            CoreVerifierError::IndividualSignatureInvalid(_) => Self::CoreVerificationError(e),
            CoreVerifierError::AggregateSignatureInvalid => Self::CoreVerificationError(e),
        }
    }
}

impl<D: Digest + FixedOutput> From<StmSigRegPartyError> for StmAggregateSignatureError<D> {
    fn from(_e: StmSigRegPartyError) -> Self {
        StmAggregateSignatureError::SerializationError
    }
}

impl From<StmSignatureError> for StmSigRegPartyError {
    fn from(_e: StmSignatureError) -> Self {
        StmSigRegPartyError::SerializationError
    }
}

impl<D: Digest + FixedOutput> From<MerkleTreeError<D>> for StmSigRegPartyError {
    fn from(_e: MerkleTreeError<D>) -> Self {
        StmSigRegPartyError::SerializationError
    }
}

impl From<AggregationError> for CoreVerifierError {
    fn from(e: AggregationError) -> Self {
        match e {
            NotEnoughSignatures(_, _) => Self::NoQuorum,
            AggregationError::UsizeConversionInvalid => unreachable!(),
        }
    }
}

impl From<MultiSignatureError> for CoreVerifierError {
    fn from(e: MultiSignatureError) -> Self {
        match e {
            MultiSignatureError::AggregateSignatureInvalid => Self::AggregateSignatureInvalid,
            MultiSignatureError::BatchInvalid => unreachable!(),
            MultiSignatureError::SerializationError => unreachable!(),
            MultiSignatureError::KeyInvalid(_) => unreachable!(),
            MultiSignatureError::SignatureInvalid(_e) => unreachable!(),
        }
    }
}

impl From<StmSignatureError> for CoreVerifierError {
    fn from(e: StmSignatureError) -> Self {
        CoreVerifierError::IndividualSignatureInvalid(e)
    }
}

impl From<MultiSignatureError> for RegisterError {
    fn from(e: MultiSignatureError) -> Self {
        match e {
            MultiSignatureError::SerializationError => SerializationError,
            MultiSignatureError::KeyInvalid(e) => Self::KeyInvalid(e),
            MultiSignatureError::SignatureInvalid(_) => unreachable!(),
            MultiSignatureError::AggregateSignatureInvalid => unreachable!(),
            MultiSignatureError::BatchInvalid => unreachable!(),
        }
    }
}

/// If verifying a single signature, the signature should be provided. If verifying a multi-sig,
/// no need to provide the signature
pub(crate) fn blst_err_to_mithril(
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
