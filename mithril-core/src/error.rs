//! Crate specific errors

use crate::merkle_tree::Path;
use crate::multi_sig_blstrs::{Signature, VerificationKey, VerificationKeyPoP};
use crate::stm::PartyId;
// use blst::BLST_ERROR;
use digest::{Digest, FixedOutput};

// todo: better organise these errors.

#[derive(Debug, thiserror::Error, Eq, PartialEq)]
/// Error types for multi signatures
pub enum MultiSignatureError {
    /// Invalid Multi signature
    #[error("Invalid multi signature")]
    InvalidSignature,
    /// This error occurs when the underlying function is passed infinity or an element outsize of the group
    #[error("Unexpected point")]
    UnexpectedBlstTypes,
    /// This error occurs when the the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,
    /// Incorrect proof of possession
    #[error("Key with invalid PoP")]
    InvalidKey(Box<VerificationKeyPoP>),
}

/// Errors which can be output by Mithril verification.
#[derive(Debug, Clone, thiserror::Error)]
pub enum MithrilWitnessError<D: Digest + FixedOutput> {
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
    /// A party did not actually win the lottery
    #[error("The current party did not win the lottery.")]
    StakeInvalid,
    /// A party submitted an invalid signature
    #[error("A provided signature is invalid")]
    InvalidSignature(Signature),
}

#[allow(clippy::from_over_into)]
impl<D: Digest + FixedOutput> Into<i64> for MithrilWitnessError<D> {
    fn into(self) -> i64 {
        // -1 is reserved to the function failing.
        match self {
            MithrilWitnessError::NoQuorum => -2,
            MithrilWitnessError::IvkInvalid(_) => -3,
            MithrilWitnessError::SumInvalid(_) => -4,
            MithrilWitnessError::IndexBoundFailed(_, _) => -5,
            MithrilWitnessError::IndexNotUnique => -6,
            MithrilWitnessError::PathInvalid(_) => -7,
            MithrilWitnessError::EvalInvalid(_) => -8,
            MithrilWitnessError::StakeInvalid => -9,
            MithrilWitnessError::InvalidSignature(_) => -10,
        }
    }
}

/// Error types for aggregation.
#[derive(Debug, Clone, thiserror::Error)]
pub enum AggregationFailure {
    /// Not enough signatures were collected, got this many instead.
    #[error("Not enough signatures. Got only {0} out of {1}.")]
    NotEnoughSignatures(u64, u64),
    /// This error happens when we try to convert a u64 to a usize and it does not fit
    #[error("Invalid usize conversion")]
    InvalidUsizeConversion,
}

/// Error types for single signature verification
#[derive(Debug, Clone, thiserror::Error)]
pub enum VerificationFailure<D: Digest + FixedOutput> {
    /// The signature index is out of bounds
    #[error("Received index, {0}, is higher than what the security parameter allows, {1}.")]
    IndexBoundFailed(u64, u64),
    /// The lottery was actually lost for the signature
    #[error("Lottery for this epoch was lost.")]
    LotteryLost,
    /// The Merkle Tree is invalid
    #[error("The path of the Merkle Tree is invalid.")]
    InvalidMerkleTree(Path<D>),
    /// The MSP signature is invalid
    #[error("Invalid Signature.")]
    InvalidSignature(Signature),
}

/// Error types related to merkle trees
#[derive(Debug, Clone, thiserror::Error)]
pub enum MerkleTreeError {
    /// Serialization error
    #[error("Serialization of a merkle tree failed")]
    SerializationError,
    /// Invalid merkle path
    #[error("Path does not verify against root")]
    InvalidPath,
}

/// Errors which can be outputted by key registration.
#[derive(Debug, Clone, thiserror::Error)]
pub enum RegisterError {
    /// This key has already been registered by a participant
    #[error("This key has already been registered.")]
    KeyRegistered(VerificationKey),
    /// This participant has already been registered
    #[error("Participant {0} has already been registered.")]
    PartyRegistered(PartyId),
    /// The supplied participant id does not belong to the
    /// participant list
    #[error("Participant id {0} does not belong to the participants list.")]
    UnknownPartyId(PartyId),
    /// The supplied key is not valid
    #[error("The verification of correctness of the supplied key is invalid.")]
    InvalidKey(Box<VerificationKeyPoP>),
    /// Serialization error
    #[error("Serialization error")]
    SerializationError,
}

impl From<MultiSignatureError> for RegisterError {
    fn from(e: MultiSignatureError) -> Self {
        match e {
            MultiSignatureError::SerializationError => Self::SerializationError,
            MultiSignatureError::InvalidKey(k) => Self::InvalidKey(k),
            _ => todo!(),
        }
    }
}

impl From<RegisterError> for MultiSignatureError {
    fn from(_: RegisterError) -> Self {
        todo!()
    }
}

impl From<MerkleTreeError> for MultiSignatureError {
    fn from(_: MerkleTreeError) -> Self {
        todo!()
    }
}

impl<D: Digest + Clone + FixedOutput> From<MultiSignatureError> for MithrilWitnessError<D> {
    fn from(_: MultiSignatureError) -> Self {
        // todo:
        Self::StakeInvalid
    }
}

impl<D: Digest + Clone + FixedOutput> From<VerificationFailure<D>> for MithrilWitnessError<D> {
    fn from(_: VerificationFailure<D>) -> Self {
        // todo:
        Self::StakeInvalid
    }
}

// pub(crate) fn blst_err_to_atms(e: BLST_ERROR) -> Result<(), MultiSignatureError> {
//     match e {
//         BLST_ERROR::BLST_SUCCESS => Ok(()),
//         BLST_ERROR::BLST_VERIFY_FAIL => Err(MultiSignatureError::InvalidSignature),
//         BLST_ERROR::BLST_AGGR_TYPE_MISMATCH => Err(MultiSignatureError::UnexpectedBlstTypes),
//         BLST_ERROR::BLST_PK_IS_INFINITY => Err(MultiSignatureError::UnexpectedBlstTypes),
//         _ => Err(MultiSignatureError::SerializationError),
//     }
// }
