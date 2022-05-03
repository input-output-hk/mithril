//! Crate specific errors

use crate::merkle_tree::Path;
use crate::mithril_proof::MithrilProof;
use crate::msp::{MspPk, MspSig};
use crate::stm::PartyId;
use blst::BLST_ERROR;

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
    InvalidKey(Box<MspPk>),
}

/// Error types for aggregation.
#[derive(Debug, Clone, thiserror::Error)]
pub enum AggregationFailure {
    /// Not enough signatures were collected, got this many instead.
    #[error("Not enough signatures. Got only {0} out of {1}.")]
    NotEnoughSignatures(u64, u64),
}

/// Error types for single signature verification
#[derive(Debug, Clone, thiserror::Error)]
pub enum VerificationFailure<F> {
    /// The lottery was actually lost for the signature
    #[error("Lottery for this epoch was lost.")]
    LotteryLost,
    /// The Merkle Tree is invalid
    #[error("The path of the Merkle Tree is invalid.")]
    InvalidMerkleTree(Path<F>),
    /// The MSP signature is invalid
    #[error("Invalid Signature.")]
    InvalidSignature(MspSig),
}

/// Error types for multisignature verification
#[derive(Debug, Clone, Copy, thiserror::Error)]
pub enum MultiVerificationFailure<Proof>
where
    Proof: MithrilProof,
{
    /// The underlying MSP aggregate is invalid
    #[error("The underlying MSP aggregate is invalid.")]
    InvalidAggregate(MspSig),
    /// Error wrapper for underlying proof system.
    #[error("Proof of validity failed. {0}")]
    ProofError(Proof::Error),
}

/// Errors which can be outputted by key registration.
#[derive(Debug, Clone, thiserror::Error)]
pub enum RegisterError {
    /// This key has already been registered by a participant
    #[error("This key has already been registered.")]
    KeyRegistered([u8; 96]),
    /// This participant has already been registered
    #[error("Participant {0} has already been registered.")]
    PartyRegistered(PartyId),
    /// The supplied participant id does not belong to the
    /// participant list
    #[error("Participant id {0} does not belong to the participants list.")]
    UnknownPartyId(PartyId),
    /// The supplied key is not valid
    #[error("The verification of correctness of the supplied key is invalid.")]
    InvalidKey(Box<MspPk>),
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

pub(crate) fn blst_err_to_atms(e: BLST_ERROR) -> Result<(), MultiSignatureError> {
    match e {
        BLST_ERROR::BLST_SUCCESS => Ok(()),
        BLST_ERROR::BLST_VERIFY_FAIL => Err(MultiSignatureError::InvalidSignature),
        BLST_ERROR::BLST_AGGR_TYPE_MISMATCH => Err(MultiSignatureError::UnexpectedBlstTypes),
        BLST_ERROR::BLST_PK_IS_INFINITY => Err(MultiSignatureError::UnexpectedBlstTypes),
        _ => Err(MultiSignatureError::SerializationError),
    }
}
