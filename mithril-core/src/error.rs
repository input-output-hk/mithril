use blst::BLST_ERROR;
use crate::msp::MspPk;
use crate::stm::PartyId;

#[derive(Debug, thiserror::Error, Eq, PartialEq)]
pub enum MultiSignatureError {
    #[error("Invalid multi signature")]
    InvalidSignature,
    #[error("Unexpected point")]
    UnexpectedBlstTypes,
    #[error("Invalid bytes")]
    SerializationError,
    #[error("Key with invalid PoP")]
    InvalidKey(Box<MspPk>)
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
    SerializationError
}

impl From<MultiSignatureError> for RegisterError {
    fn from(e: MultiSignatureError) -> Self {
        match e {
            MultiSignatureError::SerializationError => Self::SerializationError,
            MultiSignatureError::InvalidKey(k) => Self::InvalidKey(k),
            _ => todo!()
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