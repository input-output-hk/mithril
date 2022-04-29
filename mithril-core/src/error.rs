use blst::BLST_ERROR;

#[derive(Debug, thiserror::Error, Eq, PartialEq)]
pub enum MithrilCoreError {
    #[error("Invalid multi signature")]
    InvalidSignature,
    #[error("Unexpected point")]
    UnexpectedBlstTypes,
    #[error("Invalid bytes")]
    SerializationError,
    #[error("Key with invalid PoP")]
    InvalidKey
}

pub(crate) fn blst_err_to_atms(e: BLST_ERROR) -> Result<(), MithrilCoreError> {
    match e {
        BLST_ERROR::BLST_SUCCESS => Ok(()),
        BLST_ERROR::BLST_VERIFY_FAIL => Err(MithrilCoreError::InvalidSignature),
        BLST_ERROR::BLST_AGGR_TYPE_MISMATCH => Err(MithrilCoreError::UnexpectedBlstTypes),
        BLST_ERROR::BLST_PK_IS_INFINITY => Err(MithrilCoreError::UnexpectedBlstTypes),
        _ => Err(MithrilCoreError::SerializationError),
    }
}