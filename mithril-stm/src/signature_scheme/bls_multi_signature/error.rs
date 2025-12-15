//! Crate specific errors
use anyhow::anyhow;
use blst::BLST_ERROR;

use crate::StmResult;

use super::{BlsSignature, BlsVerificationKey, BlsVerificationKeyProofOfPossession};

/// Error types for multi signatures.
#[derive(Debug, thiserror::Error, Eq, PartialEq)]
pub enum BlsSignatureError {
    /// Invalid Single signature
    #[error("Invalid single signature")]
    SignatureInvalid(BlsSignature),

    /// Invalid aggregate signature
    #[error("Invalid aggregated signature")]
    AggregateSignatureInvalid,

    /// This error occurs when the serialization of the raw bytes failed
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

pub fn blst_error_to_stm_error(
    e: BLST_ERROR,
    sig: Option<BlsSignature>,
    key: Option<BlsVerificationKey>,
) -> StmResult<()> {
    match e {
        BLST_ERROR::BLST_SUCCESS => Ok(()),
        BLST_ERROR::BLST_PK_IS_INFINITY => {
            if let Some(s) = sig {
                return Err(anyhow!(BlsSignatureError::SignatureInfinity(s)));
            }
            if let Some(vk) = key {
                return Err(anyhow!(BlsSignatureError::VerificationKeyInfinity(
                    Box::new(vk)
                )));
            }
            Err(anyhow!(BlsSignatureError::SerializationError))
        }
        BLST_ERROR::BLST_VERIFY_FAIL => {
            if let Some(s) = sig {
                Err(anyhow!(BlsSignatureError::SignatureInvalid(s)))
            } else {
                Err(anyhow!(BlsSignatureError::AggregateSignatureInvalid))
            }
        }
        _ => Err(anyhow!(BlsSignatureError::SerializationError)),
    }
}
