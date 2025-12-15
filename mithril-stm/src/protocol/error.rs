use crate::signature_scheme::{BlsVerificationKey, BlsVerificationKeyProofOfPossession};

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
