use crate::{RegistrationEntry, signature_scheme::BlsVerificationKey};

/// Errors which can be outputted by key registration.
#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq)]
pub enum RegisterError {
    /// This key has already been registered by a participant
    #[error("This key has already been registered.")]
    KeyRegistered(Box<BlsVerificationKey>),

    /// This entry has already been registered by a participant
    #[error("This entry has already been registered.")]
    EntryRegistered(Box<RegistrationEntry>),

    /// Cannot register if the registration is closed.
    #[error("Cannot register if the registration is closed.")]
    RegistrationClosed,

    /// Registration is not closed. Cannot create a signer.
    #[error("Registration is not closed. Cannot create a signer")]
    RegistrationIsNotClosed,

    /// The supplied key is not valid
    #[error("The verification of correctness of the supplied key is invalid.")]
    KeyInvalid(Box<BlsVerificationKey>),

    /// Serialization error
    #[error("Serialization error")]
    SerializationError,

    /// UnregisteredInitializer error
    #[error("Initializer not registered. Cannot participate as a signer.")]
    UnregisteredInitializer,

    /// No registration found for the given index.
    #[error("No registration found for the given index.")]
    UnregisteredIndex,
}
