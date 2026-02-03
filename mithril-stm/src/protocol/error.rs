use crate::{RegistrationEntry, VerificationKeyForConcatenation};

#[cfg(feature = "future_snark")]
use crate::VerificationKeyForSnark;

/// Errors which can be outputted by key registration.
#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq)]
pub enum RegisterError {
    /// This key has already been registered by a participant
    #[error("This key has already been registered.")]
    EntryAlreadyRegistered(Box<RegistrationEntry>),

    /// Cannot register if the registration is closed.
    #[error("Cannot register if the registration is closed.")]
    RegistrationClosed,

    /// Registration is not closed. Cannot create a signer.
    #[error("Registration is not closed. Cannot create a signer")]
    RegistrationIsNotClosed,

    /// The supplied concatenation key is not valid
    #[error("The verification of correctness of the supplied concatenation key is invalid.")]
    ConcatenationKeyInvalid(Box<VerificationKeyForConcatenation>),

    #[cfg(feature = "future_snark")]
    /// The supplied snark key is not valid
    #[error("The verification of correctness of the supplied SNARK key is invalid.")]
    SnarkKeyInvalid(Box<VerificationKeyForSnark>),

    /// Serialization error
    #[error("Serialization error")]
    SerializationError,

    /// UnregisteredInitializer error
    #[error("Initializer not registered. Cannot participate as a signer.")]
    UnregisteredInitializer,

    /// No registration found for the given index.
    #[error("No registration found for the given index.")]
    UnregisteredIndex,

    #[cfg(feature = "future_snark")]
    /// Snark key is none
    #[error("The verification key for snark is undefined.")]
    SnarkKeyUndefined,

    #[cfg(feature = "future_snark")]
    /// Lottery target value is none
    #[error("The lottery target value is undefined.")]
    LotteryTargetValueUndefined,
}
