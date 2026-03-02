mod aggregate_signature;
mod error;
mod key_registration;
mod parameters;
mod participant;
mod single_signature;

pub use aggregate_signature::{
    AggregateSignature, AggregateSignatureError, AggregateSignatureType, AggregateVerificationKey,
    AggregationError, Clerk,
};
pub use error::RegisterError;
pub use key_registration::{
    ClosedKeyRegistration, ClosedRegistrationEntry, KeyRegistration, RegistrationEntry,
    RegistrationEntryForConcatenation,
};
pub use parameters::Parameters;
pub use participant::{Initializer, Signer};
pub use single_signature::{SignatureError, SingleSignature, SingleSignatureWithRegisteredParty};

#[cfg(feature = "future_snark")]
pub use key_registration::RegistrationEntryForSnark;

/// Wrapper of the Concatenation proof Verification key with proof of possession
pub type VerificationKeyProofOfPossessionForConcatenation =
    crate::signature_scheme::BlsVerificationKeyProofOfPossession;

/// Wrapper of the MultiSignature Verification key
pub type VerificationKeyForConcatenation = crate::signature_scheme::BlsVerificationKey;

#[cfg(feature = "future_snark")]
/// Wrapper of the Snark Verification key
pub type VerificationKeyForSnark = crate::signature_scheme::SchnorrVerificationKey;
