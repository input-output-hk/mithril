mod aggregate_signature;
mod eligibility_check;
mod error;
mod outdated_key_registration;
mod outdated_participant;
mod parameters;
mod single_signature;

pub use aggregate_signature::{
    AggregateSignature, AggregateSignatureError, AggregateSignatureType, AggregateVerificationKey,
    AggregationError, Clerk,
};
pub(crate) use eligibility_check::is_lottery_won;
pub use error::RegisterError;
pub use outdated_key_registration::{
    OutdatedClosedKeyRegistration, OutdatedKeyRegistration, RegisteredParty,
};
pub use outdated_participant::{
    OutdatedInitializer, OutdatedSigner, VerificationKey, VerificationKeyProofOfPossession,
};
pub use parameters::Parameters;
pub use single_signature::{SignatureError, SingleSignature, SingleSignatureWithRegisteredParty};

// New Key registration imports
mod key_registration;
mod participant;

pub use key_registration::{
    ClosedKeyRegistration, KeyRegistration, RegistrationEntry, RegistrationEntryForConcatenation,
};
pub use participant::{Initializer, Signer};
