mod aggregate_signature;
mod eligibility_check;
mod error;

mod parameters;
mod single_signature;

pub use aggregate_signature::{
    AggregateSignature, AggregateSignatureError, AggregateSignatureType, AggregateVerificationKey,
    AggregationError, Clerk,
};
pub(crate) use eligibility_check::is_lottery_won;
pub use error::RegisterError;
pub use parameters::Parameters;
pub use single_signature::{SignatureError, SingleSignature, SingleSignatureWithRegisteredParty};

// New Key registration imports
mod key_registration;
mod participant;

pub use key_registration::{
    ClosedKeyRegistration, KeyRegistration, RegistrationEntry, RegistrationEntryForConcatenation,
    VerificationKeyForConcatenation, VerificationKeyProofOfPossessionForConcatenation,
};
pub use participant::{Initializer, Signer};
