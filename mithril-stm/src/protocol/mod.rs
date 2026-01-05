mod aggregate_signature;
mod eligibility_check;
mod error;
mod key_registration;
mod parameters;
mod participant;
mod single_signature;

pub use aggregate_signature::{
    AggregateSignature, AggregateSignatureError, AggregateSignatureType, AggregateVerificationKey,
    AggregationError, Clerk,
};
pub(crate) use eligibility_check::is_lottery_won;
pub use error::RegisterError;
pub use key_registration::{ClosedKeyRegistration, KeyRegistration, RegisteredParty};
pub use parameters::Parameters;
pub use participant::{Initializer, Signer, VerificationKey, VerificationKeyProofOfPossession};
pub use single_signature::{SignatureError, SingleSignature, SingleSignatureWithRegisteredParty};
