mod aggregate_signature;
mod eligibility_check;
mod error;
mod key_registration;
mod parameters;
mod participant;
mod single_signature;

#[cfg(feature = "basic_verifier")]
pub use aggregate_signature::BasicVerifier;
pub use aggregate_signature::{
    AggregateSignature, AggregateSignatureType, AggregateVerificationKey, Clerk,
};
pub(crate) use eligibility_check::is_lottery_won;
pub(crate) use error::blst_error_to_stm_error;
pub use error::{
    AggregateSignatureError, AggregationError, MerkleTreeError, MultiSignatureError, RegisterError,
    SignatureError,
};
pub use key_registration::{ClosedKeyRegistration, KeyRegistration, RegisteredParty};
pub use parameters::Parameters;
pub use participant::{Initializer, Signer, VerificationKey, VerificationKeyProofOfPossession};
pub use single_signature::{SingleSignature, SingleSignatureWithRegisteredParty};

// Aliases
#[deprecated(since = "0.5.0", note = "Use `AggregateSignature` instead")]
pub use aggregate_signature::AggregateSignature as StmAggrSig;

#[deprecated(since = "0.5.0", note = "Use `AggregateVerificationKey` instead")]
pub use aggregate_signature::AggregateVerificationKey as StmAggrVerificationKey;

#[deprecated(since = "0.5.0", note = "Use `Clerk` instead")]
pub use aggregate_signature::Clerk as StmClerk;

#[cfg(feature = "basic_verifier")]
#[deprecated(since = "0.5.0", note = "Use `BasicVerifier` instead")]
pub use aggregate_signature::BasicVerifier as CoreVerifier;

#[deprecated(since = "0.5.0", note = "Use `Parameters` instead")]
pub use parameters::Parameters as StmParameters;

#[deprecated(since = "0.5.0", note = "Use `ClosedKeyRegistration` instead")]
pub use key_registration::ClosedKeyRegistration as ClosedKeyReg;

#[deprecated(since = "0.5.0", note = "Use `KeyRegistration` instead")]
pub use key_registration::KeyRegistration as KeyReg;

#[deprecated(since = "0.5.0", note = "Use `SingleSignature` instead")]
pub use single_signature::SingleSignature as StmSig;

#[deprecated(
    since = "0.5.0",
    note = "Use `SingleSignatureWithRegisteredParty` instead"
)]
pub use single_signature::SingleSignatureWithRegisteredParty as StmSigRegParty;

#[deprecated(since = "0.5.0", note = "Use `Initializer` instead")]
pub use participant::Initializer as StmInitializer;

#[deprecated(since = "0.5.0", note = "Use `Signer` instead")]
pub use participant::Signer as StmSigner;

#[deprecated(since = "0.5.0", note = "Use `VerificationKey` instead")]
pub use participant::VerificationKey as StmVerificationKey;

#[deprecated(
    since = "0.5.0",
    note = "Use `VerificationKeyProofOfPossession` instead"
)]
pub use participant::VerificationKeyProofOfPossession as StmVerificationKeyPoP;
