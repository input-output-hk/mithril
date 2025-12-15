use blake2::{Blake2b, digest::consts::U32};
use mithril_stm::{
    AggregateSignature, Clerk, Initializer, KeyRegistration, Parameters, Signer, SingleSignature,
    Stake, VerificationKeyProofOfPossession,
};

// Protocol types alias
type D = Blake2b<U32>;

/// The id of a mithril party.
pub type ProtocolPartyId = String;

/// Alias of [MithrilStm:Stake](type@mithril_stm::Stake).
pub type ProtocolStake = Stake;

/// Alias of [MithrilStm::Parameters](struct@mithril_stm::Parameters).
pub type ProtocolParameters = Parameters;

/// Alias of [MithrilStm:Signer](struct@mithril_stm::Signer).
pub type ProtocolSigner = Signer<D>;

/// Alias of [MithrilStm:Clerk](struct@mithril_stm::Clerk).
pub type ProtocolClerk = Clerk<D>;

/// Alias of [MithrilStm:Initializer](struct@mithril_stm::Initializer).
pub type ProtocolInitializerNotCertified = Initializer;

/// Alias of [MithrilStm:KeyRegistration](struct@mithril_stm::KeyRegistration). (Test only)
pub type ProtocolKeyRegistrationNotCertified = KeyRegistration;

/// Alias of [MithrilStm:SingleSignature](struct@mithril_stm::SingleSignature).
pub type ProtocolSingleSignature = SingleSignature;

/// Alias of [MithrilStm:AggregateSignature](enum@mithril_stm::AggregateSignature).
pub type ProtocolMultiSignature = AggregateSignature<D>;

/// Alias of [MithrilStm:VerificationKeyProofOfPossession](type@mithril_stm::VerificationKeyProofOfPossession).
pub type ProtocolSignerVerificationKey = VerificationKeyProofOfPossession;
