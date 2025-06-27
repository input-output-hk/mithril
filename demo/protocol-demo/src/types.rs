use mithril_stm::{
    KeyReg, Stake, StmAggrSig, StmClerk, StmInitializer, StmParameters, StmSig, StmSigner,
    StmVerificationKeyPoP,
};

use blake2::{digest::consts::U32, Blake2b};

// Protocol types alias
type D = Blake2b<U32>;

/// The id of a mithril party.
pub type ProtocolPartyId = String;

/// Alias of [MithrilStm:Stake](type@mithril_stm::Stake).
pub type ProtocolStake = Stake;

/// Alias of [MithrilStm::StmParameters](struct@mithril_stm::StmParameters).
pub type ProtocolParameters = StmParameters;

/// Alias of [MithrilStm:StmSigner](struct@mithril_stm::StmSigner).
pub type ProtocolSigner = StmSigner<D>;

/// Alias of [MithrilStm:StmClerk](struct@mithril_stm::StmClerk).
pub type ProtocolClerk = StmClerk<D>;

/// Alias of [MithrilStm:StmInitializer](struct@mithril_stm::StmInitializer).
pub type ProtocolInitializerNotCertified = StmInitializer;

/// Alias of [MithrilStm:KeyReg](struct@mithril_stm::KeyReg). (Test only)
pub type ProtocolKeyRegistrationNotCertified = KeyReg;

/// Alias of [MithrilStm:StmSig](struct@mithril_stm::StmSig).
pub type ProtocolSingleSignature = StmSig;

/// Alias of [MithrilStm:StmAggrSig](struct@mithril_stm::StmAggrSig).
pub type ProtocolMultiSignature = StmAggrSig<D>;

/// Alias of [MithrilStm:StmVerificationKeyPoP](type@mithril_stm::StmVerificationKeyPoP).
pub type ProtocolSignerVerificationKey = StmVerificationKeyPoP;
