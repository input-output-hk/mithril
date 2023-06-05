use mithril_stm::key_reg::KeyReg;
use mithril_stm::stm::{
    Stake, StmAggrSig, StmClerk, StmInitializer, StmParameters, StmSig, StmSigner,
    StmVerificationKeyPoP,
};

use blake2::{digest::consts::U32, Blake2b};

// Protocol types alias
type D = Blake2b<U32>;

/// The id of a mithril party.
pub type ProtocolPartyId = String;

/// Alias of [MithrilStm:Stake](type@mithril_stm::stm::Stake).
pub type ProtocolStake = Stake;

/// Alias of [MithrilStm::StmParameters](struct@mithril_stm::stm::StmParameters).
pub type ProtocolParameters = StmParameters;

/// Alias of [MithrilStm:StmSigner](struct@mithril_stm::stm::StmSigner).
pub type ProtocolSigner = StmSigner<D>;

/// Alias of [MithrilStm:StmClerk](struct@mithril_stm::stm::StmClerk).
pub type ProtocolClerk = StmClerk<D>;

/// Alias of [MithrilStm:StmInitializer](struct@mithril_stm::stm::StmInitializer).
pub type ProtocolInitializerNotCertified = StmInitializer;

/// Alias of [MithrilStm:KeyReg](struct@mithril_stm::key_reg::KeyReg). (Test only)
pub type ProtocolKeyRegistrationNotCertified = KeyReg;

/// Alias of [MithrilStm:StmSig](struct@mithril_stm::stm::StmSig).
pub type ProtocolSingleSignature = StmSig;

/// Alias of [MithrilStm:StmAggrSig](struct@mithril_stm::stm::StmAggrSig).
pub type ProtocolMultiSignature = StmAggrSig<D>;

/// Alias of [MithrilStm:StmVerificationKeyPoP](type@mithril_stm::stm::StmVerificationKeyPoP).
pub type ProtocolSignerVerificationKey = StmVerificationKeyPoP;
