use crate::crypto_helper::cardano::{
    KeyRegWrapper, ProtocolInitializerErrorWrapper, ProtocolRegistrationErrorWrapper,
    StmInitializerWrapper,
};

use mithril_stm::{
    key_reg::ClosedKeyReg,
    stm::{Index, Stake, StmClerk, StmParameters, StmSigner},
    AggregationError,
};

use blake2::{digest::consts::U32, Blake2b};

/// A protocol version
pub type ProtocolVersion<'a> = &'a str;

// Protocol types alias
pub(crate) type D = Blake2b<U32>;

/// The id of a mithril party.
pub type ProtocolPartyId = String;

/// Alias of [MithrilStm:Stake](type@mithril_stm::stm::Stake).
pub type ProtocolStake = Stake;

/// A list of [Party Id][ProtocolPartyId] associated with its [Stake][ProtocolStake].
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;

/// Alias of [MithrilStm::StmParameters](struct@mithril_stm::stm::StmParameters).
pub type ProtocolParameters = StmParameters;

/// Alias of [MithrilStm::Index](type@mithril_stm::stm::Index).
pub type ProtocolLotteryIndex = Index;

/// Alias of [MithrilStm:StmSigner](struct@mithril_stm::stm::StmSigner).
pub type ProtocolSigner = StmSigner<D>;

/// Alias of a wrapper of [MithrilStm:StmInitializer](struct@mithril_stm::stm::StmInitializer).
pub type ProtocolInitializer = StmInitializerWrapper;

/// Alias of [MithrilStm:StmClerk](struct@mithril_stm::stm::StmClerk).
pub type ProtocolClerk = StmClerk<D>;

/// Alias of a wrapper of [MithrilStm:KeyReg](struct@mithril_stm::key_reg::KeyReg).
pub type ProtocolKeyRegistration = KeyRegWrapper;

/// Alias of a wrapper of [MithrilStm:ClosedKeyReg](struct@mithril_stm::key_reg::KeyReg).
pub type ProtocolClosedKeyRegistration = ClosedKeyReg<D>;

// Error alias
/// Alias of a wrapper of [MithrilCommon:ProtocolRegistrationErrorWrapper](enum@ProtocolRegistrationErrorWrapper).
pub type ProtocolRegistrationError = ProtocolRegistrationErrorWrapper;

/// Alias of a wrapper of [MithrilCommon:ProtocolInitializerErrorWrapper](enum@ProtocolInitializerErrorWrapper).
pub type ProtocolInitializerError = ProtocolInitializerErrorWrapper;

/// Alias of [MithrilStm:AggregationError](enum@mithril_stm::AggregationError).
pub type ProtocolAggregationError = AggregationError;
