use crate::crypto_helper::cardano::{
    KeyRegWrapper, ProtocolInitializerErrorWrapper, ProtocolRegistrationErrorWrapper,
    StmInitializerWrapper,
};

use mithril_stm::{
    AggregationError, ClosedKeyRegistration, Index, Parameters, Stake, StmClerk, StmSigner,
};

use blake2::{digest::consts::U32, Blake2b};

/// A protocol version
pub type ProtocolVersion<'a> = &'a str;

// Protocol types alias
pub(crate) type D = Blake2b<U32>;

/// The id of a mithril party.
pub type ProtocolPartyId = String;

/// Alias of [MithrilStm:Stake](type@mithril_stm::Stake).
pub type ProtocolStake = Stake;

/// A list of [Party Id][ProtocolPartyId] associated with its [Stake][ProtocolStake].
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;

/// Alias of [MithrilStm::StmParameters](struct@mithril_stm::StmParameters).
pub type ProtocolParameters = Parameters;

/// Alias of [MithrilStm::Index](type@mithril_stm::Index).
pub type ProtocolLotteryIndex = Index;

/// Alias of [MithrilStm:StmSigner](struct@mithril_stm::StmSigner).
pub type ProtocolSigner = StmSigner<D>;

/// Alias of a wrapper of [MithrilStm:StmInitializer](struct@mithril_stm::StmInitializer).
pub type ProtocolInitializer = StmInitializerWrapper;

/// Alias of [MithrilStm:StmClerk](struct@mithril_stm::StmClerk).
pub type ProtocolClerk = StmClerk<D>;

/// Alias of a wrapper of [MithrilStm:KeyReg](struct@mithril_stm::KeyReg).
pub type ProtocolKeyRegistration = KeyRegWrapper;

/// Alias of a wrapper of [MithrilStm:ClosedKeyReg](struct@mithril_stm::KeyReg).
pub type ProtocolClosedKeyRegistration = ClosedKeyRegistration<D>;

// Error alias
/// Alias of a wrapper of [MithrilCommon:ProtocolRegistrationErrorWrapper](enum@ProtocolRegistrationErrorWrapper).
pub type ProtocolRegistrationError = ProtocolRegistrationErrorWrapper;

/// Alias of a wrapper of [MithrilCommon:ProtocolInitializerErrorWrapper](enum@ProtocolInitializerErrorWrapper).
pub type ProtocolInitializerError = ProtocolInitializerErrorWrapper;

/// Alias of [MithrilStm:AggregationError](enum@mithril_stm::AggregationError).
pub type ProtocolAggregationError = AggregationError;
