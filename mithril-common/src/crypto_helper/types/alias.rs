use crate::crypto_helper::cardano::{
    KeyRegWrapper, ProtocolInitializerErrorWrapper, ProtocolRegistrationErrorWrapper,
    StmInitializerWrapper,
};

use mithril_stm::{
    AggregationError, Clerk, OutdatedClosedKeyRegistration, Index, MithrilMembershipDigest, OutdatedSigner,
    Parameters, Stake,
};

/// A protocol version
pub type ProtocolVersion<'a> = &'a str;

// Protocol types alias
pub(crate) type D = MithrilMembershipDigest;

/// The id of a mithril party.
pub type ProtocolPartyId = String;

/// Alias of [MithrilStm:Stake](type@mithril_stm::Stake).
pub type ProtocolStake = Stake;

/// A list of [Party Id][ProtocolPartyId] associated with its [Stake][ProtocolStake].
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;

/// Alias of [MithrilStm::Parameters](struct@mithril_stm::Parameters).
pub type ProtocolParameters = Parameters;

/// Alias of [MithrilStm::Index](type@mithril_stm::Index).
pub type ProtocolLotteryIndex = Index;

/// Alias of [MithrilStm:Signer](struct@mithril_stm::Signer).
pub type ProtocolSigner = OutdatedSigner<D>;

/// Alias of a wrapper of [MithrilStm:Initializer](struct@mithril_stm::Initializer).
pub type ProtocolInitializer = StmInitializerWrapper;

/// Alias of [MithrilStm:Clerk](struct@mithril_stm::Clerk).
pub type ProtocolClerk = Clerk<D>;

/// Alias of a wrapper of [MithrilStm:KeyRegistration](struct@mithril_stm::KeyRegistration).
pub type ProtocolKeyRegistration = KeyRegWrapper;

/// Alias of a wrapper of [MithrilStm:ClosedKeyRegistration](struct@mithril_stm::ClosedKeyRegistration).
pub type ProtocolClosedKeyRegistration = OutdatedClosedKeyRegistration<D>;

// Error alias
/// Alias of a wrapper of [MithrilCommon:ProtocolRegistrationErrorWrapper](enum@ProtocolRegistrationErrorWrapper).
pub type ProtocolRegistrationError = ProtocolRegistrationErrorWrapper;

/// Alias of a wrapper of [MithrilCommon:ProtocolInitializerErrorWrapper](enum@ProtocolInitializerErrorWrapper).
pub type ProtocolInitializerError = ProtocolInitializerErrorWrapper;

/// Alias of [MithrilStm:AggregationError](enum@mithril_stm::AggregationError).
pub type ProtocolAggregationError = AggregationError;
