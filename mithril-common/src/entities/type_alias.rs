use std::collections::HashMap;

/// ImmutableFileNumber represents the id of immutable files in the Cardano node database
pub type ImmutableFileNumber = u64;

/// PartyId represents a signing party in Mithril protocol
pub type PartyId = String;

/// Stake represents the stakes of a participant in the Cardano chain
pub type Stake = u64;

/// StakeDistribution represents the stakes of multiple participants in the Cardano chain
pub type StakeDistribution = HashMap<PartyId, Stake>;

/// LotteryIndex represents the index of a Mithril single signature lottery
pub type LotteryIndex = u64;

/// Cardano Network magic identifier
pub type MagicId = u64;

/// Protocol version
pub type ProtocolVersion = String;
