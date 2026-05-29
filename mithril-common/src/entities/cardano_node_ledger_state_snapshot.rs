use semver::Version;
use serde::{Deserialize, Serialize};

use crate::entities::CardanoDbBeacon;

use super::CardanoNetwork;

/// Cardano node ledger state snapshot.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoNodeLedgerStateSnapshot {
    /// Unique hash of the Cardano node ledger state snapshot.
    pub hash: String,

    /// Cardano network.
    pub network: CardanoNetwork,

    /// Mithril beacon on the Cardano chain.
    pub beacon: CardanoDbBeacon,

    /// Version of the Cardano node used to create the snapshot.
    pub cardano_node_version: String,
}

impl CardanoNodeLedgerStateSnapshot {
    /// Creates a new [CardanoNodeLedgerStateSnapshot]
    pub fn new(
        hash: String,
        network: CardanoNetwork,
        beacon: CardanoDbBeacon,
        cardano_node_version: &Version,
    ) -> Self {
        Self {
            hash,
            network,
            beacon,
            cardano_node_version: format!("{cardano_node_version}"),
        }
    }
}
