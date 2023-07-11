use chrono::{DateTime, Utc};
use cli_table::{format::Justify, Table};
use serde::Serialize;

use mithril_common::{
    entities::Epoch,
    messages::{MithrilStakeDistributionListItemMessage, SnapshotListItemMessage},
};

/// SnapshotListItem represents a snapshot list item from an aggregator
/// for the purpose of tabular display
#[derive(Table, Debug, Clone, PartialEq, Eq, PartialOrd, Serialize)]
pub struct SnapshotListItem {
    /// Cardano epoch
    #[table(title = "Epoch")]
    pub epoch: Epoch,

    /// Cardano immutable file number
    #[table(title = "Immutable")]
    pub immutable_file_number: u64,

    /// Cardano Network name
    #[table(title = "Network")]
    pub network: String,

    /// Digest that is signed by the signer participants
    #[table(title = "Digest")]
    pub digest: String,

    /// Size of the snapshot file in Bytes
    #[table(title = "Size", justify = "Justify::Right")]
    pub size: u64,

    /// Number of locations where the binary content of the snapshot can be retrieved
    #[table(title = "Locations", justify = "Justify::Right")]
    pub total_locations: u16,

    /// Date and time at which the snapshot was created
    #[table(title = "Created", justify = "Justify::Right")]
    pub created_at: DateTime<Utc>,
}

impl From<SnapshotListItemMessage> for SnapshotListItem {
    fn from(value: SnapshotListItemMessage) -> Self {
        Self {
            epoch: value.beacon.epoch,
            immutable_file_number: value.beacon.immutable_file_number,
            network: value.beacon.network,
            digest: value.digest,
            size: value.size,
            total_locations: u16::try_from(value.locations.len()).unwrap(),
            created_at: value.created_at,
        }
    }
}

impl SnapshotListItem {
    /// SnapshotListItem factory
    pub fn new(
        epoch: Epoch,
        immutable_file_number: u64,
        network: String,
        digest: String,
        size: u64,
        total_locations: u16,
        created_at: DateTime<Utc>,
    ) -> SnapshotListItem {
        SnapshotListItem {
            epoch,
            immutable_file_number,
            network,
            digest,
            size,
            created_at,
            total_locations,
        }
    }
}

/// SnapshotFieldItem represents a field of a snapshot item from an aggregator
/// for the purpose of tabular display
#[derive(Table, Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct SnapshotFieldItem {
    /// Field name
    #[table(title = "Info")]
    pub field_name: String,

    /// Field value
    #[table(title = "Value")]
    pub field_value: String,
}

impl SnapshotFieldItem {
    /// SnapshotFieldItem factory
    pub fn new(field_name: String, field_value: String) -> SnapshotFieldItem {
        SnapshotFieldItem {
            field_name,
            field_value,
        }
    }
}

/// Item to display Mithril Stake Distribution lines in a table.
#[derive(Table, Debug, Clone, PartialEq, Eq, PartialOrd, Serialize)]
pub struct MithrilStakeDistributionListItem {
    #[table(title = "Epoch")]
    /// Epoch at which the Mithril Stake Distribution is created
    pub epoch: Epoch,

    #[table(title = "Hash")]
    /// Hash of the Mithril Stake Distribution (different from the AVK).
    pub hash: String,

    #[table(title = "Certificate Hash")]
    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// Date and time at which the Mithril Stake Distribution was created
    #[table(title = "Created", justify = "Justify::Right")]
    pub created_at: DateTime<Utc>,
}

impl From<MithrilStakeDistributionListItemMessage> for MithrilStakeDistributionListItem {
    fn from(value: MithrilStakeDistributionListItemMessage) -> Self {
        Self {
            epoch: value.epoch,
            hash: value.hash,
            certificate_hash: value.certificate_hash,
            created_at: value.created_at,
        }
    }
}
