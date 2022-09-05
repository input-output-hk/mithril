use cli_table::{format::Justify, Table};
use mithril_common::entities::Epoch;
use serde::{Deserialize, Serialize};

/// Client configuration
#[derive(Table, Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// Cardano network
    #[table(title = "Network")]
    pub network: String,

    /// Aggregator endpoint
    #[table(title = "Aggregator Endpoint")]
    pub aggregator_endpoint: String,
}

/// SnapshotListItem represents a snapshot list item from an aggregator
/// for the purpose of tabular display
#[derive(Table, Debug, Clone, PartialEq, Eq, PartialOrd, Serialize)]
pub struct SnapshotListItem {
    /// Cardano network
    #[table(title = "Network")]
    pub network: String,

    /// Cardano epoch
    #[table(title = "Epoch")]
    pub epoch: Epoch,

    /// Cardano immutable file number
    #[table(title = "Immutable")]
    pub immutable_file_number: u64,

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
    pub created_at: String,
}

impl SnapshotListItem {
    /// SnapshotListItem factory
    pub fn new(
        network: String,
        epoch: Epoch,
        immutable_file_number: u64,
        digest: String,
        size: u64,
        total_locations: u16,
        created_at: String,
    ) -> SnapshotListItem {
        SnapshotListItem {
            network,
            epoch,
            immutable_file_number,
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
