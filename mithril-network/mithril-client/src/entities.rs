use cli_table::{format::Justify, Table};
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
#[derive(Table, Debug, Clone, PartialEq, PartialOrd)]
pub struct SnapshotListItem {
    /// Cardano network
    #[table(title = "Network")]
    pub network: String,

    /// Digest that is signed by the signer participants
    #[table(title = "Digest")]
    pub digest: String,

    /// Whether the binary content of the snapshot is downloaded
    #[table(title = "Downloaded")]
    pub downloaded: bool,

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
        digest: String,
        downloaded: bool,
        size: u64,
        total_locations: u16,
        created_at: String,
    ) -> SnapshotListItem {
        SnapshotListItem {
            network,
            digest,
            downloaded,
            size,
            created_at,
            total_locations,
        }
    }
}
