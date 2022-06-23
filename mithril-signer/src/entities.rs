use cli_table::Table;
use serde::{Deserialize, Serialize};
use std::path::{Display, Path, PathBuf};

use mithril_common::entities::PartyId;

/// Client configuration
#[derive(Table, Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// Cardano network
    #[table(title = "Network")]
    pub network: String,

    /// Aggregator endpoint
    #[table(title = "Aggregator Endpoint")]
    pub aggregator_endpoint: String,

    /// Party Id
    #[table(title = "Party Id")]
    pub party_id: PartyId,

    /// Run Interval
    #[table(title = "Interval between two runtime cycles in ms")]
    pub run_interval: u64,

    /// Directory to snapshot
    #[table(
        title = "Path to the Cardano Node db directory",
        display_fn = "display_path"
    )]
    pub db_directory: PathBuf,

    /// Directory to store stakes
    #[table(
        title = "Path to the stake store directory",
        display_fn = "display_path"
    )]
    pub stake_store_directory: PathBuf,
}

fn display_path(path: &Path) -> Display<'_> {
    path.display()
}
