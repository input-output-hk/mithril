use cli_table::Table;
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

    /// Party Id
    #[table(title = "Party Id")]
    pub party_id: u64,

    /// Run Interval
    #[table(title = "Interval between two signatures attempts")]
    pub run_interval: u64,
}
