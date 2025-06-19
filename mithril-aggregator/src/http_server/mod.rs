pub mod routes;
pub mod validators;

pub const SERVER_BASE_PATH: &str = "aggregator";
pub const CARDANO_DATABASE_DOWNLOAD_PATH: &str = "cardano-database-download";
pub const SNAPSHOT_DOWNLOAD_PATH: &str = "snapshot_download";

/// Location of the folder containing the OpenApi specification.
#[cfg(test)]
const API_SPEC_LOCATION: &str = "..";
