mod routes;
mod server;

pub use server::{Server, SERVER_BASE_PATH};

/// Mithril API protocol version
/// this is the same as the one in openapi.yml file.
/// If you want to update this version to reflect changes in the protocol,
/// please also update the entry in the openapi.yml
pub const MITHRIL_API_VERSION: &str = "0.0.1";
