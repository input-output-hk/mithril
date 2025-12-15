mod genesis;
mod serve;

use std::sync::Arc;

pub use genesis::GenesisCommandDependenciesContainer;
use mithril_persistence::sqlite::SqliteConnection;
pub use serve::*;

/// Dependencies container for the database commands
pub struct DatabaseCommandDependenciesContainer {
    /// Main database connection
    pub main_db_connection: Arc<SqliteConnection>,
}

/// Dependencies container for the tools commands
pub struct ToolsCommandDependenciesContainer {
    /// Database connection
    pub db_connection: Arc<SqliteConnection>,
}
