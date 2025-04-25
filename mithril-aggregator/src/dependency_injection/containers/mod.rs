mod dependencies_container;
mod genesis;

pub use dependencies_container::*;
pub use genesis::GenesisToolsDependency;

use std::sync::Arc;

use mithril_persistence::sqlite::SqliteConnection;

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
