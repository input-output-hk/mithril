mod dependencies_container;
mod genesis;

pub use dependencies_container::*;
pub use genesis::GenesisToolsDependency;

use std::sync::Arc;

use mithril_persistence::sqlite::SqliteConnection;

/// Dependency container for the database commands
pub struct DatabaseCommandDependencyContainer {
    /// Main database connection
    pub main_db_connection: Arc<SqliteConnection>,
}

/// Dependency container for the tools commands
pub struct ToolsCommandDependenciesContainer {
    /// Database connection
    pub db_connection: Arc<SqliteConnection>,
}
