mod dependency_container;
mod genesis;

pub use dependency_container::*;
pub use genesis::GenesisToolsDependency;

use std::sync::Arc;

use mithril_persistence::sqlite::SqliteConnection;

/// Dependency container for the database commands
pub struct DatabaseCommandDependencyContainer {
    /// Main database connection
    pub main_db_connection: Arc<SqliteConnection>,
}
