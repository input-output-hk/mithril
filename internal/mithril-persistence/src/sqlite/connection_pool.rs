use std::{ops::Deref, sync::Arc, time::Duration};

use mithril_common::{
    resource_pool::{Reset, ResourcePool},
    StdResult,
};

use crate::sqlite::SqliteConnection;

/// SqliteConnection wrapper for a pooled connection
pub struct SqlitePooledConnection(Arc<SqliteConnection>);

impl SqlitePooledConnection {
    /// Create a new SqlitePooledConnection
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self(connection)
    }
}

impl Deref for SqlitePooledConnection {
    type Target = Arc<SqliteConnection>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Reset for SqlitePooledConnection {}

/// Pool of Sqlite connections
pub struct SqliteConnectionPool {
    connection_pool: ResourcePool<SqlitePooledConnection>,
}

impl SqliteConnectionPool {
    /// Create a new SqliteConnectionPool
    pub async fn new(connections: Vec<SqlitePooledConnection>) -> Self {
        let connection_pool = ResourcePool::new(connections.len(), connections);

        Self { connection_pool }
    }

    /// Get a connection from the pool
    pub fn connection(&self) -> StdResult<Arc<SqliteConnection>> {
        let timeout = Duration::from_millis(1000);
        let connection = self.connection_pool.acquire_resource(timeout)?;
        Ok((*connection).clone())
    }

    /// Returns a single resource pool connection
    pub fn from_connection(connection: Arc<SqliteConnection>) -> Self {
        let connection_pool = ResourcePool::new(1, vec![SqlitePooledConnection::new(connection)]);

        Self { connection_pool }
    }
}
