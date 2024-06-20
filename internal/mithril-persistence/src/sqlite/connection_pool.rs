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

    /// Create a new pool with the given size by calling the given builder function
    pub fn build(
        size: usize,
        builder: impl Fn() -> StdResult<SqliteConnection>,
    ) -> StdResult<Self> {
        let mut connections: Vec<SqlitePooledConnection> = Vec::with_capacity(size);

        for _count in 0..size {
            connections.push(SqlitePooledConnection::new(Arc::new(builder()?)));
        }

        Ok(Self {
            connection_pool: ResourcePool::new(connections.len(), connections),
        })
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::database::test_helper::cardano_tx_db_connection;

    #[test]
    fn can_build_pool_of_given_size() {
        let pool = SqliteConnectionPool::build(10, cardano_tx_db_connection).unwrap();

        assert_eq!(pool.connection_pool.size(), 10);
    }
}
