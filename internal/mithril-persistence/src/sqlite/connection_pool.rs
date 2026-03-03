use std::ops::Deref;
use std::sync::{Arc, RwLock};
use std::time::Duration;

use anyhow::anyhow;

use mithril_common::StdResult;
use mithril_resource_pool::{Reset, ResourcePool, ResourcePoolItem};

use crate::sqlite::{ConnectionBuilder, SqliteConnection};

/// SqliteConnection wrapper for a pooled connection
pub struct SqlitePooledConnection {
    connection: SqliteConnection,
    actual_version: u64,
    pool_version: Arc<RwLock<u64>>,
    builder: Arc<ConnectionBuilder>,
}

impl Reset for SqlitePooledConnection {
    fn reset(&mut self) -> StdResult<()> {
        let pool_version = self
            .pool_version
            .read()
            .map_err(|e| anyhow!(e.to_string()).context("Failed to acquire pool version lock"))?;
        if self.actual_version < *pool_version {
            self.connection = self.builder.build_without_migrations()?;
            self.actual_version = *pool_version;
        }

        Ok(())
    }
}

impl SqlitePooledConnection {
    /// Create a new SqlitePooledConnection
    fn new(
        connection: SqliteConnection,
        initial_version: u64,
        pool_version: Arc<RwLock<u64>>,
        builder: Arc<ConnectionBuilder>,
    ) -> Self {
        Self {
            connection,
            actual_version: initial_version,
            pool_version,
            builder,
        }
    }
}

impl Deref for SqlitePooledConnection {
    type Target = SqliteConnection;

    fn deref(&self) -> &Self::Target {
        &self.connection
    }
}

/// Pool of Sqlite connections
pub struct SqliteConnectionPool {
    connections: ResourcePool<SqlitePooledConnection>,
    pool_version: Arc<RwLock<u64>>,
}

impl SqliteConnectionPool {
    /// Create a new pool with the given size by calling the given builder function
    pub fn build(size: usize, builder: ConnectionBuilder) -> StdResult<Self> {
        let mut connections: Vec<SqlitePooledConnection> = Vec::with_capacity(size);
        let initial_version = 0;
        let pool_version = Arc::new(RwLock::new(initial_version));
        let builder = Arc::new(builder);

        for _count in 0..size {
            connections.push(SqlitePooledConnection::new(
                builder.build_without_migrations()?,
                initial_version,
                pool_version.clone(),
                builder.clone(),
            ));
        }

        Ok(Self {
            connections: ResourcePool::new(connections.len(), connections),
            pool_version,
        })
    }

    /// Get a connection from the pool
    pub fn connection(&self) -> StdResult<ResourcePoolItem<'_, SqlitePooledConnection>> {
        let timeout = Duration::from_millis(1000);
        let connection = self.connections.acquire_resource(timeout)?;

        Ok(connection)
    }

    /// Renew all connections in this pool
    ///
    /// Renewing a connection means that it will be closed and replaced with a new connection.
    ///
    /// - Idle connections are renewed immediately.
    /// - Pooled connections will be renewed when they are returned to the pool.
    pub fn renew_connections(&self) -> StdResult<()> {
        self.schedule_renew_for_all_connections()?;
        self.connections.reset_available_resources()?;

        Ok(())
    }

    fn schedule_renew_for_all_connections(&self) -> StdResult<()> {
        let mut pool_version = self.pool_version.write().map_err(|e| {
            anyhow!(e.to_string()).context("Failed to schedule connections renewal")
        })?;
        *pool_version += 1;

        Ok(())
    }

    #[cfg(test)]
    fn pool_version(&self) -> u64 {
        *self.pool_version.read().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use slog::{Drain, Logger};

    use mithril_common::temp_dir_create;
    use mithril_common::test::logging::MemoryDrainForTest;

    use crate::database::SqlMigration;

    use super::*;

    #[test]
    fn can_build_pool_of_given_size() {
        let pool = SqliteConnectionPool::build(10, ConnectionBuilder::open_memory()).unwrap();

        assert_eq!(pool.connections.size(), 10);
    }

    #[test]
    fn pooled_connection_release_connection_when_drop() {
        let connection_pool =
            SqliteConnectionPool::build(1, ConnectionBuilder::open_memory()).unwrap();

        {
            let _connection = connection_pool.connection().unwrap();
            assert_eq!(0, connection_pool.connections.count().unwrap());
        }

        assert_eq!(1, connection_pool.connections.count().unwrap());
    }

    #[test]
    fn renew_connections_rebuilds_pooled_connections_when_they_returns_to_pool() {
        let db_path = temp_dir_create!().join("test.db");
        let pool = Arc::new(
            SqliteConnectionPool::build(2, ConnectionBuilder::open_file(&db_path)).unwrap(),
        );

        // One connection that will be hold off so it won't be renewed
        let first_connection = pool.connection().unwrap();
        {
            // This connection will be renewed when going out of scope
            let second_connection = pool.connection().unwrap();
            assert_eq!(0, pool.pool_version());
            assert_eq!(0, first_connection.actual_version);
            assert_eq!(0, second_connection.actual_version);

            pool.renew_connections().unwrap();

            assert_eq!(1, pool.pool_version());
            assert_eq!(0, first_connection.actual_version);
            assert_eq!(0, second_connection.actual_version);
        }

        // The pools have two connections, this re-acquires the second connection, and it should have been renewed
        let second_connection = pool.connection().unwrap();
        assert_eq!(1, pool.pool_version());
        assert_eq!(0, first_connection.actual_version);
        assert_eq!(1, second_connection.actual_version);
    }

    #[test]
    fn renew_connections_immediately_renew_idle_connections() {
        let db_path = temp_dir_create!().join("test.db");
        let pool = Arc::new(
            SqliteConnectionPool::build(2, ConnectionBuilder::open_file(&db_path)).unwrap(),
        );

        let connection_that_should_not_be_renewed = pool.connection().unwrap();
        pool.renew_connections().unwrap();
        let connection_that_should_have_be_renewed = pool.connection().unwrap();

        assert_eq!(1, pool.pool_version());
        assert_eq!(0, connection_that_should_not_be_renewed.actual_version);
        assert_eq!(1, connection_that_should_have_be_renewed.actual_version);
    }

    #[test]
    fn multiple_renew_connections_increments_version() {
        let pool = SqliteConnectionPool::build(1, ConnectionBuilder::open_memory()).unwrap();
        assert_eq!(0, pool.pool_version());

        pool.renew_connections().unwrap();
        assert_eq!(1, pool.pool_version());

        pool.renew_connections().unwrap();
        assert_eq!(2, pool.pool_version());

        pool.renew_connections().unwrap();
        assert_eq!(3, pool.pool_version());
    }

    #[test]
    fn do_not_apply_migrations_when_pooling_a_connection() {
        let temp_dir = temp_dir_create!();
        let (memory_drain, inspector) = MemoryDrainForTest::new();
        let logger = Logger::root(memory_drain.fuse(), slog::o!());
        let builder = ConnectionBuilder::open_file(&temp_dir.join("db.sqlite"))
            .with_logger(logger)
            .with_migrations(vec![SqlMigration::new(1, "")]);
        let pool = SqliteConnectionPool::build(10, builder).unwrap();

        pool.connection().unwrap();
        pool.connection().unwrap();

        let number_of_times_migrations_run =
            inspector.search_logs(ConnectionBuilder::APPLY_MIGRATIONS_LOG).len();
        assert_eq!(number_of_times_migrations_run, 0);
    }
}
