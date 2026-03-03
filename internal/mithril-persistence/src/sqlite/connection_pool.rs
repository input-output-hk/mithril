use std::ops::Deref;
use std::time::Duration;

use mithril_common::StdResult;
use mithril_resource_pool::{Reset, ResourcePool, ResourcePoolItem};

use crate::sqlite::{ConnectionBuilder, SqliteConnection};

/// SqliteConnection wrapper for a pooled connection
pub struct SqlitePooledConnection<'a> {
    _token: ResourcePoolItem<'a, SqliteConnectionToken>,
    connection: SqliteConnection,
}

struct SqliteConnectionToken;

impl Reset for SqliteConnectionToken {}

impl<'a> SqlitePooledConnection<'a> {
    /// Create a new SqlitePooledConnection
    fn new(
        token: ResourcePoolItem<'a, SqliteConnectionToken>,
        connection: SqliteConnection,
    ) -> Self {
        Self {
            _token: token,
            connection,
        }
    }
}

impl<'a> Deref for SqlitePooledConnection<'a> {
    type Target = SqliteConnection;

    fn deref(&self) -> &Self::Target {
        &self.connection
    }
}

/// Pool of Sqlite connections
pub struct SqliteConnectionPool {
    tokens_pool: ResourcePool<SqliteConnectionToken>,
    builder: ConnectionBuilder,
}

impl SqliteConnectionPool {
    /// Create a new pool with the given size by calling the given builder function
    pub fn build(size: usize, builder: ConnectionBuilder) -> StdResult<Self> {
        let mut tokens: Vec<SqliteConnectionToken> = Vec::with_capacity(size);
        for _count in 0..size {
            tokens.push(SqliteConnectionToken);
        }

        Ok(Self {
            tokens_pool: ResourcePool::new(tokens.len(), tokens),
            builder,
        })
    }

    /// Get a connection from the pool
    pub fn connection(&self) -> StdResult<SqlitePooledConnection<'_>> {
        let timeout = Duration::from_millis(1000);
        let token = self.tokens_pool.acquire_resource(timeout)?;

        let connection = self.builder.build_without_migrations()?;
        Ok(SqlitePooledConnection::new(token, connection))
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

        assert_eq!(pool.tokens_pool.size(), 10);
    }

    #[test]
    fn pooled_connection_release_token_when_drop() {
        let resource_pool = ResourcePool::new(1, vec![SqliteConnectionToken]);

        {
            let token = resource_pool.acquire_resource(Duration::from_secs(1)).unwrap();
            let _pool_item = SqlitePooledConnection::new(
                token,
                ConnectionBuilder::open_memory().build().unwrap(),
            );
            assert_eq!(0, resource_pool.count().unwrap());
        }

        assert_eq!(1, resource_pool.count().unwrap());
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
