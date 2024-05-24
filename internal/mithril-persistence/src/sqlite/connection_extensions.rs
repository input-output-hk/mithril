use anyhow::Context;
use sqlite::{ReadableWithIndex, Value};

use mithril_common::StdResult;

use crate::sqlite::{EntityCursor, ProviderV2, SqliteConnection};

/// Extension trait for the [SqliteConnection] type.
pub trait ConnectionExtensions {
    /// Execute the given sql query and return the value of the first cell read.
    fn query_single_cell<Q: AsRef<str>, T: ReadableWithIndex>(
        &self,
        sql: Q,
        params: &[Value],
    ) -> StdResult<T>;

    /// Fetch entities from the database using the given provider.
    fn fetch<P: ProviderV2>(&self, provider: P) -> StdResult<EntityCursor<P::Entity>>;
}

impl ConnectionExtensions for SqliteConnection {
    fn query_single_cell<Q: AsRef<str>, T: ReadableWithIndex>(
        &self,
        sql: Q,
        params: &[Value],
    ) -> StdResult<T> {
        let mut statement = self.prepare(&sql).with_context(|| {
            format!(
                "Prepare query error: SQL=`{}`",
                sql.as_ref().replace('\n', " ").trim()
            )
        })?;
        statement.bind(params)?;
        statement.next()?;
        statement
            .read::<T, _>(0)
            .with_context(|| "Read query error")
    }

    fn fetch<P: ProviderV2>(&self, provider: P) -> StdResult<EntityCursor<P::Entity>> {
        let (condition, params) = provider.filters().expand();
        let sql = provider.get_definition(&condition);
        let cursor = self
            .prepare(&sql)
            .with_context(|| {
                format!(
                    "Prepare query error: SQL=`{}`",
                    &sql.replace('\n', " ").trim()
                )
            })?
            .into_iter()
            .bind(&params[..])?;

        let iterator = EntityCursor::new(cursor);

        Ok(iterator)
    }
}

#[cfg(test)]
mod tests {
    use sqlite::Connection;

    use super::*;

    #[test]
    fn test_query_string() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let value: String = connection.query_single_cell("select 'test'", &[]).unwrap();

        assert_eq!(value, "test");
    }

    #[test]
    fn test_query_max_number() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let value: i64 = connection
            .query_single_cell(
                "select max(a) from (select 10 a union select 90 a union select 45 a)",
                &[],
            )
            .unwrap();

        assert_eq!(value, 90);
    }

    #[test]
    fn test_query_with_params() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let value: i64 = connection
            .query_single_cell(
                "select max(a) from (select 10 a union select 45 a union select 90 a) \
                where a > ? and a < ?",
                &[Value::Integer(10), Value::Integer(90)],
            )
            .unwrap();

        assert_eq!(value, 45);
    }
}
