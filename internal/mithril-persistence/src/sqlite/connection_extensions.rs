use anyhow::Context;
use sqlite::{ReadableWithIndex, Value};

use mithril_common::StdResult;

use crate::sqlite::{EntityCursor, Query, SqliteConnection, Transaction};

/// Extension trait for the [SqliteConnection] type.
pub trait ConnectionExtensions {
    /// Begin a transaction on the connection.
    fn begin_transaction(&self) -> StdResult<Transaction>;

    /// Execute the given sql query and return the value of the first cell read.
    fn query_single_cell<Q: AsRef<str>, T: ReadableWithIndex>(
        &self,
        sql: Q,
        params: &[Value],
    ) -> StdResult<T>;

    /// Fetch entities from the database using the given query.
    fn fetch<Q: Query>(&self, query: Q) -> StdResult<EntityCursor<Q::Entity>>;

    /// Fetch the first entity from the database returned using the given query.
    fn fetch_first<Q: Query>(&self, query: Q) -> StdResult<Option<Q::Entity>> {
        let mut cursor = self.fetch(query)?;
        Ok(cursor.next())
    }

    /// Fetch entities from the database using the given query and collect the result in a collection.
    fn fetch_collect<Q: Query, B: FromIterator<Q::Entity>>(&self, query: Q) -> StdResult<B> {
        Ok(self.fetch(query)?.collect::<B>())
    }

    /// Apply a query that do not return data from the database(ie: insert, delete, ...).
    fn apply<Q: Query>(&self, query: Q) -> StdResult<()> {
        self.fetch(query)?.count();
        Ok(())
    }
}

impl ConnectionExtensions for SqliteConnection {
    fn begin_transaction(&self) -> StdResult<Transaction> {
        Ok(Transaction::begin(self)?)
    }

    fn query_single_cell<Q: AsRef<str>, T: ReadableWithIndex>(
        &self,
        sql: Q,
        params: &[Value],
    ) -> StdResult<T> {
        let mut statement = prepare_statement(self, sql.as_ref())?;
        statement.bind(params)?;
        statement.next()?;
        statement.read::<T, _>(0).with_context(|| "Read query error")
    }

    fn fetch<Q: Query>(&self, query: Q) -> StdResult<EntityCursor<Q::Entity>> {
        let (condition, params) = query.filters().expand();
        let sql = query.get_definition(&condition);
        let cursor = prepare_statement(self, &sql)?.into_iter().bind(&params[..])?;

        let iterator = EntityCursor::new(cursor);

        Ok(iterator)
    }
}

fn prepare_statement<'conn>(
    sqlite_connection: &'conn SqliteConnection,
    sql: &str,
) -> StdResult<sqlite::Statement<'conn>> {
    sqlite_connection.prepare(sql).with_context(|| {
        format!(
            "Prepare query error: SQL=`{}`",
            &sql.replace('\n', " ").trim()
        )
    })
}

#[cfg(test)]
mod tests {
    use sqlite::Connection;

    use crate::sqlite::{HydrationError, SqLiteEntity, WhereCondition};

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

    #[test]
    fn test_apply_execute_the_query() {
        struct DummySqLiteEntity {}
        impl SqLiteEntity for DummySqLiteEntity {
            fn hydrate(_row: sqlite::Row) -> Result<Self, HydrationError>
            where
                Self: Sized,
            {
                unimplemented!()
            }

            fn get_projection() -> crate::sqlite::Projection {
                unimplemented!()
            }
        }

        struct FakeQuery {
            sql: String,
        }
        impl Query for FakeQuery {
            type Entity = DummySqLiteEntity;

            fn filters(&self) -> WhereCondition {
                WhereCondition::default()
            }

            fn get_definition(&self, _condition: &str) -> String {
                self.sql.clone()
            }
        }

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        connection.execute("create table query_test(text_data);").unwrap();

        let value: i64 = connection
            .query_single_cell("select count(*) from query_test", &[])
            .unwrap();
        assert_eq!(value, 0);

        let query = FakeQuery {
            sql: "insert into query_test(text_data) values ('row 1')".to_string(),
        };
        connection.apply(query).unwrap();

        let value: i64 = connection
            .query_single_cell("select count(*) from query_test", &[])
            .unwrap();
        assert_eq!(value, 1);
    }
}
