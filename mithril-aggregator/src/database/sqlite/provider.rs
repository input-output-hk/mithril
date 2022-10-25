use std::error::Error;

use sqlite::{Connection, Value};

use super::EntityCursor;
pub trait Provider<'conn> {
    type Entity;

    fn find(
        &'conn self,
        condition: &str,
    ) -> Result<EntityCursor<'conn, Self::Entity>, Box<dyn Error>>;

    fn get_connection(&'conn self) -> &'conn Connection;

    fn query(
        &'conn self,
        sql: &str,
        params: &[Value],
    ) -> Result<EntityCursor<'conn, Self::Entity>, Box<dyn Error>> {
        let cursor = self
            .get_connection()
            .prepare(sql)?
            .into_cursor()
            .bind(params)?;
        let iterator = EntityCursor::new(cursor);

        Ok(iterator)
    }
}

#[cfg(test)]
mod tests {
    use crate::database::sqlite::Entity;

    use super::*;

    #[derive(Debug, PartialEq)]
    struct TestEntity {
        text_data: String,
        real_data: f64,
        integer_data: i64,
        maybe_null: Option<i64>,
    }

    impl Entity for TestEntity {
        fn hydrate(row: sqlite::Row) -> Self {
            TestEntity {
                text_data: row.get::<String, _>(0),
                real_data: row.get::<f64, _>(1),
                integer_data: row.get::<i64, _>(2),
                maybe_null: row.get::<Option<i64>, _>(3),
            }
        }
    }

    struct TestEntityProvider {
        connection: Connection,
    }

    impl TestEntityProvider {
        pub fn new(connection: Connection) -> Self {
            Self { connection }
        }
    }

    impl<'conn> Provider<'conn> for TestEntityProvider {
        type Entity = TestEntity;

        fn find(
            &'conn self,
            condition: &str,
        ) -> Result<EntityCursor<'conn, Self::Entity>, Box<dyn Error>> {
            let sql = "select text_data, real_data, integer_data, maybe_null from provider_test";

            self.query(sql, &[])
        }

        fn get_connection(&'conn self) -> &'conn Connection {
            &self.connection
        }
    }
    #[test]
    pub fn simple_test() {
        let connection = Connection::open(":memory:").unwrap();
        connection
            .execute(
                "
            drop table if exists provider_test;
            create table provider_test(text_data text not null, real_data real not null, integer_data integer not null, maybe_null integer);
            insert into provider_test(text_data, real_data, integer_data, maybe_null) values ('row 1', 3.14, -52, null);
            insert into provider_test(text_data, real_data, integer_data, maybe_null) values ('row 2', 2.72, 1789, 0);
            ",
            )
            .unwrap();
        let provider = TestEntityProvider::new(connection);
        let mut cursor = provider.find("whatever").unwrap();
        let entity = cursor
            .next()
            .expect("there shoud be two results, none returned");
        assert_eq!(
            TestEntity {
                text_data: "row 1".to_string(),
                real_data: 3.14,
                integer_data: -52,
                maybe_null: None
            },
            entity
        );
        let entity = cursor
            .next()
            .expect("there shoud be two results, only one returned");
        assert_eq!(
            TestEntity {
                text_data: "row 2".to_string(),
                real_data: 2.72,
                integer_data: 1789,
                maybe_null: Some(0)
            },
            entity
        );

        assert!(cursor.next().is_none());
    }
}
