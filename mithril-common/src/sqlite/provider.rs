use anyhow::Context;
use sqlite::Connection;

use crate::StdResult;

use super::{EntityCursor, SqLiteEntity, WhereCondition};

/// A Provider is able to performe queries on a database and return iterator of a defined entity.
/// It aims at being easily testable and adaptable.
pub trait Provider<'conn> {
    /// Entity type returned by the result cursor.
    type Entity: SqLiteEntity;

    /// Share the connection.
    fn get_connection(&'conn self) -> &'conn Connection;

    /// Perform the parametrized definition query.
    fn find(&'conn self, filters: WhereCondition) -> StdResult<EntityCursor<'conn, Self::Entity>> {
        let (condition, params) = filters.expand();
        let sql = self.get_definition(&condition);
        let cursor = self
            .get_connection()
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

    /// Return the definition of this provider, ie the actual SQL query this
    /// provider performs.
    fn get_definition(&self, condition: &str) -> String;
}

#[cfg(test)]
mod tests {
    use sqlite::Value;

    use crate::sqlite::{Projection, SourceAlias};

    use super::super::{entity::HydrationError, SqLiteEntity};
    use super::*;

    #[derive(Debug, PartialEq)]
    struct TestEntity {
        text_data: String,
        real_data: f64,
        integer_data: i64,
        maybe_null: Option<i64>,
    }

    impl SqLiteEntity for TestEntity {
        fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError> {
            Ok(TestEntity {
                text_data: row.read::<&str, _>(0).to_string(),
                real_data: row.read::<f64, _>(1),
                integer_data: row.read::<i64, _>(2),
                maybe_null: row.read::<Option<i64>, _>(3),
            })
        }

        fn get_projection() -> Projection {
            let mut projection = Projection::default();

            projection.add_field("text_data", "{:test:}.text_data", "text");
            projection.add_field("real_data", "{:test:}.real_data", "real");
            projection.add_field("integer_data", "{:test:}.integer_data", "integer");
            projection.add_field("maybe_null", "{:test:}.maybe_null", "integer");

            projection
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

        fn get_connection(&'conn self) -> &'conn Connection {
            &self.connection
        }

        fn get_definition(&self, condition: &str) -> String {
            let aliases = SourceAlias::new(&[("{:test:}", "test")]);
            let projection = Self::Entity::get_projection().expand(aliases);

            format!("select {projection} from provider_test as test where {condition}")
        }
    }

    struct TestEntityUpdateProvider {
        connection: Connection,
    }

    impl TestEntityUpdateProvider {
        pub fn new(connection: Connection) -> Self {
            Self { connection }
        }
    }

    impl<'conn> Provider<'conn> for TestEntityUpdateProvider {
        type Entity = TestEntity;

        fn get_connection(&'conn self) -> &'conn Connection {
            &self.connection
        }

        fn get_definition(&self, _condition: &str) -> String {
            let aliases = SourceAlias::new(&[("{:test:}", "provider_test")]);
            let projection = Self::Entity::get_projection().expand(aliases);

            format!(
                r#"
insert into provider_test (text_data, real_data, integer_data, maybe_null) values (?1, ?2, ?3, ?4)
  on conflict (text_data) do update set
    real_data = excluded.real_data,
    integer_data = excluded.integer_data,
    maybe_null = excluded.maybe_null 
returning {projection}
"#
            )
        }
    }

    fn init_database() -> Connection {
        let connection = Connection::open(":memory:").unwrap();
        connection
            .execute(
                "
            drop table if exists provider_test;
            create table provider_test(text_data text not null primary key, real_data real not null, integer_data integer not null, maybe_null integer);
            insert into provider_test(text_data, real_data, integer_data, maybe_null) values ('row 1', 1.23, -52, null);
            insert into provider_test(text_data, real_data, integer_data, maybe_null) values ('row 2', 2.34, 1789, 0);
            ",
            )
            .unwrap();

        connection
    }

    #[test]
    pub fn simple_test() {
        let provider = TestEntityProvider::new(init_database());
        let mut cursor = provider.find(WhereCondition::default()).unwrap();
        let entity = cursor
            .next()
            .expect("there shoud be two results, none returned");
        assert_eq!(
            TestEntity {
                text_data: "row 1".to_string(),
                real_data: 1.23,
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
                real_data: 2.34,
                integer_data: 1789,
                maybe_null: Some(0)
            },
            entity
        );

        assert!(cursor.next().is_none(), "there should be no result");
    }

    #[test]
    pub fn test_condition() {
        let provider = TestEntityProvider::new(init_database());
        let mut cursor = provider
            .find(WhereCondition::new("maybe_null is not null", Vec::new()))
            .unwrap();
        let entity = cursor
            .next()
            .expect("there shoud be one result, none returned");
        assert_eq!(
            TestEntity {
                text_data: "row 2".to_string(),
                real_data: 2.34,
                integer_data: 1789,
                maybe_null: Some(0)
            },
            entity
        );
        assert!(cursor.next().is_none());
    }

    #[test]
    pub fn test_parameters() {
        let provider = TestEntityProvider::new(init_database());
        let mut cursor = provider
            .find(WhereCondition::new(
                "text_data like ?",
                vec![Value::String("%1".to_string())],
            ))
            .unwrap();
        let entity = cursor
            .next()
            .expect("there shoud be one result, none returned");
        assert_eq!(
            TestEntity {
                text_data: "row 1".to_string(),
                real_data: 1.23,
                integer_data: -52,
                maybe_null: None
            },
            entity
        );
        assert!(cursor.next().is_none());
    }
    #[test]
    fn test_upsertion() {
        let provider = TestEntityUpdateProvider::new(init_database());
        let params = [
            Value::String("row 1".to_string()),
            Value::Float(1.234),
            Value::Integer(0),
            Value::Null,
        ]
        .to_vec();
        let mut cursor = provider.find(WhereCondition::new("", params)).unwrap();

        let entity = cursor
            .next()
            .expect("there shoud be one result, none returned");
        assert_eq!(
            TestEntity {
                text_data: "row 1".to_string(),
                real_data: 1.234,
                integer_data: 0,
                maybe_null: None
            },
            entity
        );
        assert!(cursor.next().is_none());
    }
}
