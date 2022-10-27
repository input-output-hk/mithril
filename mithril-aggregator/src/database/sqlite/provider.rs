use std::error::Error;

use sqlite::{Connection, Value};

use super::{EntityCursor, Projection};

/// A Provider is able to performe queries on a database and return iterator of a defined entity.
/// It aims at being easily testable and adaptable.
pub trait Provider<'conn> {
    type Entity;

    fn get_connection(&'conn self) -> &'conn Connection;

    fn get_projection(&self) -> &dyn Projection;

    fn find(
        &'conn self,
        condition: Option<&str>,
        params: &[Value],
    ) -> Result<EntityCursor<'conn, Self::Entity>, Box<dyn Error>> {
        let sql = self.get_definition(condition);
        let cursor = self
            .get_connection()
            .prepare(sql)?
            .into_cursor()
            .bind(params)?;
        let iterator = EntityCursor::new(cursor);

        Ok(iterator)
    }

    fn get_definition(&self, condition: Option<&str>) -> String;
}

#[cfg(test)]
mod tests {
    use crate::database::sqlite::{Entity, ProjectionField};

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

    pub struct TestProjection {
        fields: Vec<ProjectionField>,
    }

    impl Projection for TestProjection {
        fn get_fields(&self) -> &Vec<ProjectionField> {
            &self.fields
        }

        fn set_field(&mut self, field: ProjectionField) {
            let _ = self.fields.push(field);
        }
    }

    impl TestProjection {
        pub fn new() -> Self {
            let mut projection = Self { fields: Vec::new() };

            projection.add_field("text_data", "{:test:}.text_data", "text");
            projection.add_field("real_data", "{:test:}.real_data", "real");
            projection.add_field("integer_data", "{:test:}.integer_data", "integer");
            projection.add_field("maybe_null", "{:test:}.maybe_null", "integer");

            projection
        }
    }

    struct TestEntityProvider {
        connection: Connection,
        projection: TestProjection,
    }

    impl TestEntityProvider {
        pub fn new(connection: Connection) -> Self {
            Self {
                connection,
                projection: TestProjection::new(),
            }
        }
    }

    impl<'conn> Provider<'conn> for TestEntityProvider {
        type Entity = TestEntity;

        fn get_connection(&'conn self) -> &'conn Connection {
            &self.connection
        }

        fn get_projection(&self) -> &dyn Projection {
            &self.projection
        }

        fn get_definition(&self, condition: Option<&str>) -> String {
            let where_clause = condition.unwrap_or("true");
            let aliases = [("{:test:}".to_string(), "test".to_string())]
                .into_iter()
                .collect();
            let projection = self.get_projection().expand(aliases);

            format!("select {projection} from provider_test as test where {where_clause}")
        }
    }

    struct TestEntityUpdateProvider {
        connection: Connection,
        projection: TestProjection,
    }

    impl TestEntityUpdateProvider {
        pub fn new(connection: Connection) -> Self {
            Self {
                connection,
                projection: TestProjection::new(),
            }
        }
    }

    impl<'conn> Provider<'conn> for TestEntityUpdateProvider {
        type Entity = TestEntity;

        fn get_connection(&'conn self) -> &'conn Connection {
            &self.connection
        }

        fn get_projection(&self) -> &dyn Projection {
            &self.projection
        }

        fn get_definition(&self, condition: Option<&str>) -> String {
            let where_clause = condition.unwrap_or("true");
            let aliases = [("{:test:}".to_string(), "provider_test".to_string())]
                .into_iter()
                .collect();
            let projection = self.get_projection().expand(aliases);

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
            insert into provider_test(text_data, real_data, integer_data, maybe_null) values ('row 1', 3.14, -52, null);
            insert into provider_test(text_data, real_data, integer_data, maybe_null) values ('row 2', 2.72, 1789, 0);
            ",
            )
            .unwrap();

        connection
    }

    #[test]
    pub fn simple_test() {
        let provider = TestEntityProvider::new(init_database());
        let mut cursor = provider.find(None, &[]).unwrap();
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

        assert!(cursor.next().is_none(), "there should be no result");
    }

    #[test]
    pub fn test_condition() {
        let provider = TestEntityProvider::new(init_database());
        let mut cursor = provider.find(Some("maybe_null is not null"), &[]).unwrap();
        let entity = cursor
            .next()
            .expect("there shoud be one result, none returned");
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

    #[test]
    pub fn test_parameters() {
        let provider = TestEntityProvider::new(init_database());
        let mut cursor = provider
            .find(Some("text_data like ?"), &[Value::String("%1".to_string())])
            .unwrap();
        let entity = cursor
            .next()
            .expect("there shoud be one result, none returned");
        assert_eq!(
            TestEntity {
                text_data: "row 1".to_string(),
                real_data: 3.14,
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
        ];
        let mut cursor = provider.find(None, &params).unwrap();

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
