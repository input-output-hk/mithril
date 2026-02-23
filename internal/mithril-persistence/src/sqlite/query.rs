use super::{MutationPayload, SqLiteEntity, WhereCondition};

/// Define a query to perform on database and return iterator of a defined entity.
///
/// Used as a parameter of [fetch][crate::sqlite::ConnectionExtensions::fetch].
///
/// It aims at being easily testable and adaptable.
pub trait Query {
    /// Entity type returned by the result cursor.
    type Entity: SqLiteEntity;

    /// Return the filters to apply to the query.
    fn filters(&self) -> WhereCondition;

    /// Returns the mutation payload for INSERT/UPDATE operations.
    fn mutation_payload(&self) -> Option<&dyn MutationPayload> {
        // Blanket implementation which returns None for read-only queries.
        None
    }

    /// Return the definition of this query, ie the actual SQL this query performs.
    fn get_definition(&self, condition: &str) -> String;
}

#[cfg(test)]
mod tests {
    use sqlite::{Connection, Value};

    use crate::sqlite::{
        ConnectionExtensions, FixedColumnPayload, GetAllCondition, Projection, SourceAlias,
        SqliteConnection,
    };

    use super::super::{SqLiteEntity, entity::HydrationError};
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

    struct GetTestEntityQuery {
        condition: WhereCondition,
    }

    impl GetTestEntityQuery {
        pub fn new(condition: WhereCondition) -> Self {
            Self { condition }
        }
    }

    impl Query for GetTestEntityQuery {
        type Entity = TestEntity;

        fn filters(&self) -> WhereCondition {
            self.condition.clone()
        }

        fn get_definition(&self, condition: &str) -> String {
            let aliases = SourceAlias::new(&[("{:test:}", "test")]);
            let projection = Self::Entity::get_projection().expand(aliases);

            format!("select {projection} from query_test as test where {condition}")
        }
    }

    struct UpdateTestEntityQuery {
        payload: FixedColumnPayload<4>,
    }

    impl UpdateTestEntityQuery {
        pub fn new(payload: FixedColumnPayload<4>) -> Self {
            Self { payload }
        }
    }

    impl Query for UpdateTestEntityQuery {
        type Entity = TestEntity;

        fn filters(&self) -> WhereCondition {
            WhereCondition::default()
        }

        fn mutation_payload(&self) -> Option<&dyn MutationPayload> {
            Some(&self.payload)
        }

        fn get_definition(&self, _condition: &str) -> String {
            let aliases = SourceAlias::new(&[("{:test:}", "query_test")]);
            let projection = Self::Entity::get_projection().expand(aliases);

            format!(
                r#"
insert into query_test ({columns}) values {binding_columns}
  on conflict (text_data) do update set
    real_data = excluded.real_data,
    integer_data = excluded.integer_data,
    maybe_null = excluded.maybe_null 
returning {projection}
"#,
                columns = self.payload.joined_columns(),
                binding_columns = self.payload.binding_columns(),
            )
        }
    }

    impl GetAllCondition for GetTestEntityQuery {}

    fn init_database() -> SqliteConnection {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        connection
            .execute(
                "
            drop table if exists query_test;
            create table query_test(text_data text not null primary key, real_data real not null, integer_data integer not null, maybe_null integer);
            insert into query_test(text_data, real_data, integer_data, maybe_null) values ('row 1', 1.23, -52, null);
            insert into query_test(text_data, real_data, integer_data, maybe_null) values ('row 2', 2.34, 1789, 0);
            ",
            )
            .unwrap();

        connection
    }

    #[test]
    pub fn simple_test() {
        let connection = init_database();
        let mut cursor = connection
            .fetch(GetTestEntityQuery::new(WhereCondition::default()))
            .unwrap();
        let entity = cursor.next().expect("there should be two results, none returned");
        assert_eq!(
            TestEntity {
                text_data: "row 1".to_string(),
                real_data: 1.23,
                integer_data: -52,
                maybe_null: None
            },
            entity
        );
        let entity = cursor.next().expect("there should be two results, only one returned");
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
        let connection = init_database();
        let mut cursor = connection
            .fetch(GetTestEntityQuery::new(WhereCondition::new(
                "maybe_null is not null",
                Vec::new(),
            )))
            .unwrap();
        let entity = cursor.next().expect("there should be one result, none returned");
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
        let connection = init_database();
        let mut cursor = connection
            .fetch(GetTestEntityQuery::new(WhereCondition::new(
                "text_data like ?",
                vec![Value::String("%1".to_string())],
            )))
            .unwrap();
        let entity = cursor.next().expect("there should be one result, none returned");
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
        let connection = init_database();
        let payload =
            FixedColumnPayload::new(["text_data", "real_data", "integer_data", "maybe_null"])
                .with_rows(vec![[
                    Value::String("row 1".to_string()),
                    Value::Float(1.234),
                    Value::Integer(0),
                    Value::Null,
                ]]);

        let mut cursor = connection.fetch(UpdateTestEntityQuery::new(payload)).unwrap();

        let entity = cursor.next().expect("there should be one result, none returned");
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

    #[test]
    fn test_upsertion_with_more_than_sqlite_binding_limit_automatically_chunk() {
        // Note: max sqlite binding is 32 766
        let connection = init_database();
        let payload =
            FixedColumnPayload::new(["text_data", "real_data", "integer_data", "maybe_null"])
                .with_rows(
                    (1..30_000)
                        .map(|i| {
                            [
                                Value::String(format!("row-{i}")),
                                Value::Float(1.234),
                                Value::Integer(0),
                                Value::Null,
                            ]
                        })
                        .collect(),
                );

        connection
            .apply(UpdateTestEntityQuery::new(payload))
            .expect("should automatically chunk when sqlite binding limit is reached");
    }
}
