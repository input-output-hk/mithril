use sqlite::Value;

use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::SignedEntityRecord;

/// Query to insert [SignedEntityRecord] in the sqlite database
pub struct InsertSignedEntityRecordQuery {
    condition: WhereCondition,
}

impl InsertSignedEntityRecordQuery {
    pub fn one(signed_entity_record: SignedEntityRecord) -> Self {
        Self {
            condition: WhereCondition::new(
            "(signed_entity_id, signed_entity_type_id, certificate_id, beacon, artifact, created_at) values (?*, ?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(signed_entity_record.signed_entity_id),
                Value::Integer(signed_entity_record.signed_entity_type.index() as i64),
                Value::String(signed_entity_record.certificate_id),
                Value::String(signed_entity_record.signed_entity_type.get_json_beacon().unwrap()),
                Value::String(signed_entity_record.artifact),
                Value::String(signed_entity_record.created_at.to_rfc3339()),
            ],
        )
        }
    }
}

impl Query for InsertSignedEntityRecordQuery {
    type Entity = SignedEntityRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:signed_entity:}", "signed_entity")]));

        format!("insert into signed_entity {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::main_db_connection;

    use super::*;

    #[test]
    fn test_insert_signed_entity_record() {
        let signed_entity_records = SignedEntityRecord::fake_records(5);

        let connection = main_db_connection().unwrap();

        for signed_entity_record in signed_entity_records {
            let signed_entity_record_saved = connection
                .fetch_first(InsertSignedEntityRecordQuery::one(
                    signed_entity_record.clone(),
                ))
                .unwrap();
            assert_eq!(Some(signed_entity_record), signed_entity_record_saved);
        }
    }
}
