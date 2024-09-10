use sqlite::Value;

use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::BufferedSingleSignatureRecord;

/// Query to insert or replace [BufferedSingleSignatureRecord] in the sqlite database
pub struct InsertOrReplaceBufferedSingleSignatureRecordQuery {
    condition: WhereCondition,
}

impl InsertOrReplaceBufferedSingleSignatureRecordQuery {
    pub fn one(record: BufferedSingleSignatureRecord) -> Self {
        let condition =
        WhereCondition::new(
            "(party_id, epoch, signed_entity_type_id, lottery_indexes, signature, created_at) values (?*, ?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(record.party_id),
                Value::Integer(
                    record.epoch.try_into().unwrap(),
                ),
                Value::Integer(record.signed_entity_type_id.index() as i64),
                Value::String(serde_json::to_string(&record.lottery_indexes).unwrap()),
                Value::String(record.signature),
                Value::String(record.created_at.to_rfc3339()),
            ],
        );

        Self { condition }
    }
}

impl Query for InsertOrReplaceBufferedSingleSignatureRecordQuery {
    type Entity = BufferedSingleSignatureRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
            "{:buffered_single_signature:}",
            "buffered_single_signature",
        )]));

        format!(
            "insert or replace into buffered_single_signature {condition} returning {projection}"
        )
    }
}

#[cfg(test)]
mod tests {
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::{
        main_db_connection, setup_buffered_single_signature_records,
    };

    use super::*;

    #[test]
    fn test_insert_or_replace_single_signature_record() {
        let records = setup_buffered_single_signature_records(1, 3);

        let connection = main_db_connection().unwrap();

        for buffered_signature_record in records.clone() {
            let single_signature_record_saved = connection
                .fetch_first(InsertOrReplaceBufferedSingleSignatureRecordQuery::one(
                    buffered_signature_record.clone(),
                ))
                .unwrap();
            assert_eq!(
                Some(buffered_signature_record),
                single_signature_record_saved
            );
        }

        for mut buffered_signature_record in records {
            buffered_signature_record.lottery_indexes.push(5);
            let single_signature_record_saved = connection
                .fetch_first(InsertOrReplaceBufferedSingleSignatureRecordQuery::one(
                    buffered_signature_record.clone(),
                ))
                .unwrap();
            assert_eq!(
                Some(buffered_signature_record),
                single_signature_record_saved
            );
        }
    }
}
