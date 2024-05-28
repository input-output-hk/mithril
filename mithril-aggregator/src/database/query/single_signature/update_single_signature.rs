use sqlite::Value;

use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::SingleSignatureRecord;

/// Query to update [SingleSignatureRecord] in the sqlite database
pub struct UpdateSingleSignatureRecordQuery {
    condition: WhereCondition,
}

impl UpdateSingleSignatureRecordQuery {
    pub fn one(single_signature_record: SingleSignatureRecord) -> Self {
        let condition =
        WhereCondition::new(
            "(open_message_id, signer_id, registration_epoch_setting_id, lottery_indexes, signature, created_at) values (?*, ?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(single_signature_record.open_message_id.to_string()),
                Value::String(single_signature_record.signer_id),
                Value::Integer(
                    single_signature_record.registration_epoch_setting_id.try_into().unwrap(),
                ),
                Value::String(serde_json::to_string(&single_signature_record.lottery_indexes).unwrap()),
                Value::String(single_signature_record.signature),
                Value::String(single_signature_record.created_at.to_rfc3339()),
            ],
        );

        Self { condition }
    }
}

impl Query for UpdateSingleSignatureRecordQuery {
    type Entity = SingleSignatureRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
            "{:single_signature:}",
            "single_signature",
        )]));

        format!("insert or replace into single_signature {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use crate::database::test_helper::{main_db_connection, setup_single_signature_records};
    use mithril_persistence::sqlite::ConnectionExtensions;

    use super::*;

    #[test]
    fn test_update_single_signature_record() {
        let single_signature_records = setup_single_signature_records(2, 3, 4);

        let connection = main_db_connection().unwrap();

        for single_signature_record in single_signature_records.clone() {
            let single_signature_record_saved = connection
                .fetch_first(UpdateSingleSignatureRecordQuery::one(
                    single_signature_record.clone(),
                ))
                .unwrap();
            assert_eq!(Some(single_signature_record), single_signature_record_saved);
        }

        for mut single_signature_record in single_signature_records {
            single_signature_record.lottery_indexes.push(5);
            let single_signature_record_saved = connection
                .fetch_first(UpdateSingleSignatureRecordQuery::one(
                    single_signature_record.clone(),
                ))
                .unwrap();
            assert_eq!(Some(single_signature_record), single_signature_record_saved);
        }
    }
}
