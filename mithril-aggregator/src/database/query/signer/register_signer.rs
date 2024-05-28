use sqlite::Value;

use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::SignerRecord;

/// Query to register a [SignerRecord] in the sqlite database
///
/// If it already exists it's `last_registered_at` and `updated_at` fields will be updated.
pub struct RegisterSignerRecordQuery {
    condition: WhereCondition,
}

impl RegisterSignerRecordQuery {
    pub fn one(signer_record: SignerRecord) -> Self {
        let condition = WhereCondition::new(
            "(signer_id, pool_ticker, created_at, updated_at, last_registered_at) values (?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(signer_record.signer_id),
                signer_record
                    .pool_ticker
                    .map(Value::String)
                    .unwrap_or(Value::Null),
                Value::String(signer_record.created_at.to_rfc3339()),
                Value::String(signer_record.updated_at.to_rfc3339()),
                signer_record
                    .last_registered_at
                    .map(|d| Value::String(d.to_rfc3339()))
                    .unwrap_or(Value::Null),
            ],
        );

        Self { condition }
    }
}

impl Query for RegisterSignerRecordQuery {
    type Entity = SignerRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection =
            Self::Entity::get_projection().expand(SourceAlias::new(&[("{:signer:}", "signer")]));

        format!(
            "insert into signer {condition} on conflict (signer_id) do update set \
            updated_at = excluded.updated_at, last_registered_at = excluded.last_registered_at returning {projection}"
        )
    }
}

#[cfg(test)]
mod tests {
    use chrono::Duration;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::main_db_connection;

    use super::*;

    #[test]
    fn test_insert_signer_record() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = main_db_connection().unwrap();

        for signer_record in signer_records_fake.clone() {
            let signer_record_saved = connection
                .fetch_first(RegisterSignerRecordQuery::one(signer_record.clone()))
                .unwrap();
            assert_eq!(Some(signer_record), signer_record_saved);
        }

        for mut signer_record in signer_records_fake {
            signer_record.updated_at += Duration::try_hours(1).unwrap();
            let signer_record_saved = connection
                .fetch_first(RegisterSignerRecordQuery::one(signer_record.clone()))
                .unwrap();
            assert_eq!(Some(signer_record), signer_record_saved);
        }
    }
}
