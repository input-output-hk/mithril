use std::iter::repeat;

use sqlite::Value;

use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::SignerRecord;

/// Query used by the [signer importer][crate::tools::SignersImporter] to register a [SignerRecord]
/// in the sqlite database.
///
/// If it already exists it's `pool_ticker` and `updated_at` fields will be updated.
pub struct ImportSignerRecordQuery {
    condition: WhereCondition,
}

impl ImportSignerRecordQuery {
    pub fn one(signer_record: SignerRecord) -> Self {
        Self::many(vec![signer_record])
    }

    pub fn many(signer_records: Vec<SignerRecord>) -> Self {
        let columns = "(signer_id, pool_ticker, created_at, updated_at, last_registered_at)";
        let values_columns: Vec<&str> = repeat("(?*, ?*, ?*, ?*, ?*)")
            .take(signer_records.len())
            .collect();
        let values = signer_records
            .into_iter()
            .flat_map(|signer_record| {
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
                ]
            })
            .collect();

        let condition = WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values,
        );

        Self { condition }
    }
}

impl Query for ImportSignerRecordQuery {
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
            "insert into signer {condition} on conflict(signer_id) do update \
            set pool_ticker = excluded.pool_ticker, updated_at = excluded.updated_at returning {projection}"
        )
    }
}

#[cfg(test)]
mod tests {
    use chrono::Duration;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::{insert_signers, main_db_connection};

    use super::*;

    #[test]
    fn test_update_signer_record() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = main_db_connection().unwrap();
        insert_signers(&connection, signer_records_fake.clone()).unwrap();

        for signer_record in signer_records_fake.clone() {
            let signer_record_saved = connection
                .fetch_first(ImportSignerRecordQuery::one(signer_record.clone()))
                .unwrap();
            assert_eq!(Some(signer_record), signer_record_saved);
        }

        for mut signer_record in signer_records_fake {
            signer_record.pool_ticker = Some(format!("new-pool-{}", signer_record.signer_id));
            signer_record.updated_at += Duration::try_hours(1).unwrap();
            let signer_record_saved = connection
                .fetch_first(ImportSignerRecordQuery::one(signer_record.clone()))
                .unwrap();
            assert_eq!(Some(signer_record), signer_record_saved);
        }
    }

    #[test]
    fn test_update_many_signer_records() {
        let mut signer_records_fake = SignerRecord::fake_records(5);
        signer_records_fake.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));

        let connection = main_db_connection().unwrap();
        insert_signers(&connection, signer_records_fake.clone()).unwrap();

        let mut saved_records: Vec<SignerRecord> = connection
            .fetch_collect(ImportSignerRecordQuery::many(signer_records_fake.clone()))
            .unwrap();
        saved_records.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));
        assert_eq!(signer_records_fake, saved_records);

        for signer_record in signer_records_fake.iter_mut() {
            signer_record.pool_ticker = Some(format!("new-pool-{}", signer_record.signer_id));
            signer_record.updated_at += Duration::try_hours(1).unwrap();
        }
        let mut saved_records: Vec<SignerRecord> = connection
            .fetch_collect(ImportSignerRecordQuery::many(signer_records_fake.clone()))
            .unwrap();
        saved_records.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));
        assert_eq!(signer_records_fake, saved_records);
    }
}
