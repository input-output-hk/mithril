use std::iter::repeat;

use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SignerRecord;

/// Query used by the [signer importer][crate::tools::SignersImporter] to register a [SignerRecord]
/// in the sqlite database.
///
/// If it already exists it's `pool_ticker` and `updated_at` fields will be updated.
pub struct ImportSignerRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> ImportSignerRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_import_condition(&self, signer_records: Vec<SignerRecord>) -> WhereCondition {
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

        WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values,
        )
    }

    pub(crate) fn persist(&self, signer_record: SignerRecord) -> StdResult<SignerRecord> {
        let filters = self.get_import_condition(vec![signer_record.clone()]);

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!("No entity returned by the persister, signer_record = {signer_record:?}")
        });

        Ok(entity)
    }

    pub(crate) fn persist_many(
        &self,
        signer_records: Vec<SignerRecord>,
    ) -> StdResult<Vec<SignerRecord>> {
        let filters = self.get_import_condition(signer_records);

        Ok(self.find(filters)?.collect())
    }
}

impl<'conn> Provider<'conn> for ImportSignerRecordProvider<'conn> {
    type Entity = SignerRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
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
    use sqlite::Connection;

    use crate::database::test_helper::{apply_all_migrations_to_db, insert_signers};

    use super::*;

    #[test]
    fn test_update_signer_record() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        insert_signers(&connection, signer_records_fake.clone()).unwrap();

        let provider = ImportSignerRecordProvider::new(&connection);

        for signer_record in signer_records_fake.clone() {
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }

        for mut signer_record in signer_records_fake {
            signer_record.pool_ticker = Some(format!("new-pool-{}", signer_record.signer_id));
            signer_record.updated_at += Duration::try_hours(1).unwrap();
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }
    }

    #[test]
    fn test_update_many_signer_records() {
        let mut signer_records_fake = SignerRecord::fake_records(5);
        signer_records_fake.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        insert_signers(&connection, signer_records_fake.clone()).unwrap();

        let provider = ImportSignerRecordProvider::new(&connection);
        let mut saved_records = provider.persist_many(signer_records_fake.clone()).unwrap();
        saved_records.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));
        assert_eq!(signer_records_fake, saved_records);

        for signer_record in signer_records_fake.iter_mut() {
            signer_record.pool_ticker = Some(format!("new-pool-{}", signer_record.signer_id));
            signer_record.updated_at += Duration::try_hours(1).unwrap();
        }
        let mut saved_records = provider.persist_many(signer_records_fake.clone()).unwrap();
        saved_records.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));
        assert_eq!(signer_records_fake, saved_records);
    }
}
