use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SignerRecord;

/// Query to register a [SignerRecord] in the sqlite database
///
/// If it already exists it's `last_registered_at` and `updated_at` fields will be updated.
pub(crate) struct RegisterSignerRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> RegisterSignerRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    fn get_register_condition(&self, signer_record: SignerRecord) -> WhereCondition {
        WhereCondition::new(
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
        )
    }

    pub(crate) fn persist(&self, signer_record: SignerRecord) -> StdResult<SignerRecord> {
        let filters = self.get_register_condition(signer_record.clone());

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!("No entity returned by the persister, signer_record = {signer_record:?}")
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for RegisterSignerRecordProvider<'conn> {
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
            "insert into signer {condition} on conflict (signer_id) do update set \
            updated_at = excluded.updated_at, last_registered_at = excluded.last_registered_at returning {projection}"
        )
    }
}

#[cfg(test)]
mod tests {
    use chrono::Duration;
    use sqlite::Connection;

    use crate::database::test_helper::apply_all_migrations_to_db;

    use super::*;

    #[test]
    fn test_insert_signer_record() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();

        let provider = RegisterSignerRecordProvider::new(&connection);

        for signer_record in signer_records_fake.clone() {
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }

        for mut signer_record in signer_records_fake {
            signer_record.updated_at += Duration::try_hours(1).unwrap();
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }
    }
}
