use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SingleSignatureRecord;

/// Query to update [SingleSignatureRecord] in the sqlite database
pub(crate) struct UpdateSingleSignatureRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> UpdateSingleSignatureRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_update_condition(
        &self,
        single_signature_record: &SingleSignatureRecord,
    ) -> WhereCondition {
        WhereCondition::new(
            "(open_message_id, signer_id, registration_epoch_setting_id, lottery_indexes, signature, created_at) values (?*, ?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(single_signature_record.open_message_id.to_string()),
                Value::String(single_signature_record.signer_id.to_owned()),
                Value::Integer(
                    single_signature_record.registration_epoch_setting_id.try_into().unwrap(),
                ),
                Value::String(serde_json::to_string(&single_signature_record.lottery_indexes).unwrap()),
                Value::String(single_signature_record.signature.to_owned()),
                Value::String(single_signature_record.created_at.to_rfc3339()),
            ],
        )
    }

    pub(crate) fn persist(
        &self,
        single_signature_record: SingleSignatureRecord,
    ) -> StdResult<SingleSignatureRecord> {
        let filters = self.get_update_condition(&single_signature_record);

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!(
                "No entity returned by the persister, single_signature_record = {single_signature_record:?}"
            )
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for UpdateSingleSignatureRecordProvider<'conn> {
    type Entity = SingleSignatureRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
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
    use sqlite::Connection;

    use crate::database::test_helper::{
        apply_all_migrations_to_db, disable_foreign_key_support, setup_single_signature_records,
    };

    use super::*;

    #[test]
    fn test_update_single_signature_record() {
        let single_signature_records = setup_single_signature_records(2, 3, 4);
        let single_signature_records_copy = single_signature_records.clone();

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        disable_foreign_key_support(&connection).unwrap();

        let provider = UpdateSingleSignatureRecordProvider::new(&connection);

        for single_signature_record in single_signature_records {
            let single_signature_record_saved =
                provider.persist(single_signature_record.clone()).unwrap();
            assert_eq!(single_signature_record, single_signature_record_saved);
        }

        for single_signature_record in single_signature_records_copy {
            let single_signature_record_saved =
                provider.persist(single_signature_record.clone()).unwrap();
            assert_eq!(single_signature_record, single_signature_record_saved);
        }
    }
}
