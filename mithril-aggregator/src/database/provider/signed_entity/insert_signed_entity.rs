use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SignedEntityRecord;

/// Query to insert [SignedEntityRecord] in the sqlite database
pub struct InsertSignedEntityRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> InsertSignedEntityRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_insert_condition(
        &self,
        signed_entity_record: SignedEntityRecord,
    ) -> WhereCondition {
        WhereCondition::new(
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

    pub(crate) fn persist(
        &self,
        signed_entity_record: SignedEntityRecord,
    ) -> StdResult<SignedEntityRecord> {
        let filters = self.get_insert_condition(signed_entity_record.clone());

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!(
                "No entity returned by the persister, signed_entity_record = {signed_entity_record:?}"
            )
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for InsertSignedEntityRecordProvider<'conn> {
    type Entity = SignedEntityRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
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
    use sqlite::Connection;

    use crate::database::test_helper::{apply_all_migrations_to_db, disable_foreign_key_support};

    use super::*;

    #[test]
    fn test_insert_signed_entity_record() {
        let signed_entity_records = SignedEntityRecord::fake_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        disable_foreign_key_support(&connection).unwrap();

        let provider = InsertSignedEntityRecordProvider::new(&connection);

        for signed_entity_record in signed_entity_records {
            let signed_entity_record_saved =
                provider.persist(signed_entity_record.clone()).unwrap();
            assert_eq!(signed_entity_record, signed_entity_record_saved);
        }
    }
}
