#[cfg(test)]
use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SignerRecord;

/// Simple queries to retrieve [SignerRecord] from the sqlite database.
pub(crate) struct GetSignerRecordProvider<'client> {
    client: &'client SqliteConnection,
}

impl<'client> GetSignerRecordProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client SqliteConnection) -> Self {
        Self { client }
    }

    #[cfg(test)]
    fn condition_by_signer_id(&self, signer_id: String) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signer_id = ?*",
            vec![Value::String(signer_id)],
        ))
    }

    #[cfg(test)]
    /// Get SignerRecords for a given signer id.
    pub fn get_by_signer_id(&self, signer_id: String) -> StdResult<EntityCursor<SignerRecord>> {
        let filters = self.condition_by_signer_id(signer_id)?;
        let signer_record = self.find(filters)?;

        Ok(signer_record)
    }

    /// Get all SignerRecords.
    pub fn get_all(&self) -> StdResult<EntityCursor<SignerRecord>> {
        let filters = WhereCondition::default();
        let signer_record = self.find(filters)?;

        Ok(signer_record)
    }
}

impl<'client> Provider<'client> for GetSignerRecordProvider<'client> {
    type Entity = SignerRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:signer:}", "s")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from signer as s where {condition} order by ROWID desc")
    }
}

#[cfg(test)]
mod tests {
    use sqlite::Connection;

    use crate::database::test_helper::{apply_all_migrations_to_db, insert_signers};

    use super::*;

    #[test]
    fn test_get_signer_records() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        insert_signers(&connection, signer_records_fake.clone()).unwrap();

        let provider = GetSignerRecordProvider::new(&connection);

        let signer_records: Vec<SignerRecord> = provider
            .get_by_signer_id(signer_records_fake[0].signer_id.to_owned())
            .unwrap()
            .collect();
        let expected_signer_records: Vec<SignerRecord> = vec![signer_records_fake[0].to_owned()];
        assert_eq!(expected_signer_records, signer_records);

        let signer_records: Vec<SignerRecord> = provider
            .get_by_signer_id(signer_records_fake[2].signer_id.to_owned())
            .unwrap()
            .collect();
        let expected_signer_records: Vec<SignerRecord> = vec![signer_records_fake[2].to_owned()];
        assert_eq!(expected_signer_records, signer_records);

        let cursor = provider
            .get_by_signer_id("signer-id-not-registered".to_string())
            .unwrap();
        assert_eq!(0, cursor.count());

        let signer_records: Vec<SignerRecord> = provider.get_all().unwrap().collect();
        let expected_signer_records: Vec<SignerRecord> =
            signer_records_fake.into_iter().rev().collect();
        assert_eq!(expected_signer_records, signer_records);
    }
}
