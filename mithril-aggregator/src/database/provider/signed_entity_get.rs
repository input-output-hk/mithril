use sqlite::Value;

use mithril_common::entities::SignedEntityTypeDiscriminants;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SignedEntityRecord;

/// Simple queries to retrieve [SignedEntityRecord] from the sqlite database.
pub(crate) struct GetSignedEntityRecordProvider<'client> {
    client: &'client SqliteConnection,
}

impl<'client> GetSignedEntityRecordProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client SqliteConnection) -> Self {
        Self { client }
    }

    fn condition_by_signed_entity_id(&self, signed_entity_id: &str) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signed_entity_id = ?*",
            vec![Value::String(signed_entity_id.to_owned())],
        ))
    }

    fn condition_by_certificate_id(&self, certificate_id: &str) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "certificate_id = ?*",
            vec![Value::String(certificate_id.to_owned())],
        ))
    }

    fn condition_by_certificates_ids(&self, certificates_ids: &[&str]) -> WhereCondition {
        let ids_values = certificates_ids
            .iter()
            .map(|id| Value::String(id.to_string()))
            .collect();

        WhereCondition::where_in("certificate_id", ids_values)
    }

    fn condition_by_signed_entity_type(
        &self,
        signed_entity_type: &SignedEntityTypeDiscriminants,
    ) -> StdResult<WhereCondition> {
        let signed_entity_type_id: i64 = signed_entity_type.index() as i64;

        Ok(WhereCondition::new(
            "signed_entity_type_id = ?*",
            vec![Value::Integer(signed_entity_type_id)],
        ))
    }

    /// Get SignedEntityRecords for a given signed_entity id.
    pub fn get_by_signed_entity_id(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<EntityCursor<SignedEntityRecord>> {
        let filters = self.condition_by_signed_entity_id(signed_entity_id)?;
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    /// Get [record][SignedEntityRecord] for a given `certificate_id`.
    pub fn get_by_certificate_id(
        &self,
        certificate_id: &str,
    ) -> StdResult<EntityCursor<SignedEntityRecord>> {
        let filters = self.condition_by_certificate_id(certificate_id)?;
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    /// Get [records][SignedEntityRecord] for a list of given `certificates_ids`.
    pub fn get_by_certificates_ids(
        &self,
        certificates_ids: &[&str],
    ) -> StdResult<EntityCursor<SignedEntityRecord>> {
        let filters = self.condition_by_certificates_ids(certificates_ids);
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    /// Get SignedEntityRecords for a given signed entity type.
    pub fn get_by_signed_entity_type(
        &self,
        signed_entity_type: &SignedEntityTypeDiscriminants,
    ) -> StdResult<EntityCursor<SignedEntityRecord>> {
        let filters = self.condition_by_signed_entity_type(signed_entity_type)?;
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    #[cfg(test)]
    /// Get all SignedEntityRecords.
    pub fn get_all(&self) -> StdResult<EntityCursor<SignedEntityRecord>> {
        let filters = WhereCondition::default();
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }
}

impl<'client> Provider<'client> for GetSignedEntityRecordProvider<'client> {
    type Entity = SignedEntityRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:signed_entity:}", "se")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!(
            "select {projection} from signed_entity as se where {condition} order by ROWID desc"
        )
    }
}

#[cfg(test)]
mod tests {
    use sqlite::Connection;

    use mithril_common::entities::CardanoDbBeacon;
    use mithril_common::entities::SignedEntityType;

    use crate::database::test_helper::{
        apply_all_migrations_to_db, disable_foreign_key_support, insert_signed_entities,
    };

    use super::*;

    pub fn setup_signed_entity_db(
        connection: &SqliteConnection,
        signed_entity_records: Vec<SignedEntityRecord>,
    ) -> StdResult<()> {
        apply_all_migrations_to_db(connection)?;
        disable_foreign_key_support(connection)?;
        insert_signed_entities(connection, signed_entity_records)?;

        Ok(())
    }

    #[test]
    fn test_get_signed_entity_records() {
        let signed_entity_records = SignedEntityRecord::fake_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signed_entity_db(&connection, signed_entity_records.clone()).unwrap();

        let provider = GetSignedEntityRecordProvider::new(&connection);

        let first_signed_entity_type = signed_entity_records.first().unwrap().to_owned();
        let signed_entity_records: Vec<SignedEntityRecord> = provider
            .get_by_signed_entity_id(&first_signed_entity_type.signed_entity_id)
            .unwrap()
            .collect();
        assert_eq!(vec![first_signed_entity_type], signed_entity_records);

        let signed_entity_records: Vec<SignedEntityRecord> = provider
            .get_by_signed_entity_type(&SignedEntityTypeDiscriminants::CardanoImmutableFilesFull)
            .unwrap()
            .collect();
        let expected_signed_entity_records: Vec<SignedEntityRecord> = signed_entity_records
            .iter()
            .filter_map(|se| {
                (se.signed_entity_type.index()
                    == SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::default())
                        .index())
                .then_some(se.to_owned())
            })
            .collect();
        assert_eq!(expected_signed_entity_records, signed_entity_records);

        let signed_entity_records: Vec<SignedEntityRecord> = provider.get_all().unwrap().collect();
        let expected_signed_entity_records: Vec<SignedEntityRecord> =
            signed_entity_records.iter().map(|c| c.to_owned()).collect();
        assert_eq!(expected_signed_entity_records, signed_entity_records);
    }
}
