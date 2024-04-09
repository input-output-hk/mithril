use sqlite::Value;
use uuid::Uuid;

use mithril_common::{entities::Epoch, StdResult};
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SingleSignatureRecord;

/// Simple queries to retrieve [SingleSignatureRecord] from the sqlite database.
pub struct GetSingleSignatureRecordProvider<'client> {
    client: &'client SqliteConnection,
}

#[allow(dead_code)] // todo: Unused in production code, Should we keep it ?
impl<'client> GetSingleSignatureRecordProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client SqliteConnection) -> Self {
        Self { client }
    }

    fn condition_by_open_message_id(&self, open_message_id: &Uuid) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "open_message_id = ?*",
            vec![Value::String(open_message_id.to_string())],
        ))
    }

    fn condition_by_signer_id(&self, signer_id: String) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signer_id = ?*",
            vec![Value::String(signer_id)],
        ))
    }

    fn condition_by_registration_epoch(
        &self,
        registration_epoch: &Epoch,
    ) -> StdResult<WhereCondition> {
        let epoch: i64 = registration_epoch.try_into()?;

        Ok(WhereCondition::new(
            "registration_epoch_setting_id = ?*",
            vec![Value::Integer(epoch)],
        ))
    }

    /// Get SingleSignatureRecords for a given Open Message id.
    pub fn get_by_open_message_id(
        &self,
        open_message_id: &Uuid,
    ) -> StdResult<EntityCursor<SingleSignatureRecord>> {
        let filters = self.condition_by_open_message_id(open_message_id)?;
        let single_signature_record = self.find(filters)?;

        Ok(single_signature_record)
    }

    /// Get all SingleSignatureRecords.
    pub fn get_all(&self) -> StdResult<EntityCursor<SingleSignatureRecord>> {
        let filters = WhereCondition::default();
        let single_signature_record = self.find(filters)?;

        Ok(single_signature_record)
    }
}

impl<'client> Provider<'client> for GetSingleSignatureRecordProvider<'client> {
    type Entity = SingleSignatureRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:single_signature:}", "ssig")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from single_signature as ssig where {condition} order by ROWID desc")
    }
}

#[cfg(test)]
mod tests {
    use sqlite::Connection;

    use crate::database::test_helper::{
        apply_all_migrations_to_db, disable_foreign_key_support, insert_single_signatures_in_db,
        setup_single_signature_records,
    };

    use super::*;

    #[tokio::test]
    async fn test_get_single_signature_records() {
        let single_signature_records_src = setup_single_signature_records(2, 3, 4);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        disable_foreign_key_support(&connection).unwrap();
        insert_single_signatures_in_db(&connection, single_signature_records_src.clone()).unwrap();

        let provider = GetSingleSignatureRecordProvider::new(&connection);

        let open_message_id_test = single_signature_records_src[0].open_message_id.to_owned();
        let single_signature_records: Vec<SingleSignatureRecord> = provider
            .get_by_open_message_id(&open_message_id_test)
            .unwrap()
            .collect();
        let expected_single_signature_records: Vec<SingleSignatureRecord> =
            single_signature_records
                .iter()
                .filter_map(|ssig| {
                    if ssig.open_message_id == open_message_id_test {
                        Some(ssig.to_owned())
                    } else {
                        None
                    }
                })
                .collect();
        assert!(!single_signature_records.is_empty());
        assert_eq!(expected_single_signature_records, single_signature_records);

        let open_message_id_test = single_signature_records
            .last()
            .unwrap()
            .open_message_id
            .to_owned();
        let single_signature_records: Vec<SingleSignatureRecord> = provider
            .get_by_open_message_id(&open_message_id_test)
            .unwrap()
            .collect();
        let expected_single_signature_records: Vec<SingleSignatureRecord> =
            single_signature_records
                .iter()
                .filter_map(|ssig| {
                    if ssig.open_message_id == open_message_id_test {
                        Some(ssig.to_owned())
                    } else {
                        None
                    }
                })
                .collect();
        assert!(!single_signature_records.is_empty());
        assert_eq!(expected_single_signature_records, single_signature_records);

        let open_message_id_test = Uuid::parse_str("193d1442-e89b-43cf-9519-04d8db9a12ff").unwrap();
        let single_signature_records: Vec<SingleSignatureRecord> = provider
            .get_by_open_message_id(&open_message_id_test)
            .unwrap()
            .collect();
        assert!(single_signature_records.is_empty());

        let single_signature_records_returned: Vec<SingleSignatureRecord> = provider
            .get_all()
            .unwrap()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect();
        assert_eq!(
            single_signature_records_src,
            single_signature_records_returned
        );
    }
}
