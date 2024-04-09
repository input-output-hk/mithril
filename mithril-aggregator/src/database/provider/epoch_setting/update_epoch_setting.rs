use sqlite::Value;

use mithril_common::entities::{Epoch, ProtocolParameters};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::EpochSettingRecord;

/// Query to update [EpochSettingRecord] in the sqlite database
pub struct UpdateEpochSettingProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> UpdateEpochSettingProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    // todo: not pub
    pub fn get_update_condition(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> WhereCondition {
        let epoch_setting_id: i64 = epoch.try_into().unwrap();

        WhereCondition::new(
            "(epoch_setting_id, protocol_parameters) values (?1, ?2)",
            vec![
                Value::Integer(epoch_setting_id),
                Value::String(serde_json::to_string(&protocol_parameters).unwrap()),
            ],
        )
    }

    pub fn persist(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> StdResult<EpochSettingRecord> {
        let filters = self.get_update_condition(epoch, protocol_parameters);

        let entity = self
            .find(filters)?
            .next()
            .unwrap_or_else(|| panic!("No entity returned by the persister, epoch = {epoch:?}"));

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for UpdateEpochSettingProvider<'conn> {
    type Entity = EpochSettingRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:epoch_setting:}", "epoch_setting")]));

        format!("insert or replace into epoch_setting {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use sqlite::Connection;

    use mithril_common::test_utils::fake_data;

    use crate::database::provider::GetEpochSettingProvider;
    use crate::database::test_helper::{apply_all_migrations_to_db, insert_epoch_settings};

    use super::*;

    #[test]
    fn test_update_epoch_setting() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        insert_epoch_settings(&connection, &[3]).unwrap();

        let provider = UpdateEpochSettingProvider::new(&connection);
        let epoch_setting_record = provider
            .persist(Epoch(3), fake_data::protocol_parameters())
            .unwrap();

        assert_eq!(Epoch(3), epoch_setting_record.epoch_setting_id);
        assert_eq!(
            fake_data::protocol_parameters(),
            epoch_setting_record.protocol_parameters
        );

        let provider = GetEpochSettingProvider::new(&connection);
        let mut cursor = provider.get_by_epoch(&Epoch(3)).unwrap();
        let epoch_setting_record = cursor
            .next()
            .expect("Should have an epoch setting for epoch 3.");

        assert_eq!(Epoch(3), epoch_setting_record.epoch_setting_id);
        assert_eq!(
            fake_data::protocol_parameters(),
            epoch_setting_record.protocol_parameters
        );
        assert_eq!(0, cursor.count());
    }
}
