use anyhow::Context;
use sqlite::Value;

use mithril_common::{entities::Epoch, StdResult};
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::EpochSettingRecord;

/// Simple queries to retrieve [EpochSettingRecord] from the sqlite database.
pub struct GetEpochSettingProvider<'client> {
    client: &'client SqliteConnection,
}

impl<'client> GetEpochSettingProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client SqliteConnection) -> Self {
        Self { client }
    }

    fn condition_by_epoch(&self, epoch: &Epoch) -> StdResult<WhereCondition> {
        let epoch_setting_id: i64 = epoch
            .try_into()
            .with_context(|| format!("Can not convert epoch: '{epoch}'"))?;

        Ok(WhereCondition::new(
            "epoch_setting_id = ?*",
            vec![Value::Integer(epoch_setting_id)],
        ))
    }

    /// Get EpochSettingRecords for a given Epoch for given pool_ids.
    pub fn get_by_epoch(&self, epoch: &Epoch) -> StdResult<EntityCursor<EpochSettingRecord>> {
        let filters = self.condition_by_epoch(epoch)?;
        let epoch_setting_record = self.find(filters)?;

        Ok(epoch_setting_record)
    }
}

impl<'client> Provider<'client> for GetEpochSettingProvider<'client> {
    type Entity = EpochSettingRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:epoch_setting:}", "es")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from epoch_setting as es where {condition} order by epoch_setting_id desc")
    }
}

#[cfg(test)]
mod tests {
    use sqlite::Connection;

    use mithril_common::entities::ProtocolParameters;

    use crate::database::test_helper::{apply_all_migrations_to_db, insert_epoch_settings};

    use super::*;

    pub fn setup_epoch_setting_db(
        connection: &SqliteConnection,
        epoch_to_insert_settings: &[u64],
    ) -> StdResult<()> {
        apply_all_migrations_to_db(connection)?;
        insert_epoch_settings(connection, epoch_to_insert_settings)?;
        Ok(())
    }

    #[test]
    fn test_get_epoch_settings() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_epoch_setting_db(&connection, &[1, 2, 3]).unwrap();

        let provider = GetEpochSettingProvider::new(&connection);

        let mut cursor = provider.get_by_epoch(&Epoch(1)).unwrap();
        let epoch_setting_record = cursor
            .next()
            .expect("Should have an epoch setting for epoch 1.");
        assert_eq!(Epoch(1), epoch_setting_record.epoch_setting_id);
        assert_eq!(
            ProtocolParameters::new(1, 2, 1.0),
            epoch_setting_record.protocol_parameters
        );

        let mut cursor = provider.get_by_epoch(&Epoch(3)).unwrap();
        let epoch_setting_record = cursor
            .next()
            .expect("Should have an epoch setting for epoch 3.");
        assert_eq!(Epoch(3), epoch_setting_record.epoch_setting_id);
        assert_eq!(
            ProtocolParameters::new(3, 4, 1.0),
            epoch_setting_record.protocol_parameters
        );

        let cursor = provider.get_by_epoch(&Epoch(5)).unwrap();
        assert_eq!(0, cursor.count());
    }
}
