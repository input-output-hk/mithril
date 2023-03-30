use std::sync::Arc;

use async_trait::async_trait;
use sqlite::{Connection, Value};

use mithril_common::{
    entities::{Epoch, ProtocolParameters},
    sqlite::{
        EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
        WhereCondition,
    },
    store::{
        adapter::{AdapterError, StoreAdapter},
        StoreError,
    },
};

use crate::ProtocolParametersStorer;

use mithril_common::StdError;
use tokio::sync::Mutex;

/// Delete epoch settings for Epoch older than this.
const EPOCH_SETTING_PRUNE_EPOCH_THRESHOLD: Epoch = Epoch(3);

/// Settings for an epoch, including the protocol parameters.
#[derive(Debug, PartialEq)]
pub struct EpochSettingRecord {
    /// Epoch setting id, i.e. the epoch number.
    epoch_setting_id: Epoch,

    /// Protocol parameters.
    protocol_parameters: ProtocolParameters,
}

impl SqLiteEntity for EpochSettingRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let epoch_setting_id_int = row.get::<i64, _>(0);
        let protocol_parameters_string = &row.get::<String, _>(1);

        let epoch_setting_record = Self {
            epoch_setting_id: Epoch(epoch_setting_id_int.try_into().map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not cast i64 ({epoch_setting_id_int}) to u64. Error: '{e}'"
                ))
            })?),
            protocol_parameters: serde_json::from_str(protocol_parameters_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{protocol_parameters_string}' to ProtocolParameters. Error: {e}"
                    ))
                },
            )?,
        };

        Ok(epoch_setting_record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field(
            "epoch_setting_id",
            "{:epoch_setting:}.epoch_setting_id",
            "integer",
        );
        projection.add_field(
            "protocol_parameters",
            "{:epoch_setting:}.protocol_parameters",
            "text",
        );

        projection
    }
}

/// Simple [EpochSetting] provider.
pub struct EpochSettingProvider<'client> {
    client: &'client Connection,
}

impl<'client> EpochSettingProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client Connection) -> Self {
        Self { client }
    }

    fn condition_by_epoch(&self, epoch: &Epoch) -> Result<WhereCondition, StdError> {
        let epoch_setting_id: i64 = i64::try_from(epoch.0)?;

        Ok(WhereCondition::new(
            "epoch_setting_id = ?*",
            vec![Value::Integer(epoch_setting_id)],
        ))
    }

    /// Get EpochSettingRecords for a given Epoch for given pool_ids.
    pub fn get_by_epoch(
        &self,
        epoch: &Epoch,
    ) -> Result<EntityCursor<EpochSettingRecord>, StdError> {
        let filters = self.condition_by_epoch(epoch)?;
        let epoch_setting_record = self.find(filters)?;

        Ok(epoch_setting_record)
    }

    /// Get all EpochSettingRecords.
    pub fn get_all(&self) -> Result<EntityCursor<EpochSettingRecord>, StdError> {
        let filters = WhereCondition::default();
        let epoch_setting_record = self.find(filters)?;

        Ok(epoch_setting_record)
    }
}

impl<'client> Provider<'client> for EpochSettingProvider<'client> {
    type Entity = EpochSettingRecord;

    fn get_connection(&'client self) -> &'client Connection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:epoch_setting:}", "es")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from epoch_setting as es where {condition} order by epoch_setting_id desc")
    }
}

/// Query to update the epoch setting
pub struct UpdateEpochSettingProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> UpdateEpochSettingProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    fn get_update_condition(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> WhereCondition {
        let epoch_setting_id = i64::try_from(epoch.0).unwrap();

        WhereCondition::new(
            "(epoch_setting_id, protocol_parameters) values (?1, ?2)",
            vec![
                Value::Integer(epoch_setting_id),
                Value::String(serde_json::to_string(&protocol_parameters).unwrap()),
            ],
        )
    }

    fn persist(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> Result<EpochSettingRecord, StdError> {
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

    fn get_connection(&'conn self) -> &'conn Connection {
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

/// Provider to remove old data from theepoch_setting table
pub struct DeleteEpochSettingProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> Provider<'conn> for DeleteEpochSettingProvider<'conn> {
    type Entity = EpochSettingRecord;

    fn get_connection(&'conn self) -> &'conn Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:epoch_setting:}", "epoch_setting")]));

        format!("delete from epoch_setting where {condition} returning {projection}")
    }
}

impl<'conn> DeleteEpochSettingProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    /// Create the SQL condition to prune data older than the given Epoch.
    fn get_prune_condition(&self, epoch_threshold: Epoch) -> WhereCondition {
        let epoch_setting_id_value = Value::Integer(i64::try_from(epoch_threshold.0).unwrap());

        WhereCondition::new("epoch_setting_id < ?*", vec![epoch_setting_id_value])
    }

    /// Prune the epoch setting data older than the given epoch.
    pub fn prune(
        &self,
        epoch_threshold: Epoch,
    ) -> Result<EntityCursor<EpochSettingRecord>, StdError> {
        let filters = self.get_prune_condition(epoch_threshold);

        self.find(filters)
    }
}
/// Service to deal with epoch settings (read & write).
pub struct EpochSettingStore {
    connection: Arc<Mutex<Connection>>,
}

impl EpochSettingStore {
    /// Create a new EpochSetting service
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }
}

#[async_trait]
impl StoreAdapter for EpochSettingStore {
    type Key = Epoch;
    type Record = ProtocolParameters;

    async fn store_record(
        &mut self,
        key: &Self::Key,
        record: &Self::Record,
    ) -> Result<(), AdapterError> {
        let epoch = *key;
        let connection = &*self.connection.lock().await;
        let provider = UpdateEpochSettingProvider::new(connection);
        connection
            .execute("begin transaction")
            .map_err(|e| AdapterError::QueryError(e.into()))?;

        let _epoch_setting_record = provider
            .persist(epoch, record.to_owned())
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;

        // Clean useless old epoch settings if needed.
        if epoch > EPOCH_SETTING_PRUNE_EPOCH_THRESHOLD {
            let _ = DeleteEpochSettingProvider::new(connection)
                .prune(epoch - EPOCH_SETTING_PRUNE_EPOCH_THRESHOLD)
                .map_err(AdapterError::InitializationError)?;
        }
        connection
            .execute("commit transaction")
            .map_err(|e| AdapterError::QueryError(e.into()))?;

        Ok(())
    }

    async fn get_record(&self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = EpochSettingProvider::new(connection);
        let mut cursor = provider
            .get_by_epoch(key)
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let epoch_setting_record = cursor.next();

        Ok(epoch_setting_record.map(|es| es.protocol_parameters))
    }

    async fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        Ok(self.get_record(key).await?.is_some())
    }

    async fn get_last_n_records(
        &self,
        how_many: usize,
    ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = EpochSettingProvider::new(connection);
        let cursor = provider
            .get_all()
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let protocol_parameters: Vec<(Epoch, ProtocolParameters)> = cursor
            .map(|es| (es.epoch_setting_id, es.protocol_parameters))
            .take(how_many)
            .collect();
        Ok(protocol_parameters)
    }

    async fn remove(&mut self, _key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        unimplemented!()
    }

    async fn get_iter(&self) -> Result<Box<dyn Iterator<Item = Self::Record> + '_>, AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = EpochSettingProvider::new(connection);
        let cursor = provider
            .get_all()
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let protocol_parameters: Vec<ProtocolParameters> =
            cursor.map(|es| es.protocol_parameters).collect();
        Ok(Box::new(protocol_parameters.into_iter()))
    }
}

#[async_trait]
impl ProtocolParametersStorer for EpochSettingStore {
    async fn save_protocol_parameters(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> Result<Option<ProtocolParameters>, StoreError> {
        let connection = &*self.connection.lock().await;
        let provider = UpdateEpochSettingProvider::new(connection);
        connection
            .execute("begin transaction")
            .map_err(|e| AdapterError::QueryError(e.into()))?;

        let epoch_setting_record = provider
            .persist(epoch, protocol_parameters)
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;

        // Clean useless old epoch settings if needed.
        if epoch > EPOCH_SETTING_PRUNE_EPOCH_THRESHOLD {
            let _ = DeleteEpochSettingProvider::new(connection)
                .prune(epoch - EPOCH_SETTING_PRUNE_EPOCH_THRESHOLD)
                .map_err(AdapterError::InitializationError)?;
        }
        connection
            .execute("commit transaction")
            .map_err(|e| AdapterError::QueryError(e.into()))?;

        Ok(Some(epoch_setting_record.protocol_parameters))
    }

    async fn get_protocol_parameters(
        &self,
        epoch: Epoch,
    ) -> Result<Option<ProtocolParameters>, StoreError> {
        let connection = &*self.connection.lock().await;
        let provider = EpochSettingProvider::new(connection);
        let mut cursor = provider
            .get_by_epoch(&epoch)
            .map_err(|e| AdapterError::GeneralError(format!("Could not get epoch setting: {e}")))?;

        if let Some(epoch_setting_record) = cursor.next() {
            return Ok(Some(epoch_setting_record.protocol_parameters));
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    pub fn setup_epoch_setting_db(connection: &Connection) -> Result<(), StdError> {
        use crate::database::migration::get_migrations;

        let query = {
            // leverage the expanded parameter from this provider which is unit
            // tested on its own above.
            let update_provider = UpdateEpochSettingProvider::new(connection);
            let (sql_values, _) = update_provider
                .get_update_condition(Epoch(1), ProtocolParameters::new(1, 2, 1.0))
                .expand();
            for migration in get_migrations() {
                connection.execute(&migration.alterations)?;
            }

            format!("insert into epoch_setting {sql_values}")
        };

        let epoch_settings: &[(i64, ProtocolParameters); 3] = &[
            (1, ProtocolParameters::new(1, 2, 1.0)),
            (2, ProtocolParameters::new(2, 3, 1.0)),
            (3, ProtocolParameters::new(3, 4, 1.0)),
        ];
        for (epoch, protocol_parameters) in epoch_settings {
            let mut statement = connection.prepare(&query)?;

            statement.bind(1, *epoch).unwrap();
            statement
                .bind(
                    2,
                    serde_json::to_string(protocol_parameters).unwrap().as_str(),
                )
                .unwrap();
            statement.next().unwrap();
        }

        Ok(())
    }

    #[test]
    fn projection() {
        let projection = EpochSettingRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:epoch_setting:}", "es")]);

        assert_eq!(
            "es.epoch_setting_id as epoch_setting_id, es.protocol_parameters as protocol_parameters".to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn get_epoch_setting_by_epoch() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = EpochSettingProvider::new(&connection);
        let condition = provider.condition_by_epoch(&Epoch(17)).unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("epoch_setting_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::Integer(17)], values);
    }

    #[test]
    fn update_epoch_setting() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = UpdateEpochSettingProvider::new(&connection);
        let condition = provider.get_update_condition(Epoch(1), ProtocolParameters::new(1, 2, 1.0));
        let (values, params) = condition.expand();

        assert_eq!(
            "(epoch_setting_id, protocol_parameters) values (?1, ?2)".to_string(),
            values
        );
        assert_eq!(
            vec![
                Value::Integer(1),
                Value::String(serde_json::to_string(&ProtocolParameters::new(1, 2, 1.0)).unwrap())
            ],
            params
        );
    }

    #[test]
    fn prune() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = DeleteEpochSettingProvider::new(&connection);
        let condition = provider.get_prune_condition(Epoch(5));
        let (condition, params) = condition.expand();

        assert_eq!("epoch_setting_id < ?1".to_string(), condition);
        assert_eq!(vec![Value::Integer(5)], params);
    }

    #[test]
    fn test_get_epoch_settings() {
        let connection = Connection::open(":memory:").unwrap();
        setup_epoch_setting_db(&connection).unwrap();

        let provider = EpochSettingProvider::new(&connection);

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

    #[test]
    fn test_update_epoch_setting() {
        let connection = Connection::open(":memory:").unwrap();
        setup_epoch_setting_db(&connection).unwrap();

        let provider = UpdateEpochSettingProvider::new(&connection);
        let epoch_setting_record = provider
            .persist(Epoch(3), fake_data::protocol_parameters())
            .unwrap();

        assert_eq!(Epoch(3), epoch_setting_record.epoch_setting_id);
        assert_eq!(
            fake_data::protocol_parameters(),
            epoch_setting_record.protocol_parameters
        );

        let provider = EpochSettingProvider::new(&connection);
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

    #[test]
    fn test_prune() {
        let connection = Connection::open(":memory:").unwrap();
        setup_epoch_setting_db(&connection).unwrap();

        let provider = DeleteEpochSettingProvider::new(&connection);
        let cursor = provider.prune(Epoch(2)).unwrap();

        assert_eq!(1, cursor.count());

        let provider = EpochSettingProvider::new(&connection);
        let cursor = provider.get_by_epoch(&Epoch(1)).unwrap();

        assert_eq!(0, cursor.count());

        let cursor = provider.get_by_epoch(&Epoch(2)).unwrap();

        assert_eq!(1, cursor.count());
    }

    #[tokio::test]
    async fn test_store_adapter() {
        let connection = Connection::open(":memory:").unwrap();
        setup_epoch_setting_db(&connection).unwrap();

        let epoch_settings: Vec<(Epoch, ProtocolParameters)> = vec![
            (Epoch(1), ProtocolParameters::new(1, 20, 1.0)),
            (Epoch(2), ProtocolParameters::new(2, 30, 1.0)),
            (Epoch(3), ProtocolParameters::new(3, 40, 1.0)),
            (Epoch(4), ProtocolParameters::new(4, 50, 1.0)),
            (Epoch(5), ProtocolParameters::new(5, 60, 1.0)),
        ];

        let mut epoch_setting_store_adapter =
            EpochSettingStore::new(Arc::new(Mutex::new(connection)));

        for epoch_setting in &epoch_settings {
            assert!(epoch_setting_store_adapter
                .store_record(&epoch_setting.0, &epoch_setting.1)
                .await
                .is_ok());
        }

        for epoch_setting in &epoch_settings {
            assert!(epoch_setting_store_adapter
                .record_exists(&epoch_setting.0)
                .await
                .unwrap());
            assert_eq!(
                Some(epoch_setting.1.to_owned()),
                epoch_setting_store_adapter
                    .get_record(&epoch_setting.0)
                    .await
                    .unwrap()
            );
        }

        assert_eq!(
            epoch_settings,
            epoch_setting_store_adapter
                .get_last_n_records(epoch_settings.len())
                .await
                .unwrap()
                .into_iter()
                .rev()
                .collect::<Vec<(Epoch, ProtocolParameters)>>()
        )
    }
}
