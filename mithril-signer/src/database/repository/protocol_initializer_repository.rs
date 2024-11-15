use std::sync::Arc;

use anyhow::Ok;
use async_trait::async_trait;

use crate::database::query::{
    DeleteProtocolInitializerQuery, InsertOrReplaceProtocolInitializerQuery,
};
use crate::database::record::ProtocolInitializerRecord;
use crate::{
    database::query::GetProtocolInitializerQuery, services::EpochPruningTask,
    store::ProtocolInitializerStorer,
};
use mithril_common::{crypto_helper::ProtocolInitializer, entities::Epoch, StdResult};
use mithril_persistence::sqlite::ConnectionExtensions;
use mithril_persistence::{sqlite::SqliteConnection, store::adapter::StoreAdapter};

/// Implementation of the ProtocolInitializerStorer
pub struct ProtocolInitializerRepository {
    connection: Arc<SqliteConnection>,
    retention_limit: Option<u64>,
}

impl ProtocolInitializerRepository {
    /// Create a new ProtocolInitializerRepository.
    pub fn new(connection: Arc<SqliteConnection>, retention_limit: Option<u64>) -> Self {
        Self {
            connection,
            retention_limit,
        }
    }
}

#[async_trait]
impl EpochPruningTask for ProtocolInitializerRepository {
    fn pruned_data(&self) -> &'static str {
        "Protocol initializer"
    }

    async fn prune(&self, epoch: Epoch) -> StdResult<()> {
        if let Some(threshold) = self.retention_limit {
            self.connection
                .apply(DeleteProtocolInitializerQuery::below_epoch_threshold(
                    epoch - threshold,
                ))?;
        }
        Ok(())
    }
}

#[async_trait]
impl ProtocolInitializerStorer for ProtocolInitializerRepository {
    async fn save_protocol_initializer(
        &self,
        epoch: Epoch,
        protocol_initializer: ProtocolInitializer,
    ) -> StdResult<Option<ProtocolInitializer>> {
        let previous_protocol_initializer = self.get_protocol_initializer(epoch).await?;
        let record = ProtocolInitializerRecord {
            epoch,
            protocol_initializer: protocol_initializer.clone(),
            created_at: chrono::Utc::now(),
        };
        self.connection
            .apply(InsertOrReplaceProtocolInitializerQuery::one(record).unwrap())?;

        Ok(previous_protocol_initializer)
    }

    async fn get_protocol_initializer(
        &self,
        epoch: Epoch,
    ) -> StdResult<Option<ProtocolInitializer>> {
        let record = self
            .connection
            .fetch_first(GetProtocolInitializerQuery::for_epoch(epoch))?;

        Ok(record.map(|record| record.protocol_initializer))
    }

    async fn get_last_protocol_initializer(
        &self,
        last: usize,
    ) -> StdResult<Vec<(Epoch, ProtocolInitializer)>> {
        let record: Vec<ProtocolInitializerRecord> = self
            .connection
            .fetch_collect(GetProtocolInitializerQuery::last_n(last))?;

        Ok(record
            .iter()
            .map(|record| (record.epoch, record.protocol_initializer.to_owned()))
            .collect())
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;
    use mithril_persistence::{
        sqlite::{ConnectionBuilder, ConnectionOptions},
        store::adapter::SQLiteAdapter,
    };

    use crate::database::test_helper::main_db_connection;

    use super::*;

    fn setup_protocol_initializers(nb_epoch: u64) -> Vec<(Epoch, ProtocolInitializer)> {
        let mut values: Vec<(Epoch, ProtocolInitializer)> = Vec::new();
        for epoch in 1..=nb_epoch {
            let stake = (epoch + 1) * 100;
            let protocol_initializer = fake_data::protocol_initializer("1", stake);
            values.push((Epoch(epoch), protocol_initializer));
        }
        values
    }

    async fn init_store(
        nb_epoch: u64,
        retention_limit: Option<u64>,
    ) -> ProtocolInitializerRepository {
        let store = ProtocolInitializerRepository::new(
            Arc::new(main_db_connection().unwrap()),
            retention_limit,
        );

        let values = setup_protocol_initializers(nb_epoch);
        store_protocol_initializers(&store, &values).await;

        store
    }

    async fn store_protocol_initializers(
        store: &ProtocolInitializerRepository,
        values: &[(Epoch, ProtocolInitializer)],
    ) {
        for value in values.iter() {
            store
                .save_protocol_initializer(value.0, value.1.clone())
                .await
                .unwrap();
        }
    }

    #[tokio::test]
    async fn save_key_in_empty_store_return_none_as_previous_value() {
        let protocol_initializers = setup_protocol_initializers(1);

        let store = init_store(0, None).await;
        let res = store
            .save_protocol_initializer(
                protocol_initializers[0].0,
                protocol_initializers[0].1.clone(),
            )
            .await
            .unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn update_protocol_initializer_in_store_return_previous_value() {
        let protocol_initializers = setup_protocol_initializers(2);
        let store = init_store(0, None).await;
        store_protocol_initializers(&store, &protocol_initializers[0..1]).await;

        let res = store
            .save_protocol_initializer(
                protocol_initializers[0].0,
                protocol_initializers[1].1.clone(),
            )
            .await
            .unwrap();

        assert!(res.is_some());
        assert_eq!(
            protocol_initializers[0].1.get_stake(),
            res.unwrap().get_stake()
        );
    }

    #[tokio::test]
    async fn get_protocol_initializer_for_empty_epoch() {
        let store = init_store(2, None).await;
        let res = store.get_protocol_initializer(Epoch(0)).await.unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_protocol_initializer_for_existing_epoch() {
        let store = init_store(2, None).await;
        let res = store.get_protocol_initializer(Epoch(1)).await.unwrap();

        assert!(res.is_some());
    }

    #[tokio::test]
    async fn get_last_protocol_initializer_return_last_one_first() {
        let store = init_store(0, None).await;
        let values = setup_protocol_initializers(10);
        store_protocol_initializers(&store, &values).await;

        let res = store.get_last_protocol_initializer(3).await.unwrap();

        assert_eq!(3, res.len());
        assert_eq!(values[9].0, res[0].0);
        assert_eq!(values[8].0, res[1].0);
        assert_eq!(values[7].0, res[2].0);
    }

    #[tokio::test]
    async fn get_last_protocol_initializer_return_all_when_too_few_records() {
        let store = init_store(0, None).await;
        let values = setup_protocol_initializers(2);
        store_protocol_initializers(&store, &values).await;

        let res = store.get_last_protocol_initializer(3).await.unwrap();

        assert_eq!(2, res.len());
        assert_eq!(values[1].0, res[0].0);
        assert_eq!(values[0].0, res[1].0);
    }

    #[tokio::test]
    async fn prune_epoch_older_than_threshold() {
        const PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD: u64 = 10;

        let store = init_store(0, Some(PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD)).await;
        let values = setup_protocol_initializers(2);
        store_protocol_initializers(&store, &values).await;

        store
            .prune(Epoch(2) + PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD)
            .await
            .unwrap();

        let res = store.get_last_protocol_initializer(10).await.unwrap();

        assert_eq!(1, res.len());
        assert_eq!(Epoch(2), res[0].0);
    }

    #[tokio::test]
    async fn without_threshold_nothing_is_pruned() {
        let store = init_store(0, None).await;
        let values = setup_protocol_initializers(2);
        store_protocol_initializers(&store, &values).await;

        store.prune(Epoch(100)).await.unwrap();

        let res = store.get_last_protocol_initializer(10).await.unwrap();
        assert_eq!(2, res.len());
    }

    #[tokio::test]
    async fn should_migrate_data_from_adapter() {
        let migrations = crate::database::migration::get_migrations();

        // TODO: Do it in test_helper (it is done by build_main_db_connection)
        fn create_connection_builder() -> ConnectionBuilder {
            ConnectionBuilder::open_memory()
                .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
        }
        let connection = Arc::new(create_connection_builder().build().unwrap());

        // The adapter will create the table.
        let mut adapter = SQLiteAdapter::<Epoch, ProtocolInitializer>::new(
            "protocol_initializer",
            connection.clone(),
        )
        .unwrap();

        assert!(connection
            .prepare("select key_hash from protocol_initializer;")
            .is_ok());
        assert!(connection.prepare("select * from db_version;").is_err());

        // Here we can add some data with the old schema.
        let (_, protocol_initializer_to_retrieve) = &setup_protocol_initializers(1)[0];

        // If we don't want to use the adapter anymore, we can execute request directly.
        assert!(adapter.get_record(&Epoch(5)).await.unwrap().is_none());
        adapter
            .store_record(&Epoch(5), &protocol_initializer_to_retrieve)
            .await
            .unwrap();
        assert!(adapter.get_record(&Epoch(5)).await.unwrap().is_some());

        // We finish the migration
        create_connection_builder()
            .apply_migrations(&connection, migrations)
            .unwrap();

        assert!(connection
            .prepare("select key_hash from protocol_initializer;")
            .is_err());
        assert!(connection
            .prepare("select * from protocol_initializer;")
            .is_ok());

        let value: i64 = connection
            .query_single_cell("select count(*) from protocol_initializer", &[])
            .unwrap();
        assert_eq!(value, 1);

        // We can check that data are migrated.
        let store = ProtocolInitializerRepository::new(connection, None);
        let protocol_initializer = store.get_protocol_initializer(Epoch(5)).await.unwrap();

        assert_eq!(
            protocol_initializer.unwrap().get_stake(),
            protocol_initializer_to_retrieve.get_stake()
        );
    }
}
