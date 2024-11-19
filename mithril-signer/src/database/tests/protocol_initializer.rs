use std::sync::Arc;

use mithril_common::test_utils::fake_data;
use mithril_common::{crypto_helper::ProtocolInitializer, entities::Epoch};
use mithril_persistence::sqlite::{ConnectionBuilder, ConnectionExtensions, ConnectionOptions};

use crate::database::repository::ProtocolInitializerRepository;
use crate::database::test_helper::{main_db_connection, FakeStoreAdapter};
use crate::services::EpochPruningTask;
use crate::store::ProtocolInitializerStorer;

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
    values: &[(Epoch, ProtocolInitializer)],
    retention_limit: Option<u64>,
) -> ProtocolInitializerRepository {
    let store = ProtocolInitializerRepository::new(
        Arc::new(main_db_connection().unwrap()),
        retention_limit,
    );

    store_protocol_initializers(&store, values).await;

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

mod request {
    use super::*;

    #[tokio::test]
    async fn save_key_in_empty_store_return_none_as_previous_value() {
        let protocol_initializers = setup_protocol_initializers(1);

        let store = init_store(&[], None).await;
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
        let store = init_store(&protocol_initializers[0..1], None).await;

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
        let store = init_store(&setup_protocol_initializers(2), None).await;

        let res = store.get_protocol_initializer(Epoch(0)).await.unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_protocol_initializer_for_existing_epoch() {
        let store = init_store(&setup_protocol_initializers(2), None).await;

        let res = store.get_protocol_initializer(Epoch(1)).await.unwrap();

        assert!(res.is_some());
    }

    #[tokio::test]
    async fn get_last_protocol_initializer_return_last_one_first() {
        let values = setup_protocol_initializers(10);
        let store = init_store(&values, None).await;

        let res = store.get_last_protocol_initializer(3).await.unwrap();

        assert_eq!(3, res.len());
        assert_eq!(values[9].0, res[0].0);
        assert_eq!(values[8].0, res[1].0);
        assert_eq!(values[7].0, res[2].0);
    }

    #[tokio::test]
    async fn get_last_protocol_initializer_return_all_when_too_few_records() {
        let values = setup_protocol_initializers(2);
        let store = init_store(&values, None).await;

        let res = store.get_last_protocol_initializer(3).await.unwrap();

        assert_eq!(2, res.len());
        assert_eq!(values[1].0, res[0].0);
        assert_eq!(values[0].0, res[1].0);
    }
}

mod pruning {
    use super::*;

    async fn get_epochs_in_database(store: &ProtocolInitializerRepository) -> Vec<Epoch> {
        let result = store.get_last_protocol_initializer(10).await.unwrap();
        result.into_iter().map(|(epoch, _)| epoch).collect()
    }

    #[tokio::test]
    async fn prune_epoch_older_than_threshold() {
        const PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD: u64 = 10;

        let nb_epochs = 5;
        let store = init_store(
            &setup_protocol_initializers(nb_epochs),
            Some(PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD),
        )
        .await;

        assert_eq!(
            vec!(Epoch(5), Epoch(4), Epoch(3), Epoch(2), Epoch(1)),
            get_epochs_in_database(&store).await
        );

        let current_epoch = Epoch(4) + PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD;
        store.prune(current_epoch).await.unwrap();

        assert_eq!(
            vec!(Epoch(5), Epoch(4)),
            get_epochs_in_database(&store).await
        );
    }

    #[tokio::test]
    async fn without_threshold_nothing_is_pruned() {
        let nb_epochs = 5;
        let store = init_store(&setup_protocol_initializers(nb_epochs), None).await;

        store.prune(Epoch(100)).await.unwrap();

        let result = store.get_last_protocol_initializer(10).await.unwrap();
        assert_eq!(nb_epochs as usize, result.len());
    }
}

mod migration {
    use super::*;

    #[tokio::test]
    async fn should_migrate_data_from_adapter() {
        let migrations = crate::database::migration::get_migrations();

        // TODO: Do it in test_helper (it is done by build_main_db_connection)
        fn create_connection_builder() -> ConnectionBuilder {
            ConnectionBuilder::open_memory()
                .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
        }
        let connection = Arc::new(create_connection_builder().build().unwrap());
        let protocol_initializer_adapter =
            FakeStoreAdapter::new(connection.clone(), "protocol_initializer");
        // The adapter will create the table.
        protocol_initializer_adapter.create_table();

        assert!(connection
            .prepare("select key_hash from protocol_initializer;")
            .is_ok());

        // Here we can add some data with the old schema.
        let (_, protocol_initializer_to_retrieve) = &setup_protocol_initializers(1)[0];

        assert!(!protocol_initializer_adapter.is_key_hash_exist("HashEpoch5"));

        protocol_initializer_adapter
            .store_record("HashEpoch5", &Epoch(5), protocol_initializer_to_retrieve)
            .unwrap();

        assert!(protocol_initializer_adapter.is_key_hash_exist("HashEpoch5"));

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
