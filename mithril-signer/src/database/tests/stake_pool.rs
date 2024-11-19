use std::sync::Arc;

use mithril_common::entities::{Epoch, StakeDistribution};
use mithril_common::signable_builder::StakeDistributionRetriever;
use mithril_persistence::sqlite::{ConnectionBuilder, ConnectionOptions};
use mithril_persistence::store::StakeStorer;

use crate::database::repository::StakePoolStore;
use crate::database::test_helper::{insert_stake_pool, main_db_connection, FakeStoreAdapter};
use crate::services::EpochPruningTask;

mod request {

    use super::*;

    #[tokio::test]
    async fn retrieve_with_no_stakes_returns_none() {
        let connection = main_db_connection().unwrap();
        let store = StakePoolStore::new(Arc::new(connection), None);

        let result = store.retrieve(Epoch(1)).await.unwrap();

        assert!(result.is_none());
    }

    #[tokio::test]
    async fn retrieve_returns_stake_distribution() {
        let stake_distribution_to_retrieve =
            StakeDistribution::from([("pool-123".to_string(), 123)]);
        let connection = main_db_connection().unwrap();
        let store = StakePoolStore::new(Arc::new(connection), None);
        store
            .save_stakes(Epoch(1), stake_distribution_to_retrieve.clone())
            .await
            .unwrap();

        let stake_distribution = store.retrieve(Epoch(1)).await.unwrap();

        assert_eq!(stake_distribution, Some(stake_distribution_to_retrieve));
    }
}

mod pruning {
    use super::*;

    #[tokio::test]
    async fn prune_epoch_settings_older_than_threshold() {
        let connection = main_db_connection().unwrap();
        const STAKE_POOL_PRUNE_EPOCH_THRESHOLD: u64 = 10;
        insert_stake_pool(&connection, &[1, 2]).unwrap();
        let store =
            StakePoolStore::new(Arc::new(connection), Some(STAKE_POOL_PRUNE_EPOCH_THRESHOLD));

        store
            .prune(Epoch(2) + STAKE_POOL_PRUNE_EPOCH_THRESHOLD)
            .await
            .unwrap();

        let epoch1_stakes = store.get_stakes(Epoch(1)).await.unwrap();
        let epoch2_stakes = store.get_stakes(Epoch(2)).await.unwrap();

        assert_eq!(
            None, epoch1_stakes,
            "Stakes at epoch 1 should have been pruned",
        );
        assert!(
            epoch2_stakes.is_some(),
            "Stakes at epoch 2 should still exist",
        );
    }

    #[tokio::test]
    async fn without_threshold_nothing_is_pruned() {
        let connection = main_db_connection().unwrap();
        insert_stake_pool(&connection, &[1, 2]).unwrap();
        let store = StakePoolStore::new(Arc::new(connection), None);

        store.prune(Epoch(100)).await.unwrap();

        let epoch1_stakes = store.get_stakes(Epoch(1)).await.unwrap();
        let epoch2_stakes = store.get_stakes(Epoch(2)).await.unwrap();

        assert!(
            epoch1_stakes.is_some(),
            "Stakes at epoch 1 should have been pruned",
        );
        assert!(
            epoch2_stakes.is_some(),
            "Stakes at epoch 2 should still exist",
        );
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

        // The adapter will create the table.
        let stake_adapter = FakeStoreAdapter::new(connection.clone(), "stake");
        // The adapter will create the table.
        stake_adapter.create_table();

        assert!(connection.prepare("select * from stake;").is_ok());
        assert!(connection.prepare("select * from db_version;").is_err());
        assert!(connection.prepare("select * from stake_pool;").is_err());

        // Here we can add some data with the old schema.
        let stake_distribution_to_retrieve =
            StakeDistribution::from([("pool-123".to_string(), 123)]);

        // If we don't want to use the adapter anymore, we can execute request directly.
        assert!(!stake_adapter.is_key_hash_exist("HashEpoch5"));
        stake_adapter
            .store_record("HashEpoch5", &Epoch(5), &stake_distribution_to_retrieve)
            .unwrap();
        assert!(stake_adapter.is_key_hash_exist("HashEpoch5"));

        // We finish the migration
        create_connection_builder()
            .apply_migrations(&connection, migrations)
            .unwrap();
        assert!(connection.prepare("select * from stake;").is_err());
        assert!(connection.prepare("select * from stake_pool;").is_ok());

        // We can check that data are migrated.
        let store = StakePoolStore::new(connection, None);
        let stake_distribution = store.retrieve(Epoch(5)).await.unwrap();
        assert_eq!(stake_distribution, Some(stake_distribution_to_retrieve));
    }
}
