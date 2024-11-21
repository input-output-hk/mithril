use std::sync::Arc;

use mithril_common::entities::{Epoch, StakeDistribution};
use mithril_common::signable_builder::StakeDistributionRetriever;
use mithril_persistence::sqlite::ConnectionBuilder;
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
        let stake_distribution_epoch_100 =
            StakeDistribution::from([("pool-A".to_string(), 1000), ("pool-B".to_string(), 1200)]);
        let stake_distribution_epoch_200 = StakeDistribution::from([
            ("pool-A".to_string(), 2500),
            ("pool-B".to_string(), 2000),
            ("pool-C".to_string(), 2600),
        ]);
        let connection = main_db_connection().unwrap();
        let store = StakePoolStore::new(Arc::new(connection), None);
        store
            .save_stakes(Epoch(100), stake_distribution_epoch_100.clone())
            .await
            .unwrap();
        store
            .save_stakes(Epoch(200), stake_distribution_epoch_200.clone())
            .await
            .unwrap();

        {
            let stake_distribution_in_database = store.retrieve(Epoch(100)).await.unwrap().unwrap();

            assert_eq!(2, stake_distribution_in_database.len());
            assert_eq!(1000, stake_distribution_in_database["pool-A"]);
            assert_eq!(1200, stake_distribution_in_database["pool-B"]);
        }

        {
            let stake_distribution_in_database = store.retrieve(Epoch(200)).await.unwrap().unwrap();

            assert_eq!(3, stake_distribution_in_database.len());
            assert_eq!(2500, stake_distribution_in_database["pool-A"]);
            assert_eq!(2000, stake_distribution_in_database["pool-B"]);
            assert_eq!(2600, stake_distribution_in_database["pool-C"]);
        }
    }

    #[tokio::test]
    async fn save_stake_distribution_return_inserted_records() {
        let epoch = Epoch(100);

        let connection = main_db_connection().unwrap();
        let store = StakePoolStore::new(Arc::new(connection), None);

        {
            let stake_distribution = StakeDistribution::from([
                ("pool-A".to_string(), 1000),
                ("pool-B".to_string(), 1200),
            ]);

            let save_result = store
                .save_stakes(epoch, stake_distribution.clone())
                .await
                .unwrap();

            assert_eq!(stake_distribution, save_result.unwrap());
        }

        {
            let stake_distribution = StakeDistribution::from([
                ("pool-A".to_string(), 2000),
                ("pool-C".to_string(), 2300),
            ]);

            let save_result = store
                .save_stakes(epoch, stake_distribution.clone())
                .await
                .unwrap();

            assert_eq!(stake_distribution, save_result.unwrap());
        }
    }

    #[tokio::test]
    async fn save_stake_distribution_replace_all_stake_for_the_epoch() {
        let epoch = Epoch(100);

        let connection = main_db_connection().unwrap();
        let store = StakePoolStore::new(Arc::new(connection), None);

        {
            let stake_distribution = StakeDistribution::from([
                ("pool-A".to_string(), 1000),
                ("pool-B".to_string(), 1200),
            ]);
            store
                .save_stakes(epoch, stake_distribution.clone())
                .await
                .unwrap();

            let stake_distribution_in_database = store.retrieve(epoch).await.unwrap().unwrap();

            assert_eq!(2, stake_distribution_in_database.len());
            assert_eq!(1000, stake_distribution_in_database["pool-A"]);
            assert_eq!(1200, stake_distribution_in_database["pool-B"]);
        }

        {
            let stake_distribution = StakeDistribution::from([
                ("pool-B".to_string(), 2000),
                ("pool-C".to_string(), 2300),
            ]);
            store
                .save_stakes(epoch, stake_distribution.clone())
                .await
                .unwrap();

            let stake_distribution_in_database = store.retrieve(epoch).await.unwrap().unwrap();

            assert_eq!(2, stake_distribution_in_database.len());
            assert_eq!(2000, stake_distribution_in_database["pool-B"]);
            assert_eq!(2300, stake_distribution_in_database["pool-C"]);
        }
    }

    #[tokio::test]
    async fn save_stake_distribution_do_not_change_other_epoch() {
        let connection = main_db_connection().unwrap();
        let store = StakePoolStore::new(Arc::new(connection), None);

        let stake_distribution_99 = StakeDistribution::from([("pool-A".to_string(), 50)]);
        store
            .save_stakes(Epoch(99), stake_distribution_99.clone())
            .await
            .unwrap();

        let stake_distribution_100 = StakeDistribution::from([("pool-A".to_string(), 1000)]);
        store
            .save_stakes(Epoch(100), stake_distribution_100.clone())
            .await
            .unwrap();

        let stake_distribution_101 = StakeDistribution::from([("pool-A".to_string(), 5000)]);
        store
            .save_stakes(Epoch(101), stake_distribution_101.clone())
            .await
            .unwrap();

        {
            let stake_distribution_100_updated =
                StakeDistribution::from([("pool-A".to_string(), 1111)]);
            store
                .save_stakes(Epoch(100), stake_distribution_100_updated.clone())
                .await
                .unwrap();

            let stake_distribution_in_database = store.retrieve(Epoch(100)).await.unwrap().unwrap();
            assert_eq!(
                stake_distribution_100_updated,
                stake_distribution_in_database
            );

            let stake_distribution_in_database = store.retrieve(Epoch(99)).await.unwrap().unwrap();
            assert_eq!(stake_distribution_99, stake_distribution_in_database);

            let stake_distribution_in_database = store.retrieve(Epoch(101)).await.unwrap().unwrap();
            assert_eq!(stake_distribution_101, stake_distribution_in_database);
        }
    }
}

mod pruning {
    use super::*;

    async fn get_epochs_in_database_until(
        store: &StakePoolStore,
        until_epoch: Epoch,
    ) -> Vec<Epoch> {
        let mut epochs_in_database = vec![];
        let mut current_epoch = Epoch(1);
        while current_epoch <= until_epoch {
            if store.get_stakes(current_epoch).await.unwrap().is_some() {
                epochs_in_database.push(current_epoch);
            }
            current_epoch += 1;
        }
        epochs_in_database
    }

    #[tokio::test]
    async fn prune_epoch_settings_older_than_threshold() {
        const STAKE_POOL_PRUNE_EPOCH_THRESHOLD: u64 = 10;

        let connection = main_db_connection().unwrap();
        insert_stake_pool(&connection, &[1, 2, 3, 4, 5]).unwrap();
        let store =
            StakePoolStore::new(Arc::new(connection), Some(STAKE_POOL_PRUNE_EPOCH_THRESHOLD));

        assert_eq!(
            vec!(Epoch(1), Epoch(2), Epoch(3), Epoch(4), Epoch(5)),
            get_epochs_in_database_until(&store, Epoch(8)).await
        );

        let current_epoch = Epoch(4) + STAKE_POOL_PRUNE_EPOCH_THRESHOLD;
        store.prune(current_epoch).await.unwrap();

        assert_eq!(
            vec!(Epoch(4), Epoch(5)),
            get_epochs_in_database_until(&store, Epoch(8)).await
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

        let connection = Arc::new(ConnectionBuilder::open_memory().build().unwrap());

        let stake_adapter = FakeStoreAdapter::new(connection.clone(), "stake");
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
        ConnectionBuilder::open_memory()
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
