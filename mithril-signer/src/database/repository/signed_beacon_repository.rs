use async_trait::async_trait;
use std::sync::Arc;

use mithril_common::entities::{Epoch, SignedEntityType};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};

use crate::database::query::{
    DeleteSignedBeaconRecordQuery, GetSignedBeaconQuery, InsertSignedBeaconRecordQuery,
};
use crate::database::record::SignedBeaconRecord;
use crate::entities::BeaconToSign;
use crate::services::{EpochPruningTask, SignedBeaconStore};

/// A [SignedBeaconStore] implementation using SQLite.
pub struct SignedBeaconRepository {
    connection: Arc<SqliteConnection>,
}

impl SignedBeaconRepository {
    /// Create a new instance of the `SignedBeaconRepository`.
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }

    /// Get the last signed beacon.
    pub fn get_last(&self) -> StdResult<Option<SignedBeaconRecord>> {
        self.connection.fetch_first(GetSignedBeaconQuery::all())
    }

    /// Prune all signed beacons that have an epoch below the given threshold.
    pub fn prune_below_epoch(&self, epoch: Epoch) -> StdResult<()> {
        let _ = self
            .connection
            .fetch_first(DeleteSignedBeaconRecordQuery::below_epoch_threshold(epoch))?;
        Ok(())
    }
}

#[async_trait]
impl SignedBeaconStore for SignedBeaconRepository {
    async fn filter_out_already_signed_entities(
        &self,
        entities: Vec<SignedEntityType>,
    ) -> StdResult<Vec<SignedEntityType>> {
        let already_signed_entities: Vec<SignedEntityType> = self
            .connection
            .fetch(GetSignedBeaconQuery::by_signed_entities(&entities)?)?
            .map(|record| record.signed_entity_type)
            .collect();

        Ok(entities
            .into_iter()
            .filter(|e| !already_signed_entities.contains(e))
            .collect())
    }

    async fn mark_beacon_as_signed(&self, entity: &BeaconToSign) -> StdResult<()> {
        let record = SignedBeaconRecord {
            epoch: entity.epoch,
            signed_entity_type: entity.signed_entity_type.clone(),
            initiated_at: entity.initiated_at,
            signed_at: chrono::Utc::now(),
        };
        let _ = self
            .connection
            .fetch_first(InsertSignedBeaconRecordQuery::one(record)?)?;

        Ok(())
    }
}

#[async_trait]
impl EpochPruningTask for SignedBeaconRepository {
    fn pruned_data(&self) -> &'static str {
        "Signed Beacon"
    }

    async fn prune_below_epoch_threshold(&self, epoch_threshold: Epoch) -> StdResult<()> {
        self.prune_below_epoch(epoch_threshold)
    }
}

#[cfg(test)]
mod tests {
    use chrono::Utc;

    use mithril_common::entities::{
        BlockNumber, Epoch, SignedEntityConfig, SignedEntityTypeDiscriminants, TimePoint,
    };
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::query::GetSignedBeaconQuery;
    use crate::database::record::SignedBeaconRecord;
    use crate::database::test_helper::{insert_signed_beacons, main_db_connection};

    use super::*;

    fn all_signed_entity_type_for(time_point: &TimePoint) -> Vec<SignedEntityType> {
        let config = SignedEntityConfig {
            allowed_discriminants: SignedEntityTypeDiscriminants::all(),
            ..SignedEntityConfig::dummy()
        };
        config.list_allowed_signed_entity_types(time_point).unwrap()
    }

    #[test]
    fn get_last_stored_signed_beacon() {
        let connection = Arc::new(main_db_connection().unwrap());
        let repository = SignedBeaconRepository::new(connection.clone());

        let last_signed_beacon = repository.get_last().unwrap();
        assert_eq!(None, last_signed_beacon);

        insert_signed_beacons(
            &connection,
            vec![SignedBeaconRecord::fake(
                Epoch(1941),
                SignedEntityType::MithrilStakeDistribution(Epoch(1941)),
            )],
        );

        let last_signed_beacon = repository.get_last().unwrap();
        assert_eq!(
            Some(SignedBeaconRecord::fake(
                Epoch(1941),
                SignedEntityType::MithrilStakeDistribution(Epoch(1941)),
            )),
            last_signed_beacon
        );

        insert_signed_beacons(
            &connection,
            SignedBeaconRecord::fakes(&[
                (
                    Epoch(1942),
                    vec![SignedEntityType::MithrilStakeDistribution(Epoch(1942))],
                ),
                (
                    Epoch(1943),
                    vec![SignedEntityType::MithrilStakeDistribution(Epoch(1943))],
                ),
            ]),
        );

        let last_signed_beacon = repository.get_last().unwrap();
        assert_eq!(
            Some(SignedBeaconRecord::fake(
                Epoch(1943),
                SignedEntityType::MithrilStakeDistribution(Epoch(1943)),
            )),
            last_signed_beacon
        );
    }

    #[tokio::test]
    async fn filter_out_nothing_if_nothing_was_previously_signed() {
        let connection = Arc::new(main_db_connection().unwrap());
        let repository = SignedBeaconRepository::new(connection.clone());

        let to_filter = all_signed_entity_type_for(&TimePoint::dummy());
        let available_entities = repository
            .filter_out_already_signed_entities(to_filter.clone())
            .await
            .unwrap();

        assert_eq!(to_filter, available_entities);
    }

    #[tokio::test]
    async fn filter_out_nothing_if_previously_signed_entities_doesnt_match_passed_entities() {
        let connection = Arc::new(main_db_connection().unwrap());
        let repository = SignedBeaconRepository::new(connection.clone());

        let time_point = TimePoint::dummy();
        insert_signed_beacons(
            &connection,
            SignedBeaconRecord::fakes(&[(
                Epoch(1941),
                vec![SignedEntityType::MithrilStakeDistribution(
                    time_point.epoch - 2,
                )],
            )]),
        );
        let to_filter = all_signed_entity_type_for(&time_point);

        let available_entities = repository
            .filter_out_already_signed_entities(to_filter.clone())
            .await
            .unwrap();
        assert_eq!(to_filter, available_entities);
    }

    #[tokio::test]
    async fn filter_out_everything_if_previously_signed_entities_match_all_passed_entities() {
        let connection = Arc::new(main_db_connection().unwrap());
        let repository = SignedBeaconRepository::new(connection.clone());

        let to_filter = all_signed_entity_type_for(&TimePoint::dummy());
        insert_signed_beacons(
            &connection,
            to_filter
                .iter()
                .map(|entity| SignedBeaconRecord::fake(Epoch(4872), entity.clone()))
                .collect(),
        );

        let available_entities = repository
            .filter_out_already_signed_entities(to_filter.clone())
            .await
            .unwrap();
        assert_eq!(Vec::<SignedEntityType>::new(), available_entities);
    }

    #[tokio::test]
    async fn filter_out_partially_if_some_previously_signed_entities_match_passed_entities() {
        let connection = Arc::new(main_db_connection().unwrap());
        let repository = SignedBeaconRepository::new(connection.clone());

        let time_point = TimePoint::dummy();
        let signed_beacons = [
            SignedEntityType::MithrilStakeDistribution(time_point.epoch),
            SignedEntityType::CardanoTransactions(
                time_point.epoch,
                time_point.chain_point.block_number,
            ),
        ];
        insert_signed_beacons(
            &connection,
            signed_beacons
                .iter()
                .map(|entity| SignedBeaconRecord::fake(Epoch(4872), entity.clone()))
                .collect(),
        );

        let available_entities = repository
            .filter_out_already_signed_entities(vec![
                SignedEntityType::MithrilStakeDistribution(time_point.epoch),
                SignedEntityType::CardanoStakeDistribution(time_point.epoch),
                SignedEntityType::CardanoTransactions(
                    time_point.epoch,
                    time_point.chain_point.block_number,
                ),
                SignedEntityType::CardanoStakeDistribution(time_point.epoch + 10),
            ])
            .await
            .unwrap();

        assert_eq!(
            vec![
                SignedEntityType::CardanoStakeDistribution(time_point.epoch),
                SignedEntityType::CardanoStakeDistribution(time_point.epoch + 10),
            ],
            available_entities
        );
    }

    #[tokio::test]
    async fn mark_beacon_as_signed() {
        let connection = Arc::new(main_db_connection().unwrap());
        let repository = SignedBeaconRepository::new(connection.clone());

        let beacon_to_sign = BeaconToSign {
            epoch: Epoch(13),
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(13)),
            initiated_at: Utc::now(),
        };

        let signed_beacons: Vec<SignedBeaconRecord> = connection
            .fetch_collect(GetSignedBeaconQuery::all())
            .unwrap();
        assert_eq!(Vec::<SignedBeaconRecord>::new(), signed_beacons);

        repository
            .mark_beacon_as_signed(&beacon_to_sign)
            .await
            .unwrap();

        let signed_beacon = connection
            .fetch_first(GetSignedBeaconQuery::all())
            .unwrap()
            .expect("A signed beacon should have been inserted");
        assert_eq!(beacon_to_sign, signed_beacon);
    }

    #[test]
    fn test_prune() {
        let connection = Arc::new(main_db_connection().unwrap());
        let repository = SignedBeaconRepository::new(connection.clone());
        insert_signed_beacons(
            &connection,
            SignedBeaconRecord::fakes(&[
                (
                    Epoch(7),
                    vec![
                        SignedEntityType::MithrilStakeDistribution(Epoch(7)),
                        SignedEntityType::CardanoTransactions(Epoch(7), BlockNumber(12)),
                    ],
                ),
                (
                    Epoch(8),
                    vec![SignedEntityType::MithrilStakeDistribution(Epoch(8))],
                ),
            ]),
        );

        repository.prune_below_epoch(Epoch(8)).unwrap();

        let signed_beacons: Vec<SignedBeaconRecord> = connection
            .fetch_collect(GetSignedBeaconQuery::all())
            .unwrap();
        assert_eq!(
            vec![SignedBeaconRecord::fake(
                Epoch(8),
                SignedEntityType::MithrilStakeDistribution(Epoch(8))
            )],
            signed_beacons
        );
    }
}
