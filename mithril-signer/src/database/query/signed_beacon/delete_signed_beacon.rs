use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::SignedBeaconRecord;

/// Query to delete old [SignedBeaconRecord] from the sqlite database
pub struct DeleteSignedBeaconRecordQuery {
    condition: WhereCondition,
}

impl Query for DeleteSignedBeaconRecordQuery {
    type Entity = SignedBeaconRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:signed_beacon:}", "signed_beacon")]));

        format!("delete from signed_beacon where {condition} returning {projection}")
    }
}

impl DeleteSignedBeaconRecordQuery {
    /// Create the SQL query to prune data older than the given Epoch.
    pub fn below_epoch_threshold(epoch_threshold: Epoch) -> Self {
        let epoch_threshold = Value::Integer(epoch_threshold.try_into().unwrap());

        Self {
            condition: WhereCondition::new("epoch < ?*", vec![epoch_threshold]),
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{BlockNumber, SignedEntityType};
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::query::GetSignedBeaconQuery;
    use crate::database::test_helper::{insert_signed_beacons, main_db_connection};

    use super::*;

    // Epoch of the signed entities is irrelevant for those tests, only SignedBeacon.epoch matter
    const WHATEVER_EPOCH: Epoch = Epoch(378);

    #[test]
    fn test_delete_nothing_if_nothing_strictly_below_epoch_threshold() {
        let connection = main_db_connection().unwrap();
        insert_signed_beacons(
            &connection,
            SignedBeaconRecord::fakes(&[(
                Epoch(7),
                vec![SignedEntityType::MithrilStakeDistribution(WHATEVER_EPOCH)],
            )]),
        );

        let delete_cursor = connection
            .fetch(DeleteSignedBeaconRecordQuery::below_epoch_threshold(Epoch(
                7,
            )))
            .unwrap();
        assert_eq!(0, delete_cursor.count());

        let get_all_cursor = connection.fetch(GetSignedBeaconQuery::all()).unwrap();
        assert_eq!(1, get_all_cursor.count());
    }

    #[test]
    fn test_delete_below_epoch_threshold() {
        let connection = main_db_connection().unwrap();
        insert_signed_beacons(
            &connection,
            SignedBeaconRecord::fakes(&[
                (
                    Epoch(7),
                    vec![
                        SignedEntityType::MithrilStakeDistribution(WHATEVER_EPOCH),
                        SignedEntityType::CardanoTransactions(WHATEVER_EPOCH, BlockNumber(12)),
                    ],
                ),
                (
                    Epoch(8),
                    vec![
                        SignedEntityType::MithrilStakeDistribution(WHATEVER_EPOCH),
                        SignedEntityType::CardanoStakeDistribution(WHATEVER_EPOCH),
                    ],
                ),
                (
                    Epoch(9),
                    vec![
                        SignedEntityType::MithrilStakeDistribution(WHATEVER_EPOCH),
                        SignedEntityType::CardanoStakeDistribution(WHATEVER_EPOCH),
                        SignedEntityType::CardanoTransactions(WHATEVER_EPOCH, BlockNumber(23)),
                    ],
                ),
            ]),
        );

        let delete_cursor = connection
            .fetch(DeleteSignedBeaconRecordQuery::below_epoch_threshold(Epoch(
                9,
            )))
            .unwrap();
        assert_eq!(4, delete_cursor.count());

        let get_all_cursor = connection.fetch(GetSignedBeaconQuery::all()).unwrap();
        assert_eq!(3, get_all_cursor.count());
    }
}
