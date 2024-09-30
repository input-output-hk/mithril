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

    #[test]
    fn test_prune_below_epoch_threshold() {
        let connection = main_db_connection().unwrap();
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
                    vec![
                        SignedEntityType::MithrilStakeDistribution(Epoch(8)),
                        SignedEntityType::CardanoStakeDistribution(Epoch(8)),
                    ],
                ),
                (
                    Epoch(9),
                    vec![
                        SignedEntityType::MithrilStakeDistribution(Epoch(9)),
                        SignedEntityType::CardanoStakeDistribution(Epoch(9)),
                        SignedEntityType::CardanoTransactions(Epoch(9), BlockNumber(23)),
                    ],
                ),
            ]),
        );

        let cursor = connection
            .fetch(DeleteSignedBeaconRecordQuery::below_epoch_threshold(Epoch(
                7,
            )))
            .unwrap();
        assert_eq!(0, cursor.count());

        let cursor = connection
            .fetch(DeleteSignedBeaconRecordQuery::below_epoch_threshold(Epoch(
                9,
            )))
            .unwrap();
        assert_eq!(4, cursor.count());

        let cursor = connection.fetch(GetSignedBeaconQuery::all()).unwrap();
        assert_eq!(3, cursor.count());
    }
}
