use sqlite::Value;

use mithril_common::entities::SignedEntityType;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::SignedBeaconRecord;

/// Simple queries to retrieve [SignedBeacon] from the sqlite database.
pub struct GetSignedBeaconQuery {
    condition: WhereCondition,
}

impl GetSignedBeaconQuery {
    /// Get all signed beacons.
    pub fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }

    /// Get all signed beacons that match the given signed entity types.
    pub fn by_signed_entities(signed_entity_types: &[SignedEntityType]) -> StdResult<Self> {
        fn signed_entity_condition(entity: &SignedEntityType) -> StdResult<WhereCondition> {
            Ok(WhereCondition::new(
                "signed_entity_type_id = ?* and beacon = ?*",
                vec![
                    Value::Integer(entity.index() as i64),
                    Value::String(entity.get_json_beacon()?),
                ],
            ))
        }

        let condition = match signed_entity_types {
            [] => WhereCondition::new("false", vec![]),
            [first, rest @ ..] => {
                let mut condition = signed_entity_condition(first)?;

                for entity in rest {
                    condition = condition.or_where(signed_entity_condition(entity)?);
                }

                condition
            }
        };

        Ok(Self { condition })
    }
}

impl Query for GetSignedBeaconQuery {
    type Entity = SignedBeaconRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:signed_beacon:}", "b")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from signed_beacon as b where {condition} order by ROWID desc")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{BlockNumber, Epoch};
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::{insert_signed_beacons, main_db_connection};

    use super::*;

    #[test]
    fn test_get_all() {
        let connection = main_db_connection().unwrap();
        let records = SignedBeaconRecord::fakes(&[
            (
                Epoch(3),
                vec![SignedEntityType::MithrilStakeDistribution(Epoch(3))],
            ),
            (
                Epoch(4),
                vec![
                    SignedEntityType::CardanoStakeDistribution(Epoch(4)),
                    SignedEntityType::CardanoTransactions(Epoch(4), BlockNumber(124)),
                ],
            ),
        ]);
        insert_signed_beacons(&connection, records.clone());

        let stored_records: Vec<SignedBeaconRecord> = connection
            .fetch_collect(GetSignedBeaconQuery::all())
            .unwrap();

        assert_eq!(
            records.into_iter().rev().collect::<Vec<_>>(),
            stored_records
        );
    }

    mod get_by_signed_entities {
        use super::*;

        #[test]
        fn with_empty_db() {
            let connection = main_db_connection().unwrap();

            let stored_records: Vec<SignedBeaconRecord> = connection
                .fetch_collect(
                    GetSignedBeaconQuery::by_signed_entities(&[
                        SignedEntityType::MithrilStakeDistribution(Epoch(3)),
                        SignedEntityType::CardanoTransactions(Epoch(4), BlockNumber(124)),
                    ])
                    .unwrap(),
                )
                .unwrap();

            assert_eq!(Vec::<SignedBeaconRecord>::new(), stored_records);
        }

        #[test]
        fn with_empty_list() {
            let connection = main_db_connection().unwrap();
            let records = SignedBeaconRecord::fakes(&[
                (
                    Epoch(3),
                    vec![SignedEntityType::MithrilStakeDistribution(Epoch(3))],
                ),
                (
                    Epoch(4),
                    vec![SignedEntityType::CardanoStakeDistribution(Epoch(4))],
                ),
            ]);
            insert_signed_beacons(&connection, records.clone());

            let stored_records: Vec<SignedBeaconRecord> = connection
                .fetch_collect(GetSignedBeaconQuery::by_signed_entities(&[]).unwrap())
                .unwrap();

            assert_eq!(Vec::<SignedBeaconRecord>::new(), stored_records);
        }

        #[test]
        fn with_one_matching() {
            let connection = main_db_connection().unwrap();
            let records = SignedBeaconRecord::fakes(&[
                (
                    Epoch(3),
                    vec![SignedEntityType::MithrilStakeDistribution(Epoch(3))],
                ),
                (
                    Epoch(4),
                    vec![SignedEntityType::CardanoStakeDistribution(Epoch(4))],
                ),
            ]);
            insert_signed_beacons(&connection, records.clone());

            let stored_records: Vec<SignedBeaconRecord> = connection
                .fetch_collect(
                    GetSignedBeaconQuery::by_signed_entities(&[
                        SignedEntityType::MithrilStakeDistribution(Epoch(3)),
                    ])
                    .unwrap(),
                )
                .unwrap();

            assert_eq!(
                vec![SignedBeaconRecord::fake(
                    Epoch(3),
                    SignedEntityType::MithrilStakeDistribution(Epoch(3))
                )],
                stored_records
            );
        }

        #[test]
        fn with_multiple_matchings_over_several_epochs() {
            let connection = main_db_connection().unwrap();
            let records = SignedBeaconRecord::fakes(&[
                (
                    Epoch(3),
                    vec![
                        SignedEntityType::MithrilStakeDistribution(Epoch(3)),
                        SignedEntityType::CardanoStakeDistribution(Epoch(3)),
                    ],
                ),
                (
                    Epoch(4),
                    vec![
                        SignedEntityType::CardanoStakeDistribution(Epoch(4)),
                        SignedEntityType::MithrilStakeDistribution(Epoch(4)),
                        SignedEntityType::CardanoTransactions(Epoch(4), BlockNumber(109)),
                    ],
                ),
                (
                    Epoch(5),
                    vec![
                        SignedEntityType::CardanoTransactions(Epoch(5), BlockNumber(124)),
                        SignedEntityType::CardanoTransactions(Epoch(5), BlockNumber(133)),
                    ],
                ),
            ]);
            insert_signed_beacons(&connection, records.clone());

            let stored_records: Vec<SignedBeaconRecord> = connection
                .fetch_collect(
                    GetSignedBeaconQuery::by_signed_entities(&[
                        SignedEntityType::MithrilStakeDistribution(Epoch(3)),
                        SignedEntityType::MithrilStakeDistribution(Epoch(4)),
                        SignedEntityType::CardanoTransactions(Epoch(4), BlockNumber(109)),
                        SignedEntityType::CardanoTransactions(Epoch(5), BlockNumber(133)),
                    ])
                    .unwrap(),
                )
                .unwrap();

            assert_eq!(
                SignedBeaconRecord::fakes(&[
                    (
                        Epoch(5),
                        vec![SignedEntityType::CardanoTransactions(
                            Epoch(5),
                            BlockNumber(133)
                        ),],
                    ),
                    (
                        Epoch(4),
                        vec![
                            SignedEntityType::CardanoTransactions(Epoch(4), BlockNumber(109)),
                            SignedEntityType::MithrilStakeDistribution(Epoch(4)),
                        ],
                    ),
                    (
                        Epoch(3),
                        vec![SignedEntityType::MithrilStakeDistribution(Epoch(3)),],
                    ),
                ]),
                stored_records
            );
        }
    }
}
