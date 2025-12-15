use mithril_common::StdResult;
use mithril_common::entities::{Epoch, SignedEntityTypeDiscriminants};
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};
use sqlite::Value;

use crate::database::record::SignedEntityRecord;

/// Simple queries to retrieve [SignedEntityRecord] from the sqlite database.
pub struct GetSignedEntityRecordQuery {
    condition: WhereCondition,
}

impl GetSignedEntityRecordQuery {
    #[cfg(test)]
    pub fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }

    pub fn by_signed_entity_id(signed_entity_id: &str) -> Self {
        Self {
            condition: WhereCondition::new(
                "signed_entity_id = ?*",
                vec![Value::String(signed_entity_id.to_owned())],
            ),
        }
    }

    pub fn by_certificate_id(certificate_id: &str) -> Self {
        Self {
            condition: WhereCondition::new(
                "certificate_id = ?*",
                vec![Value::String(certificate_id.to_owned())],
            ),
        }
    }

    pub fn by_certificates_ids(certificates_ids: &[&str]) -> Self {
        let ids_values = certificates_ids
            .iter()
            .map(|id| Value::String(id.to_string()))
            .collect();

        Self {
            condition: WhereCondition::where_in("certificate_id", ids_values),
        }
    }

    pub fn by_signed_entity_type(
        signed_entity_type: &SignedEntityTypeDiscriminants,
    ) -> StdResult<Self> {
        let signed_entity_type_id: i64 = signed_entity_type.index() as i64;

        Ok(Self {
            condition: WhereCondition::new(
                "signed_entity_type_id = ?*",
                vec![Value::Integer(signed_entity_type_id)],
            ),
        })
    }

    pub fn by_signed_entity_type_and_epoch(
        signed_entity_type: &SignedEntityTypeDiscriminants,
        epoch: Epoch,
    ) -> Self {
        let signed_entity_type_id = signed_entity_type.index() as i64;
        let epoch = *epoch as i64;

        Self {
            condition: WhereCondition::new(
                "signed_entity_type_id = ?* and epoch = ?*",
                vec![Value::Integer(signed_entity_type_id), Value::Integer(epoch)],
            ),
        }
    }
}

impl Query for GetSignedEntityRecordQuery {
    type Entity = SignedEntityRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:signed_entity:}", "se")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!(
            "select {projection} from signed_entity as se where {condition} order by ROWID desc"
        )
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{BlockNumber, CardanoDbBeacon, SignedEntityType};
    use mithril_persistence::sqlite::ConnectionExtensions;
    use sqlite::ConnectionThreadSafe;

    use super::*;
    use crate::database::test_helper::{insert_signed_entities, main_db_connection};

    fn create_database(records: &[SignedEntityRecord]) -> ConnectionThreadSafe {
        let connection = main_db_connection().unwrap();
        insert_signed_entities(&connection, records.to_vec()).unwrap();

        connection
    }

    #[test]
    fn by_signed_entity_and_epoch_returns_records_filtered_by_epoch() {
        let records = vec![
            SignedEntityRecord::fake_with_signed_entity(
                SignedEntityType::CardanoStakeDistribution(Epoch(3)),
            ),
            SignedEntityRecord::fake_with_signed_entity(
                SignedEntityType::CardanoStakeDistribution(Epoch(4)),
            ),
            SignedEntityRecord::fake_with_signed_entity(
                SignedEntityType::CardanoStakeDistribution(Epoch(5)),
            ),
        ];

        let connection = create_database(&records);

        let records_retrieved: Vec<SignedEntityRecord> = connection
            .fetch_collect(GetSignedEntityRecordQuery::by_signed_entity_type_and_epoch(
                &SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                Epoch(4),
            ))
            .unwrap();

        assert_eq!(vec![records[1].clone()], records_retrieved);
    }

    #[test]
    fn by_signed_entity_and_epoch_returns_records_filtered_by_discriminant() {
        let records = vec![
            SignedEntityRecord::fake_with_signed_entity(
                SignedEntityType::CardanoStakeDistribution(Epoch(3)),
            ),
            SignedEntityRecord::fake_with_signed_entity(
                SignedEntityType::MithrilStakeDistribution(Epoch(3)),
            ),
            SignedEntityRecord::fake_with_signed_entity(SignedEntityType::CardanoDatabase(
                CardanoDbBeacon::new(3, 98),
            )),
        ];

        let connection = create_database(&records);

        let fetched_msd_records: Vec<SignedEntityRecord> = connection
            .fetch_collect(GetSignedEntityRecordQuery::by_signed_entity_type_and_epoch(
                &SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                Epoch(3),
            ))
            .unwrap();
        assert_eq!(vec![records[1].clone()], fetched_msd_records);

        let fetched_cdb_records: Vec<SignedEntityRecord> = connection
            .fetch_collect(GetSignedEntityRecordQuery::by_signed_entity_type_and_epoch(
                &SignedEntityTypeDiscriminants::CardanoDatabase,
                Epoch(3),
            ))
            .unwrap();
        assert_eq!(vec![records[2].clone()], fetched_cdb_records);
    }

    #[test]
    fn test_get_record_by_id() {
        let signed_entity_records = vec![
            SignedEntityRecord::fake_with_signed_entity(
                SignedEntityType::CardanoStakeDistribution(Epoch(3)),
            ),
            SignedEntityRecord::fake_with_signed_entity(SignedEntityType::CardanoTransactions(
                Epoch(4),
                BlockNumber(5),
            )),
        ];

        let connection = main_db_connection().unwrap();
        insert_signed_entities(&connection, signed_entity_records.clone()).unwrap();

        let first_signed_entity_type = signed_entity_records[0].clone();
        let fetched_record = connection
            .fetch_first(GetSignedEntityRecordQuery::by_signed_entity_id(
                &first_signed_entity_type.signed_entity_id,
            ))
            .unwrap();
        assert_eq!(Some(first_signed_entity_type), fetched_record);
    }

    #[test]
    fn test_get_record_by_signed_entity_type() {
        let signed_entity_records = vec![
            SignedEntityRecord::fake_with_signed_entity(
                SignedEntityType::MithrilStakeDistribution(Epoch(2)),
            ),
            SignedEntityRecord::fake_with_signed_entity(SignedEntityType::CardanoTransactions(
                Epoch(4),
                BlockNumber(5),
            )),
            SignedEntityRecord::fake_with_signed_entity(SignedEntityType::CardanoTransactions(
                Epoch(5),
                BlockNumber(9),
            )),
        ];

        let connection = main_db_connection().unwrap();
        insert_signed_entities(&connection, signed_entity_records.clone()).unwrap();

        let fetched_tx_records: Vec<SignedEntityRecord> = connection
            .fetch_collect(
                GetSignedEntityRecordQuery::by_signed_entity_type(
                    &SignedEntityTypeDiscriminants::CardanoTransactions,
                )
                .unwrap(),
            )
            .unwrap();
        let expected_tx_records: Vec<SignedEntityRecord> =
            vec![signed_entity_records[2].clone(), signed_entity_records[1].clone()];
        assert_eq!(expected_tx_records, fetched_tx_records);
    }

    #[test]
    fn test_get_all_records() {
        let signed_entity_records = SignedEntityRecord::fake_records(5);

        let connection = main_db_connection().unwrap();
        insert_signed_entities(&connection, signed_entity_records.clone()).unwrap();

        let fetched_records: Vec<SignedEntityRecord> =
            connection.fetch_collect(GetSignedEntityRecordQuery::all()).unwrap();
        let expected_signed_entity_records: Vec<_> =
            signed_entity_records.into_iter().rev().collect();
        assert_eq!(expected_signed_entity_records, fetched_records);
    }
}
