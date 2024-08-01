use sqlite::Value;

use mithril_common::entities::{Epoch, SignedEntityTypeDiscriminants};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

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

    pub fn cardano_stake_distribution_by_epoch(epoch: Epoch) -> Self {
        let signed_entity_type_id =
            SignedEntityTypeDiscriminants::CardanoStakeDistribution.index() as i64;
        let epoch = *epoch as i64;

        Self {
            condition: WhereCondition::new(
                "signed_entity_type_id = ?* and beacon = ?*",
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
    use chrono::DateTime;
    use mithril_common::{
        entities::{CardanoDbBeacon, SignedEntityType},
        test_utils::fake_data,
    };
    use mithril_persistence::sqlite::ConnectionExtensions;
    use sqlite::ConnectionThreadSafe;

    use crate::database::test_helper::{insert_signed_entities, main_db_connection};

    use super::*;

    fn create_database_with_cardano_stake_distributions<T: Into<SignedEntityRecord>>(
        cardano_stake_distributions: Vec<T>,
    ) -> (ConnectionThreadSafe, Vec<SignedEntityRecord>) {
        let records = cardano_stake_distributions
            .into_iter()
            .map(|cardano_stake_distribution| cardano_stake_distribution.into())
            .collect::<Vec<_>>();

        let connection = create_database(&records);

        (connection, records)
    }

    fn create_database(records: &[SignedEntityRecord]) -> ConnectionThreadSafe {
        let connection = main_db_connection().unwrap();
        insert_signed_entities(&connection, records.to_vec()).unwrap();
        connection
    }

    #[test]
    fn cardano_stake_distribution_by_epoch_returns_records_filtered_by_epoch() {
        let mut cardano_stake_distributions = fake_data::cardano_stake_distributions(3);
        cardano_stake_distributions[0].epoch = Epoch(3);
        cardano_stake_distributions[1].epoch = Epoch(4);
        cardano_stake_distributions[2].epoch = Epoch(5);

        let (connection, records) =
            create_database_with_cardano_stake_distributions(cardano_stake_distributions);

        let records_retrieved: Vec<SignedEntityRecord> = connection
            .fetch_collect(
                GetSignedEntityRecordQuery::cardano_stake_distribution_by_epoch(Epoch(4)),
            )
            .unwrap();

        assert_eq!(vec![records[1].clone()], records_retrieved);
    }

    #[test]
    fn cardano_stake_distribution_by_epoch_returns_records_returns_only_cardano_stake_distribution_records(
    ) {
        let cardano_stake_distributions_record: SignedEntityRecord = {
            let mut cardano_stake_distribution = fake_data::cardano_stake_distribution(Epoch(4));
            cardano_stake_distribution.hash = "hash-123".to_string();
            cardano_stake_distribution.into()
        };

        let snapshots_record = {
            let mut snapshot = fake_data::snapshots(1)[0].clone();
            snapshot.beacon.epoch = Epoch(4);
            SignedEntityRecord::from_snapshot(snapshot, "whatever".to_string(), DateTime::default())
        };

        let mithril_stake_distribution_record: SignedEntityRecord = {
            let mithril_stake_distributions = fake_data::mithril_stake_distributions(1);
            let mut mithril_stake_distribution = mithril_stake_distributions[0].clone();
            mithril_stake_distribution.epoch = Epoch(4);
            mithril_stake_distribution.into()
        };

        let connection = create_database(&[
            cardano_stake_distributions_record.clone(),
            snapshots_record,
            mithril_stake_distribution_record,
        ]);

        let records_retrieved: Vec<SignedEntityRecord> = connection
            .fetch_collect(
                GetSignedEntityRecordQuery::cardano_stake_distribution_by_epoch(Epoch(4)),
            )
            .unwrap();

        assert_eq!(
            vec![cardano_stake_distributions_record.clone()],
            records_retrieved,
        );
    }

    #[test]
    fn test_get_signed_entity_records() {
        let signed_entity_records = SignedEntityRecord::fake_records(5);

        let connection = main_db_connection().unwrap();
        insert_signed_entities(&connection, signed_entity_records.clone()).unwrap();

        let first_signed_entity_type = signed_entity_records.first().unwrap().to_owned();
        let signed_entity_records: Vec<SignedEntityRecord> = connection
            .fetch_collect(GetSignedEntityRecordQuery::by_signed_entity_id(
                &first_signed_entity_type.signed_entity_id,
            ))
            .unwrap();
        assert_eq!(vec![first_signed_entity_type], signed_entity_records);

        let signed_entity_records: Vec<SignedEntityRecord> = connection
            .fetch_collect(
                GetSignedEntityRecordQuery::by_signed_entity_type(
                    &SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                )
                .unwrap(),
            )
            .unwrap();
        let expected_signed_entity_records: Vec<SignedEntityRecord> = signed_entity_records
            .iter()
            .filter_map(|se| {
                (se.signed_entity_type.index()
                    == SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::default())
                        .index())
                .then_some(se.to_owned())
            })
            .collect();
        assert_eq!(expected_signed_entity_records, signed_entity_records);

        let signed_entity_records: Vec<SignedEntityRecord> = connection
            .fetch_collect(GetSignedEntityRecordQuery::all())
            .unwrap();
        let expected_signed_entity_records: Vec<SignedEntityRecord> =
            signed_entity_records.iter().map(|c| c.to_owned()).collect();
        assert_eq!(expected_signed_entity_records, signed_entity_records);
    }
}
