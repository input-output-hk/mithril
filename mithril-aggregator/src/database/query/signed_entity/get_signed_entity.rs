use sqlite::Value;

use mithril_common::entities::SignedEntityTypeDiscriminants;
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
    use mithril_common::entities::{CardanoDbBeacon, SignedEntityType};
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::{insert_signed_entities, main_db_connection};

    use super::*;

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
