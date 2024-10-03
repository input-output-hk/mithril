use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::SignedBeaconRecord;

/// Query to insert or replace [SignedBeaconRecord] in the sqlite database
pub struct InsertSignedBeaconRecordQuery {
    condition: WhereCondition,
}

impl InsertSignedBeaconRecordQuery {
    pub fn one(record: SignedBeaconRecord) -> StdResult<Self> {
        let condition =
        WhereCondition::new(
            "(epoch, beacon, signed_entity_type_id, initiated_at, signed_at) values (?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::Integer(record.epoch.try_into()?),
                Value::String(record.signed_entity_type.get_json_beacon()?),
                Value::Integer(record.signed_entity_type.index() as i64),
                Value::String(record.initiated_at.to_rfc3339()),
                Value::String(record.signed_at.to_rfc3339()),
            ],
        );

        Ok(Self { condition })
    }
}

impl Query for InsertSignedBeaconRecordQuery {
    type Entity = SignedBeaconRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:signed_beacon:}", "signed_beacon")]));

        format!("insert into signed_beacon {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{Epoch, SignedEntityType};
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::main_db_connection;

    use super::*;

    #[test]
    fn insert_records_in_empty_db() {
        let connection = main_db_connection().unwrap();

        let record = SignedBeaconRecord::fake(
            Epoch(5),
            SignedEntityType::CardanoStakeDistribution(Epoch(6)),
        );
        let inserted_record = connection
            .fetch_first(InsertSignedBeaconRecordQuery::one(record.clone()).unwrap())
            .unwrap();

        assert_eq!(Some(record), inserted_record);
    }

    #[test]
    #[should_panic]
    fn inserting_same_record_twice_should_fail() {
        let connection = main_db_connection().unwrap();

        let record = SignedBeaconRecord::fake(
            Epoch(13),
            SignedEntityType::CardanoStakeDistribution(Epoch(17)),
        );

        connection
            .fetch_first(InsertSignedBeaconRecordQuery::one(record.clone()).unwrap())
            .unwrap();
        let _ = connection.fetch_first(InsertSignedBeaconRecordQuery::one(record.clone()).unwrap());
    }
}
