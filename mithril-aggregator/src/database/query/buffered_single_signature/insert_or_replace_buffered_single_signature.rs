use sqlite::Value;

use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::BufferedSingleSignatureRecord;

/// Query to insert or replace [BufferedSingleSignatureRecord] in the sqlite database
pub struct InsertOrReplaceBufferedSingleSignatureRecordQuery {
    condition: WhereCondition,
}

impl InsertOrReplaceBufferedSingleSignatureRecordQuery {
    pub fn one(record: BufferedSingleSignatureRecord) -> Self {
        let condition =
        WhereCondition::new(
            "(signed_entity_type_id, party_id, lottery_indexes, signature, created_at) values (?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::Integer(record.signed_entity_type_id.index() as i64),
                Value::String(record.party_id),
                Value::String(serde_json::to_string(&record.lottery_indexes).unwrap()),
                Value::String(record.signature),
                Value::String(record.created_at.to_rfc3339()),
            ],
        );

        Self { condition }
    }
}

impl Query for InsertOrReplaceBufferedSingleSignatureRecordQuery {
    type Entity = BufferedSingleSignatureRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
            "{:buffered_single_signature:}",
            "buffered_single_signature",
        )]));

        format!("insert into buffered_single_signature {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use chrono::{Duration, Utc};
    use mithril_common::entities::SignedEntityTypeDiscriminants::{
        CardanoImmutableFilesFull, CardanoStakeDistribution,
    };
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::query::GetBufferedSingleSignatureQuery;
    use crate::database::test_helper::main_db_connection;

    use super::*;

    #[test]
    fn insert_records_in_empty_db() {
        let connection = main_db_connection().unwrap();

        let record = BufferedSingleSignatureRecord::fake("party_8", CardanoImmutableFilesFull);
        let inserted_record = connection
            .fetch_first(InsertOrReplaceBufferedSingleSignatureRecordQuery::one(
                record.clone(),
            ))
            .unwrap();

        assert_eq!(Some(record), inserted_record);
    }

    #[test]
    #[should_panic]
    fn cant_insert_two_record_sharing_the_same_signature() {
        let connection = main_db_connection().unwrap();

        let record = BufferedSingleSignatureRecord::fake("party_8", CardanoImmutableFilesFull);
        let other_record = BufferedSingleSignatureRecord {
            party_id: "party_10".to_string(),
            signed_entity_type_id: CardanoStakeDistribution,
            ..record.clone()
        };

        connection
            .fetch_first(InsertOrReplaceBufferedSingleSignatureRecordQuery::one(
                record,
            ))
            .unwrap();
        connection
            .fetch_first(InsertOrReplaceBufferedSingleSignatureRecordQuery::one(
                other_record,
            ))
            .expect_err(
                "Unique constraint on signature should prevent inserting the same signature twice",
            );
    }

    #[test]
    fn cant_inserted_same_record_twice_should_replace_first_insert() {
        let connection = main_db_connection().unwrap();

        let record = BufferedSingleSignatureRecord::fake("party_8", CardanoImmutableFilesFull);

        connection
            .fetch_first(InsertOrReplaceBufferedSingleSignatureRecordQuery::one(
                record.clone(),
            ))
            .unwrap();
        let inserted_record = connection
            .fetch_first(InsertOrReplaceBufferedSingleSignatureRecordQuery::one(
                record.clone(),
            ))
            .unwrap();

        assert_eq!(Some(record), inserted_record);
    }

    #[test]
    fn inserting_record_with_same_party_id_and_discriminant_replace_previous_record() {
        let connection = main_db_connection().unwrap();

        let record = BufferedSingleSignatureRecord {
            party_id: "party_15".to_string(),
            signed_entity_type_id: CardanoStakeDistribution,
            lottery_indexes: vec![1, 2, 3],
            signature: "a signature".to_string(),
            created_at: Utc::now(),
        };
        connection
            .fetch_first(InsertOrReplaceBufferedSingleSignatureRecordQuery::one(
                record.clone(),
            ))
            .unwrap();
        let count = connection
            .fetch(GetBufferedSingleSignatureQuery::all())
            .unwrap()
            .count();

        assert_eq!(1, count);

        let updated_record = BufferedSingleSignatureRecord {
            party_id: record.party_id.clone(),
            signed_entity_type_id: record.signed_entity_type_id,
            lottery_indexes: vec![7, 8, 9],
            signature: "another signature".to_string(),
            created_at: Utc::now() + Duration::minutes(18),
        };
        let replaced_record = connection
            .fetch_first(InsertOrReplaceBufferedSingleSignatureRecordQuery::one(
                updated_record.clone(),
            ))
            .unwrap();
        let count = connection
            .fetch(GetBufferedSingleSignatureQuery::all())
            .unwrap()
            .count();

        assert_eq!(Some(updated_record), replaced_record);
        assert_eq!(1, count);
    }
}
