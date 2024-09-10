use sqlite::Value;

use mithril_common::entities::{PartyId, SignedEntityTypeDiscriminants};
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::BufferedSingleSignatureRecord;

/// Query to delete old [BufferedSingleSignatureRecord] from the sqlite database
pub struct DeleteBufferedSingleSignatureQuery {
    condition: WhereCondition,
}

impl DeleteBufferedSingleSignatureQuery {
    pub fn by_discriminant_and_party_ids(
        signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
        party_ids: Vec<PartyId>,
    ) -> Self {
        let ids_values = party_ids.into_iter().map(Value::String).collect();

        Self {
            condition: WhereCondition::new(
                "signed_entity_type_id = ?*",
                vec![Value::Integer(
                    signed_entity_type_discriminants.index() as i64
                )],
            )
            .and_where(WhereCondition::where_in("party_id", ids_values)),
        }
    }
}

impl Query for DeleteBufferedSingleSignatureQuery {
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

        format!("delete from buffered_single_signature where {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::SignedEntityTypeDiscriminants::{
        CardanoTransactions, MithrilStakeDistribution,
    };
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::query::GetBufferedSingleSignatureQuery;
    use crate::database::test_helper::{insert_buffered_single_signatures, main_db_connection};

    use super::*;

    #[test]
    fn test_delete_buffered_single_signature_records_by_discriminant_and_party_ids() {
        let connection = main_db_connection().unwrap();
        let records = BufferedSingleSignatureRecord::fakes(&[
            ("party_1", MithrilStakeDistribution),
            ("party_2", MithrilStakeDistribution),
            ("party_3", MithrilStakeDistribution),
            ("party_1", CardanoTransactions),
            ("party_2", CardanoTransactions),
        ]);
        insert_buffered_single_signatures(&connection, records.clone()).unwrap();

        let cursor = connection
            .fetch(
                DeleteBufferedSingleSignatureQuery::by_discriminant_and_party_ids(
                    MithrilStakeDistribution,
                    vec!["party_1".into(), "party_3".into()],
                ),
            )
            .unwrap();
        assert_eq!(2, cursor.count());

        let cursor = connection
            .fetch(GetBufferedSingleSignatureQuery::all())
            .unwrap();
        assert_eq!(3, cursor.count());

        let cursor = connection
            .fetch(
                DeleteBufferedSingleSignatureQuery::by_discriminant_and_party_ids(
                    CardanoTransactions,
                    vec!["party_1".into(), "party_2".into()],
                ),
            )
            .unwrap();
        assert_eq!(2, cursor.count());

        let cursor = connection
            .fetch(GetBufferedSingleSignatureQuery::all())
            .unwrap();
        assert_eq!(1, cursor.count());
    }
}
