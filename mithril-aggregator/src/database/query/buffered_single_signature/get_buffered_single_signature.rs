use sqlite::Value;

use mithril_common::entities::SignedEntityTypeDiscriminants;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::BufferedSingleSignatureRecord;

/// Simple queries to retrieve [BufferedSingleSignatureRecord] from the sqlite database.
pub struct GetBufferedSingleSignatureQuery {
    condition: WhereCondition,
}

impl GetBufferedSingleSignatureQuery {
    #[cfg(test)]
    pub(crate) fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }

    pub fn by_discriminant(
        signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
    ) -> Self {
        Self {
            condition: WhereCondition::new(
                "signed_entity_type_id = ?*",
                vec![Value::Integer(
                    signed_entity_type_discriminants.index() as i64
                )],
            ),
        }
    }
}

impl Query for GetBufferedSingleSignatureQuery {
    type Entity = BufferedSingleSignatureRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:buffered_single_signature:}", "b")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from buffered_single_signature as b where {condition} order by ROWID desc")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::SignedEntityTypeDiscriminants::{
        CardanoImmutableFilesFull, CardanoTransactions, MithrilStakeDistribution,
    };
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::{insert_buffered_single_signatures, main_db_connection};

    use super::*;

    #[test]
    fn test_get_all() {
        let connection = main_db_connection().unwrap();
        let records = BufferedSingleSignatureRecord::fakes(&[
            ("party1", MithrilStakeDistribution),
            ("party2", CardanoTransactions),
            ("party3", MithrilStakeDistribution),
        ]);
        insert_buffered_single_signatures(&connection, records.clone()).unwrap();

        let stored_records: Vec<BufferedSingleSignatureRecord> = connection
            .fetch_collect(GetBufferedSingleSignatureQuery::all())
            .unwrap();

        assert_eq!(
            records.into_iter().rev().collect::<Vec<_>>(),
            stored_records
        );
    }

    #[test]
    fn test_get_buffered_single_signature_records_by_discriminant() {
        let connection = main_db_connection().unwrap();
        let msd_records = BufferedSingleSignatureRecord::fakes(&[
            ("party1", MithrilStakeDistribution),
            ("party2", MithrilStakeDistribution),
        ]);
        let ctx_records = BufferedSingleSignatureRecord::fakes(&[("party3", CardanoTransactions)]);
        insert_buffered_single_signatures(
            &connection,
            [msd_records.clone(), ctx_records.clone()].concat(),
        )
        .unwrap();

        let stored_msd_records: Vec<BufferedSingleSignatureRecord> = connection
            .fetch_collect(GetBufferedSingleSignatureQuery::by_discriminant(
                MithrilStakeDistribution,
            ))
            .unwrap();
        assert_eq!(
            msd_records.into_iter().rev().collect::<Vec<_>>(),
            stored_msd_records
        );

        let stored_ctx_records: Vec<BufferedSingleSignatureRecord> = connection
            .fetch_collect(GetBufferedSingleSignatureQuery::by_discriminant(
                CardanoTransactions,
            ))
            .unwrap();
        assert_eq!(
            ctx_records.into_iter().rev().collect::<Vec<_>>(),
            stored_ctx_records
        );

        let cursor = connection
            .fetch(GetBufferedSingleSignatureQuery::by_discriminant(
                CardanoImmutableFilesFull,
            ))
            .unwrap();
        assert_eq!(0, cursor.count());
    }
}
