use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::SignerRecord;

/// Simple queries to retrieve [SignerRecord] from the sqlite database.
pub struct GetSignerRecordQuery {
    condition: WhereCondition,
}

impl GetSignerRecordQuery {
    pub fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }
}

#[cfg(test)]
mod test_extensions {
    use mithril_persistence::sqlite::WhereCondition;

    use super::*;

    impl GetSignerRecordQuery {
        /// Query to get SignerRecords for a given signer id.
        pub fn by_signer_id(signer_id: String) -> Self {
            Self {
                condition: WhereCondition::new(
                    "signer_id = ?*",
                    vec![sqlite::Value::String(signer_id)],
                ),
            }
        }
    }
}

impl Query for GetSignerRecordQuery {
    type Entity = SignerRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:signer:}", "s")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from signer as s where {condition} order by ROWID desc")
    }
}

#[cfg(test)]
mod tests {
    use crate::database::test_helper::{insert_signers, main_db_connection};
    use mithril_persistence::sqlite::ConnectionExtensions;

    use super::*;

    #[test]
    fn test_get_signer_records_by_id() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = main_db_connection().unwrap();
        insert_signers(&connection, signer_records_fake.clone()).unwrap();

        let signer_records: Vec<SignerRecord> = connection
            .fetch_collect(GetSignerRecordQuery::by_signer_id(
                signer_records_fake[0].signer_id.to_owned(),
            ))
            .unwrap();
        let expected_signer_records: Vec<SignerRecord> = vec![signer_records_fake[0].to_owned()];
        assert_eq!(expected_signer_records, signer_records);

        let signer_records: Vec<SignerRecord> = connection
            .fetch_collect(GetSignerRecordQuery::by_signer_id(
                signer_records_fake[2].signer_id.to_owned(),
            ))
            .unwrap();
        let expected_signer_records: Vec<SignerRecord> = vec![signer_records_fake[2].to_owned()];
        assert_eq!(expected_signer_records, signer_records);

        let cursor = connection
            .fetch(GetSignerRecordQuery::by_signer_id(
                "signer-id-not-registered".to_string(),
            ))
            .unwrap();
        assert_eq!(0, cursor.count());
    }

    #[test]
    fn test_get_all_signer_records() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = main_db_connection().unwrap();
        insert_signers(&connection, signer_records_fake.clone()).unwrap();

        let signer_records: Vec<SignerRecord> = connection
            .fetch_collect(GetSignerRecordQuery::all())
            .unwrap();
        let expected_signer_records: Vec<SignerRecord> =
            signer_records_fake.into_iter().rev().collect();
        assert_eq!(expected_signer_records, signer_records);
    }
}
