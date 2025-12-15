use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::query::open_message::conditions;
use crate::database::record::OpenMessageRecord;

/// Query to insert [OpenMessageRecord] in the sqlite database
pub struct InsertOrReplaceOpenMessageQuery {
    condition: WhereCondition,
}

impl InsertOrReplaceOpenMessageQuery {
    pub fn one(record: OpenMessageRecord) -> StdResult<Self> {
        Ok(Self {
            condition: conditions::insert_one(record)?,
        })
    }
}

impl Query for InsertOrReplaceOpenMessageQuery {
    type Entity = OpenMessageRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert or replace into open_message {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test::double::Dummy;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use super::*;
    use crate::database::query::GetOpenMessageQuery;
    use crate::database::test_helper::main_db_connection;

    #[test]
    fn test_insert_one() {
        let connection = main_db_connection().unwrap();
        let record = OpenMessageRecord::dummy();

        connection
            .fetch_first(InsertOrReplaceOpenMessageQuery::one(record.clone()).unwrap())
            .unwrap();
        let records: Vec<OpenMessageRecord> = connection
            .fetch_collect(
                GetOpenMessageQuery::by_epoch_and_signed_entity_type(
                    record.epoch,
                    &record.signed_entity_type,
                )
                .unwrap(),
            )
            .unwrap();

        assert_eq!(1, records.len());
        assert_eq!(record, records[0]);
    }

    #[test]
    fn test_insert_record_for_existing_signed_entity_type_replaces_it() {
        let connection = main_db_connection().unwrap();
        let record = OpenMessageRecord {
            is_expired: false,
            ..OpenMessageRecord::dummy()
        };

        connection
            .fetch_first(InsertOrReplaceOpenMessageQuery::one(record.clone()).unwrap())
            .unwrap();

        let replaced_record = connection
            .fetch_first(
                InsertOrReplaceOpenMessageQuery::one(OpenMessageRecord {
                    is_expired: true,
                    ..record.clone()
                })
                .unwrap(),
            )
            .unwrap();
        let count = connection.fetch(GetOpenMessageQuery::all()).unwrap().count();

        assert_eq!(1, count);
        assert_eq!(Some(true), replaced_record.map(|r| r.is_expired));
    }
}
