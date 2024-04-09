use std::iter::repeat;

use chrono::Utc;
use sqlite::Value;

use mithril_common::entities::{Epoch, PartyId, Stake};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::StakePool;

/// Query to insert or replace [StakePool] in the sqlite database
pub(crate) struct InsertOrReplaceStakePoolProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> InsertOrReplaceStakePoolProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_insert_or_replace_condition(
        &self,
        records: Vec<(PartyId, Epoch, Stake)>,
    ) -> WhereCondition {
        let columns = "(stake_pool_id, epoch, stake, created_at)";
        let values_columns: Vec<&str> = repeat("(?*, ?*, ?*, ?*)").take(records.len()).collect();
        let values = records
            .into_iter()
            .flat_map(|(stake_pool_id, epoch, stake)| {
                vec![
                    Value::String(stake_pool_id),
                    Value::Integer(epoch.try_into().unwrap()),
                    Value::Integer(i64::try_from(stake).unwrap()),
                    Value::String(Utc::now().to_rfc3339()),
                ]
            })
            .collect();

        WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values,
        )
    }

    pub(crate) fn persist_many(
        &self,
        records: Vec<(PartyId, Epoch, Stake)>,
    ) -> StdResult<Vec<StakePool>> {
        let filters = self.get_insert_or_replace_condition(records);

        Ok(self.find(filters)?.collect())
    }
}

impl<'conn> Provider<'conn> for InsertOrReplaceStakePoolProvider<'conn> {
    type Entity = StakePool;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:stake_pool:}", "stake_pool")]));

        format!("insert or replace into stake_pool {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use sqlite::Connection;

    use crate::database::provider::GetStakePoolProvider;
    use crate::database::test_helper::{apply_all_migrations_to_db, insert_stake_pool};

    use super::*;

    #[test]
    fn test_update_stakes() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        insert_stake_pool(&connection, &[3]).unwrap();

        let provider = InsertOrReplaceStakePoolProvider::new(&connection);
        let pools = provider
            .persist_many(vec![("pool4".to_string(), Epoch(3), 9999)])
            .unwrap();
        let stake_pool = pools.first().unwrap();

        assert_eq!("pool4".to_string(), stake_pool.stake_pool_id);
        assert_eq!(Epoch(3), stake_pool.epoch);
        assert_eq!(9999, stake_pool.stake);

        let provider = GetStakePoolProvider::new(&connection);
        let mut cursor = provider.get_by_epoch(&Epoch(3)).unwrap();
        let stake_pool = cursor.next().expect("Should have a stake pool 'pool4'.");

        assert_eq!("pool4".to_string(), stake_pool.stake_pool_id);
        assert_eq!(Epoch(3), stake_pool.epoch);
        assert_eq!(9999, stake_pool.stake);
        assert_eq!(3, cursor.count());
    }
}
