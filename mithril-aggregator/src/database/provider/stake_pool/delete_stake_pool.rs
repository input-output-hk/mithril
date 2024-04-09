use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::StakePool;

/// Query to delete old [StakePool] from the sqlite database
pub struct DeleteStakePoolProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> Provider<'conn> for DeleteStakePoolProvider<'conn> {
    type Entity = StakePool;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:stake_pool:}", "stake_pool")]));

        format!("delete from stake_pool where {condition} returning {projection}")
    }
}

impl<'conn> DeleteStakePoolProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    /// Create the SQL condition to prune data older than the given Epoch.
    fn get_prune_condition(&self, epoch_threshold: Epoch) -> WhereCondition {
        WhereCondition::new(
            "epoch < ?*",
            vec![Value::Integer(epoch_threshold.try_into().unwrap())],
        )
    }

    /// Prune the stake pools data older than the given epoch.
    pub fn prune(&self, epoch_threshold: Epoch) -> StdResult<EntityCursor<StakePool>> {
        let filters = self.get_prune_condition(epoch_threshold);

        self.find(filters)
    }
}

#[cfg(test)]
mod tests {
    use crate::database::provider::GetStakePoolProvider;
    use crate::database::test_helper::{insert_stake_pool, main_db_connection};

    use super::*;

    #[test]
    fn test_prune() {
        let connection = main_db_connection().unwrap();
        insert_stake_pool(&connection, &[1, 2]).unwrap();

        let provider = DeleteStakePoolProvider::new(&connection);
        let cursor = provider.prune(Epoch(2)).unwrap();

        assert_eq!(3, cursor.count());

        let provider = GetStakePoolProvider::new(&connection);
        let cursor = provider.get_by_epoch(&Epoch(1)).unwrap();

        assert_eq!(0, cursor.count());

        let cursor = provider.get_by_epoch(&Epoch(2)).unwrap();

        assert_eq!(3, cursor.count());
    }
}
