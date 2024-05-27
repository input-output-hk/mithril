use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::StakePool;

/// Query to delete old [StakePool] from the sqlite database
pub struct DeleteStakePoolQuery {
    condition: WhereCondition,
}

impl Query for DeleteStakePoolQuery {
    type Entity = StakePool;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:stake_pool:}", "stake_pool")]));

        format!("delete from stake_pool where {condition} returning {projection}")
    }
}

impl DeleteStakePoolQuery {
    /// Create the SQL query to prune data older than the given Epoch.
    pub fn below_epoch_threshold(epoch_threshold: Epoch) -> Self {
        let condition = WhereCondition::new(
            "epoch < ?*",
            vec![Value::Integer(epoch_threshold.try_into().unwrap())],
        );

        Self { condition }
    }
}

#[cfg(test)]
mod tests {
    use crate::database::query::GetStakePoolQuery;
    use crate::database::test_helper::{insert_stake_pool, main_db_connection};
    use mithril_persistence::sqlite::ConnectionExtensions;

    use super::*;

    #[test]
    fn test_prune_below_epoch_threshold() {
        let connection = main_db_connection().unwrap();
        insert_stake_pool(&connection, &[1, 2]).unwrap();

        let cursor = connection
            .fetch(DeleteStakePoolQuery::below_epoch_threshold(Epoch(2)))
            .unwrap();
        assert_eq!(3, cursor.count());

        let cursor = connection
            .fetch(GetStakePoolQuery::by_epoch(Epoch(1)).unwrap())
            .unwrap();
        assert_eq!(0, cursor.count());

        let cursor = connection
            .fetch(GetStakePoolQuery::by_epoch(Epoch(2)).unwrap())
            .unwrap();
        assert_eq!(3, cursor.count());
    }
}
