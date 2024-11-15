use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::ProtocolInitializerRecord;

/// Query to delete old [ProtocolInitializer] from the sqlite database
pub struct DeleteProtocolInitializerQuery {
    condition: WhereCondition,
}

impl Query for DeleteProtocolInitializerQuery {
    type Entity = ProtocolInitializerRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
            "{:protocol_initializer:}",
            "protocol_initializer",
        )]));

        format!("delete from protocol_initializer where {condition} returning {projection}")
    }
}

impl DeleteProtocolInitializerQuery {
    /// Create the SQL query to prune data older than the given Epoch.
    pub fn below_epoch_threshold(epoch_threshold: Epoch) -> Self {
        let condition = WhereCondition::new(
            "epoch < ?*",
            vec![Value::Integer(epoch_threshold.try_into().unwrap())],
        );

        Self { condition }
    }
}
