use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::ProtocolInitializerRecord;

/// Simple queries to retrieve [ProtocolInitializer] from the sqlite database.
pub struct GetProtocolInitializerQuery {
    condition: WhereCondition,
    limit: Option<usize>,
}

impl GetProtocolInitializerQuery {
    /// Get all signed beacons that match the given signed entity types.
    pub fn for_epoch(epoch: Epoch) -> Self {
        let epoch_i64: i64 = epoch.try_into().unwrap();
        let condition = WhereCondition::new(
            "protocol_initializer.epoch = ?",
            vec![Value::Integer(epoch_i64)],
        );

        Self {
            condition,
            limit: None,
        }
    }

    pub fn last_n(limit: usize) -> Self {
        let condition = WhereCondition::default();
        Self {
            condition,
            limit: Some(limit),
        }
    }
}

impl Query for GetProtocolInitializerQuery {
    type Entity = ProtocolInitializerRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:protocol_initializer:}", "protocol_initializer")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        let limit = self
            .limit
            .map_or("".to_string(), |limit| format!(" limit {}", limit));
        format!("select {projection} from protocol_initializer where {condition} order by rowid desc{limit}")
    }
}
