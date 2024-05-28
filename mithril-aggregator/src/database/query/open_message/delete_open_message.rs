use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::OpenMessageRecord;

/// Query to delete old [OpenMessageRecord] from the sqlite database
pub struct DeleteOpenMessageQuery {
    condition: WhereCondition,
}

impl DeleteOpenMessageQuery {
    pub fn below_epoch_threshold(epoch: Epoch) -> Self {
        Self {
            condition: WhereCondition::new(
                "epoch_setting_id < ?*",
                vec![Value::Integer(*epoch as i64)],
            ),
        }
    }
}

impl Query for DeleteOpenMessageQuery {
    type Entity = OpenMessageRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("delete from open_message where {condition} returning {projection}")
    }
}
