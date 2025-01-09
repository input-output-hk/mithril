use sqlite::Value;

use mithril_common::{entities::ImmutableFileName, StdResult};
use mithril_persistence::sqlite::{Query, WhereCondition};

use crate::database::record::ImmutableFileDigestRecord;

/// Simple queries to retrieve [ImmutableFileDigestRecord] from the sqlite database.
pub struct GetImmutableFileDigestQuery {
    condition: WhereCondition,
}

impl GetImmutableFileDigestQuery {
    pub fn by_immutable_file_name(immutable_file_name: &ImmutableFileName) -> StdResult<Self> {
        let condition = WhereCondition::new(
            "immutable_file_name = ?*",
            vec![Value::String(immutable_file_name.to_string())],
        );

        Ok(Self { condition })
    }

    pub fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }
}

impl Query for GetImmutableFileDigestQuery {
    type Entity = ImmutableFileDigestRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let projection = Self::Entity::expand_projection("immutable_file_digest");

        format!(
            "select {projection} from immutable_file_digest where {condition} order by rowid desc"
        )
    }
}
