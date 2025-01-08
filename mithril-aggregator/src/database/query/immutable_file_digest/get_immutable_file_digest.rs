use sqlite::Value;

use mithril_common::{entities::ImmutableFileName, StdResult};
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

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
        let aliases = SourceAlias::new(&[("{:immutable_file_digest:}", "immutable_file_digest")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!(
            "select {projection} from immutable_file_digest where {condition} order by rowid desc"
        )
    }
}