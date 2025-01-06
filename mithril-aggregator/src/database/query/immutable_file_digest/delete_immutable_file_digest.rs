use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::ImmutableFileDigestRecord;

/// Query to delete [ImmutableFileDigestRecord] from the sqlite database
pub struct DeleteImmutableFileDigestQuery {
    condition: WhereCondition,
}

impl DeleteImmutableFileDigestQuery {
    pub fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }
}

impl Query for DeleteImmutableFileDigestQuery {
    type Entity = ImmutableFileDigestRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:immutable_file_digest:}", "immutable_file_digest")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("delete from immutable_file_digest where {condition} returning {projection}")
    }
}
