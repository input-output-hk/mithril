use sqlite::Value;

use mithril_common::entities::ImmutableFileName;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, WhereCondition};

use crate::database::record::ImmutableFileDigestRecord;

/// Query to upsert [ImmutableFileDigestRecord] in the sqlite database
pub struct UpsertImmutableFileDigestQuery {
    condition: WhereCondition,
}

impl UpsertImmutableFileDigestQuery {
    pub fn one(immutable_file_name: &ImmutableFileName, digest: &str) -> StdResult<Self> {
        let expression = "(immutable_file_name, digest) values (?*, ?*)";
        let parameters = vec![
            Value::String(immutable_file_name.to_string()),
            Value::String(digest.to_string()),
        ];

        Ok(Self {
            condition: WhereCondition::new(expression, parameters),
        })
    }
}

impl Query for UpsertImmutableFileDigestQuery {
    type Entity = ImmutableFileDigestRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let projection = Self::Entity::expand_projection("immutable_file_digest");

        format!(
            r#"
insert into immutable_file_digest {condition} 
  on conflict (immutable_file_name) do update set digest = excluded.digest
returning {projection}
"#
        )
    }
}
