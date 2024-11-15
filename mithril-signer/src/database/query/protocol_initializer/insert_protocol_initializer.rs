use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::ProtocolInitializerRecord;

/// Query to insert or replace [ProtocolInitializerRecord] in the sqlite database
pub struct InsertOrReplaceProtocolInitializerQuery {
    condition: WhereCondition,
}

impl InsertOrReplaceProtocolInitializerQuery {
    pub fn one(record: ProtocolInitializerRecord) -> StdResult<Self> {
        let value = serde_json::to_string(&record.protocol_initializer).unwrap();
        let condition = WhereCondition::new(
            "(epoch, protocol, created_at) values (?*, ?*, ?*)",
            vec![
                Value::Integer(record.epoch.try_into()?),
                Value::String(value),
                Value::String(record.created_at.to_rfc3339()),
            ],
        );

        Ok(Self { condition })
    }
}

impl Query for InsertOrReplaceProtocolInitializerQuery {
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

        format!("insert or replace into protocol_initializer {condition} returning {projection}")
    }
}
