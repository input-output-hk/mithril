use anyhow::Context;
use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::OpenMessageRecord;

/// Query to update [OpenMessageRecord] in the sqlite database
pub struct UpdateOpenMessageProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> UpdateOpenMessageProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    pub fn get_update_condition(
        &self,
        open_message: &OpenMessageRecord,
    ) -> StdResult<WhereCondition> {
        let expression = "epoch_setting_id = ?*, beacon = ?*, \
signed_entity_type_id = ?*, protocol_message = ?*, is_certified = ?*, \
is_expired = ?*, expires_at = ?* where open_message_id = ?*";
        let beacon_str = open_message.signed_entity_type.get_json_beacon()?;
        let parameters = vec![
            Value::Integer(
                open_message
                    .epoch
                    .try_into()
                    .with_context(|| format!("Can not convert epoch: '{}'", open_message.epoch))?,
            ),
            Value::String(beacon_str),
            Value::Integer(open_message.signed_entity_type.index() as i64),
            Value::String(serde_json::to_string(&open_message.protocol_message)?),
            Value::Integer(open_message.is_certified as i64),
            Value::Integer(open_message.is_expired as i64),
            open_message
                .expires_at
                .map(|d| Value::String(d.to_rfc3339()))
                .unwrap_or(Value::Null),
            Value::String(open_message.open_message_id.to_string()),
        ];

        Ok(WhereCondition::new(expression, parameters))
    }
}

impl<'client> Provider<'client> for UpdateOpenMessageProvider<'client> {
    type Entity = OpenMessageRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("update open_message set {condition} returning {projection}")
    }
}
