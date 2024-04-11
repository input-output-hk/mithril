use chrono::Utc;
use sqlite::Value;
use uuid::Uuid;

use mithril_common::entities::{Epoch, ProtocolMessage, SignedEntityType};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::OpenMessageRecord;

/// Query to insert [OpenMessageRecord] in the sqlite database
pub struct InsertOpenMessageProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> InsertOpenMessageProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    pub fn get_insert_condition(
        &self,
        epoch: Epoch,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<WhereCondition> {
        let expression = "(open_message_id, epoch_setting_id, beacon, signed_entity_type_id, protocol_message, expires_at, created_at) values (?*, ?*, ?*, ?*, ?*, ?*, ?*)";
        let beacon_str = signed_entity_type.get_json_beacon()?;
        let parameters = vec![
            Value::String(Uuid::new_v4().to_string()),
            Value::Integer(epoch.try_into()?),
            Value::String(beacon_str),
            Value::Integer(signed_entity_type.index() as i64),
            Value::String(serde_json::to_string(protocol_message)?),
            signed_entity_type
                .get_open_message_timeout()
                .map(|t| Value::String((Utc::now() + t).to_rfc3339()))
                .unwrap_or(Value::Null),
            Value::String(Utc::now().to_rfc3339()),
        ];

        Ok(WhereCondition::new(expression, parameters))
    }
}

impl<'client> Provider<'client> for InsertOpenMessageProvider<'client> {
    type Entity = OpenMessageRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert into open_message {condition} returning {projection}")
    }
}
