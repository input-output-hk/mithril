use anyhow::Context;
use chrono::Utc;
use sqlite::Value;
use uuid::Uuid;

use mithril_common::{
    entities::{Epoch, ProtocolMessage, SignedEntityType},
    StdResult,
};
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::{OpenMessageRecord, OpenMessageWithSingleSignaturesRecord};

/// Simple queries to retrieve [OpenMessageRecord] from the sqlite database.
pub(crate) struct GetOpenMessageProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> GetOpenMessageProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_epoch_condition(&self, epoch: Epoch) -> WhereCondition {
        WhereCondition::new("epoch_setting_id = ?*", vec![Value::Integer(*epoch as i64)])
    }

    pub(crate) fn get_signed_entity_type_condition(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signed_entity_type_id = ?* and beacon = ?*",
            vec![
                Value::Integer(signed_entity_type.index() as i64),
                Value::String(signed_entity_type.get_json_beacon()?),
            ],
        ))
    }

    pub(crate) fn get_expired_entity_type_condition(&self, now: &str) -> WhereCondition {
        WhereCondition::new("expires_at < ?*", vec![Value::String(now.to_string())])
    }

    // Useful in test and probably in the future.
    #[allow(dead_code)]
    fn get_open_message_id_condition(&self, open_message_id: &str) -> WhereCondition {
        WhereCondition::new(
            "open_message_id = ?*",
            vec![Value::String(open_message_id.to_owned())],
        )
    }
}

impl<'client> Provider<'client> for GetOpenMessageProvider<'client> {
    type Entity = OpenMessageRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[
            ("{:open_message:}", "open_message"),
            ("{:single_signature:}", "single_signature"),
        ]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("select {projection} from open_message where {condition} order by created_at desc")
    }
}

/// Query to insert [OpenMessageRecord] in the sqlite database
pub(crate) struct InsertOpenMessageProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> InsertOpenMessageProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_insert_condition(
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

/// Query to update [OpenMessageRecord] in the sqlite database
pub(crate) struct UpdateOpenMessageProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> UpdateOpenMessageProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_update_condition(
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

/// Query to delete old [OpenMessageRecord] from the sqlite database
pub(crate) struct DeleteOpenMessageProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> DeleteOpenMessageProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_epoch_condition(&self, epoch: Epoch) -> WhereCondition {
        WhereCondition::new("epoch_setting_id < ?*", vec![Value::Integer(*epoch as i64)])
    }
}

impl<'client> Provider<'client> for DeleteOpenMessageProvider<'client> {
    type Entity = OpenMessageRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("delete from open_message where {condition} returning {projection}")
    }
}

/// Simple queries to retrieve [OpenMessageWithSingleSignaturesRecord] from the sqlite database.
pub(crate) struct OpenMessageWithSingleSignaturesProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> OpenMessageWithSingleSignaturesProvider<'client> {
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_epoch_condition(&self, epoch: Epoch) -> WhereCondition {
        WhereCondition::new("epoch_setting_id = ?*", vec![Value::Integer(*epoch as i64)])
    }

    pub(crate) fn get_signed_entity_type_condition(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signed_entity_type_id = ?* and beacon = ?*",
            vec![
                Value::Integer(signed_entity_type.index() as i64),
                Value::String(signed_entity_type.get_json_beacon()?),
            ],
        ))
    }
}

impl<'client> Provider<'client> for OpenMessageWithSingleSignaturesProvider<'client> {
    type Entity = OpenMessageWithSingleSignaturesRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[
            ("{:open_message:}", "open_message"),
            ("{:single_signature:}", "single_signature"),
        ]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!(
            r#"
select {projection}
from open_message
    left outer join single_signature
        on open_message.open_message_id = single_signature.open_message_id 
where {condition}
group by open_message.open_message_id
order by open_message.created_at desc, open_message.rowid desc
"#
        )
    }
}
