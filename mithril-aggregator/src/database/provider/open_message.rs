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
pub(crate) struct OpenMessageProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> OpenMessageProvider<'client> {
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

impl<'client> Provider<'client> for OpenMessageProvider<'client> {
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

#[cfg(test)]
mod tests {
    use chrono::DateTime;
    use sqlite::Connection;

    use mithril_common::entities::CardanoDbBeacon;
    use mithril_persistence::sqlite::SourceAlias;

    use super::*;

    #[test]
    fn open_message_with_single_signature_projection() {
        let projection = OpenMessageWithSingleSignaturesRecord::get_projection();
        let aliases = SourceAlias::new(&[
            ("{:open_message:}", "open_message"),
            ("{:single_signature:}", "single_signature"),
        ]);

        assert_eq!(
            "open_message.open_message_id as open_message_id, \
open_message.epoch_setting_id as epoch_setting_id, open_message.beacon as beacon, \
open_message.signed_entity_type_id as signed_entity_type_id, \
open_message.protocol_message as protocol_message, \
open_message.is_certified as is_certified, \
open_message.is_expired as is_expired, \
open_message.created_at as created_at, \
open_message.expires_at as expires_at, \
case when single_signature.signer_id is null then json('[]') \
else json_group_array( \
    json_object( \
        'party_id', single_signature.signer_id, \
        'signature', single_signature.signature, \
        'indexes', json(single_signature.lottery_indexes) \
    ) \
) end as single_signatures"
                .to_string(),
            projection.expand(aliases)
        )
    }

    #[test]
    fn open_message_projection() {
        let projection = OpenMessageRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);

        assert_eq!(
            "open_message.open_message_id as open_message_id, open_message.epoch_setting_id as epoch_setting_id, open_message.beacon as beacon, open_message.signed_entity_type_id as signed_entity_type_id, open_message.protocol_message as protocol_message, open_message.is_certified as is_certified, open_message.is_expired as is_expired, open_message.created_at as created_at, open_message.expires_at as expires_at".to_string(),
            projection.expand(aliases)
        )
    }

    #[test]
    fn provider_epoch_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = OpenMessageProvider::new(&connection);
        let (expr, params) = provider.get_epoch_condition(Epoch(12)).expand();

        assert_eq!("epoch_setting_id = ?1".to_string(), expr);
        assert_eq!(vec![Value::Integer(12)], params,);
    }

    #[test]
    fn provider_message_type_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = OpenMessageProvider::new(&connection);
        let beacon = CardanoDbBeacon {
            network: "whatever".to_string(),
            epoch: Epoch(4),
            immutable_file_number: 400,
        };
        let (expr, params) = provider
            .get_signed_entity_type_condition(&SignedEntityType::CardanoImmutableFilesFull(
                beacon.clone(),
            ))
            .unwrap()
            .expand();

        assert_eq!(
            "signed_entity_type_id = ?1 and beacon = ?2".to_string(),
            expr
        );
        assert_eq!(
            vec![
                Value::Integer(2),
                Value::String(serde_json::to_string(&beacon).unwrap())
            ],
            params,
        );
    }

    #[test]
    fn provider_message_id_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = OpenMessageProvider::new(&connection);
        let (expr, params) = provider
            .get_open_message_id_condition("cecd7983-8b3a-42b1-b778-6d75e87828ee")
            .expand();

        assert_eq!("open_message_id = ?1".to_string(), expr);
        assert_eq!(
            vec![Value::String(
                "cecd7983-8b3a-42b1-b778-6d75e87828ee".to_string()
            )],
            params,
        );
    }

    #[test]
    fn provider_expired_entity_type_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = OpenMessageProvider::new(&connection);
        let now = Utc::now().to_rfc3339();
        let (expr, params) = provider.get_expired_entity_type_condition(&now).expand();

        assert_eq!("expires_at < ?1".to_string(), expr);
        assert_eq!(vec![Value::String(now)], params);
    }

    #[test]
    fn insert_provider_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = InsertOpenMessageProvider::new(&connection);
        let epoch = Epoch(12);
        let (expr, params) = provider
            .get_insert_condition(
                epoch,
                &SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
                    "testnet".to_string(),
                    2,
                    4,
                )),
                &ProtocolMessage::new(),
            )
            .unwrap()
            .expand();

        assert_eq!("(open_message_id, epoch_setting_id, beacon, signed_entity_type_id, protocol_message, expires_at, created_at) values (?1, ?2, ?3, ?4, ?5, ?6, ?7)".to_string(), expr);
        assert_eq!(Value::Integer(12), params[1]);
        assert_eq!(
            Value::String(
                r#"{"network":"testnet","epoch":2,"immutable_file_number":4}"#.to_string()
            ),
            params[2]
        );
        assert_eq!(Value::Integer(2), params[3]);
        assert_eq!(
            Value::String(serde_json::to_string(&ProtocolMessage::new()).unwrap()),
            params[4]
        );
    }

    #[test]
    fn update_provider_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = UpdateOpenMessageProvider::new(&connection);
        let open_message = OpenMessageRecord {
            open_message_id: Uuid::new_v4(),
            epoch: Epoch(12),
            signed_entity_type: SignedEntityType::dummy(),
            protocol_message: ProtocolMessage::new(),
            is_certified: true,
            is_expired: false,
            created_at: DateTime::<Utc>::default(),
            expires_at: None,
        };
        let (expr, params) = provider
            .get_update_condition(&open_message)
            .unwrap()
            .expand();

        assert_eq!(
            "epoch_setting_id = ?1, beacon = ?2, signed_entity_type_id = ?3, protocol_message = ?4, is_certified = ?5, is_expired = ?6, expires_at = ?7 where open_message_id = ?8"
                .to_string(),
            expr
        );
        assert_eq!(
            vec![
                Value::Integer(*open_message.epoch as i64),
                Value::String(open_message.signed_entity_type.get_json_beacon().unwrap()),
                Value::Integer(open_message.signed_entity_type.index() as i64),
                Value::String(serde_json::to_string(&open_message.protocol_message).unwrap()),
                Value::Integer(open_message.is_certified as i64),
                Value::Integer(open_message.is_expired as i64),
                open_message
                    .expires_at
                    .map(|d| Value::String(d.to_rfc3339()))
                    .unwrap_or(Value::Null),
                Value::String(open_message.open_message_id.to_string()),
            ],
            params
        );
    }

    #[test]
    fn delete_provider_epoch_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = DeleteOpenMessageProvider::new(&connection);
        let (expr, params) = provider.get_epoch_condition(Epoch(12)).expand();

        assert_eq!("epoch_setting_id < ?1".to_string(), expr);
        assert_eq!(vec![Value::Integer(12)], params,);
    }
}
