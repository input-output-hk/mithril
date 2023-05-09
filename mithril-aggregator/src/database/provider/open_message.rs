use mithril_common::StdError;

use mithril_common::entities::{ProtocolMessage, SingleSignatures};
use mithril_common::{
    entities::{Epoch, SignedEntityType},
    sqlite::{HydrationError, Projection, SqLiteEntity, WhereCondition},
    sqlite::{Provider, SourceAlias},
};

use chrono::NaiveDateTime;
use sqlite::Row;
use sqlite::{Connection, Value};

use std::sync::Arc;

use tokio::sync::Mutex;
use uuid::Uuid;

type StdResult<T> = Result<T, StdError>;

/// ## OpenMessage
///
/// An open message is a message open for signatures. Every signer may send a
/// single signature for this message from which a multi signature will be
/// generated if possible.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpenMessageRecord {
    /// OpenMessage unique identifier
    pub open_message_id: Uuid,

    /// Epoch
    pub epoch: Epoch,

    /// Type of message
    pub signed_entity_type: SignedEntityType,

    /// Message used by the Mithril Protocol
    pub protocol_message: ProtocolMessage,

    /// Has this open message been converted into a certificate?
    pub is_certified: bool,

    /// Message creation datetime, it is set by the database.
    pub created_at: NaiveDateTime,
}

impl OpenMessageRecord {
    #[cfg(test)]
    /// Create a dumb OpenMessage instance mainly for test purposes
    pub fn dummy() -> Self {
        let beacon = mithril_common::test_utils::fake_data::beacon();
        let epoch = beacon.epoch;
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon);

        Self {
            open_message_id: Uuid::parse_str("193d1442-e89b-43cf-9519-04d8db9a12ff").unwrap(),
            epoch,
            signed_entity_type,
            protocol_message: ProtocolMessage::new(),
            is_certified: false,
            created_at: chrono::Local::now().naive_local(),
        }
    }
}

impl From<OpenMessageWithSingleSignaturesRecord> for OpenMessageRecord {
    fn from(value: OpenMessageWithSingleSignaturesRecord) -> Self {
        Self {
            open_message_id: value.open_message_id,
            epoch: value.epoch,
            signed_entity_type: value.signed_entity_type,
            protocol_message: value.protocol_message,
            is_certified: value.is_certified,
            created_at: value.created_at,
        }
    }
}

impl SqLiteEntity for OpenMessageRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let open_message_id = row.get::<String, _>(0);
        let open_message_id = Uuid::parse_str(&open_message_id).map_err(|e| {
            HydrationError::InvalidData(format!(
                "Invalid UUID in open_message.open_message_id: '{open_message_id}'. Error: {e}"
            ))
        })?;
        let protocol_message = row.get::<String, _>(4);
        let protocol_message = serde_json::from_str(&protocol_message).map_err(|e| {
            HydrationError::InvalidData(format!(
                "Invalid protocol message JSON representation '{protocol_message}'. Error: {e}"
            ))
        })?;
        let epoch_setting_id = row.get::<i64, _>(1);
        let epoch_val = u64::try_from(epoch_setting_id)
            .map_err(|e| panic!("Integer field open_message.epoch_setting_id (value={epoch_setting_id}) is incompatible with u64 Epoch representation. Error = {e}"))?;

        // TODO: We need to check first that the cell can be read as a string first
        // (e.g. when beacon json is '{"network": "dev", "epoch": 1, "immutable_file_number": 2}').
        // If it fails, we fallback on readign the cell as an integer (e.g. when beacon json is '5').
        // Maybe there is a better way of doing this.
        let beacon_str = row
            .try_get::<String, _>(2)
            .unwrap_or_else(|_| (row.get::<i64, _>(2)).to_string());

        let signed_entity_type_id = usize::try_from(row.get::<i64, _>(3)).map_err(|e| {
            panic!(
                "Integer field open_message.signed_entity_type_id cannot be turned into usize: {e}"
            )
        })?;
        let signed_entity_type = SignedEntityType::hydrate(signed_entity_type_id, &beacon_str)?;
        let is_certified = row.get::<i64, _>(5) != 0;
        let datetime = &row.get::<String, _>(6);
        let created_at =
            NaiveDateTime::parse_from_str(datetime, "%Y-%m-%d %H:%M:%S").map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not turn open_message.created_at field value '{datetime}' to NaiveDateTime. Error: {e}"
                ))
            })?;

        let open_message = Self {
            open_message_id,
            epoch: Epoch(epoch_val),
            signed_entity_type,
            protocol_message,
            is_certified,
            created_at,
        };

        Ok(open_message)
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            (
                "open_message_id",
                "{:open_message:}.open_message_id",
                "text",
            ),
            (
                "epoch_setting_id",
                "{:open_message:}.epoch_setting_id",
                "int",
            ),
            ("beacon", "{:open_message:}.beacon", "text"),
            (
                "signed_entity_type_id",
                "{:open_message:}.signed_entity_type_id",
                "int",
            ),
            (
                "protocol_message",
                "{:open_message:}.protocol_message",
                "text",
            ),
            ("is_certified", "{:open_message:}.is_certified", "bool"),
            ("created_at", "{:open_message:}.created_at", "text"),
        ])
    }
}

struct OpenMessageProvider<'client> {
    connection: &'client Connection,
}

impl<'client> OpenMessageProvider<'client> {
    pub fn new(connection: &'client Connection) -> Self {
        Self { connection }
    }

    fn get_epoch_condition(&self, epoch: Epoch) -> WhereCondition {
        WhereCondition::new(
            "epoch_setting_id = ?*",
            vec![Value::Integer(epoch.0 as i64)],
        )
    }

    fn get_signed_entity_type_condition(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> WhereCondition {
        WhereCondition::new(
            "signed_entity_type_id = ?*",
            vec![Value::Integer(signed_entity_type.index() as i64)],
        )
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

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[
            ("{:open_message:}", "open_message"),
            ("{:single_signature:}", "single_signature"),
        ]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("select {projection} from open_message where {condition} order by created_at desc")
    }

    fn get_connection(&'client self) -> &'client Connection {
        self.connection
    }
}

struct InsertOpenMessageProvider<'client> {
    connection: &'client Connection,
}
impl<'client> InsertOpenMessageProvider<'client> {
    pub fn new(connection: &'client Connection) -> Self {
        Self { connection }
    }

    fn get_insert_condition(
        &self,
        epoch: Epoch,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<WhereCondition> {
        let expression = "(open_message_id, epoch_setting_id, beacon, signed_entity_type_id, protocol_message) values (?*, ?*, ?*, ?*, ?*)";
        let beacon_str = signed_entity_type.get_json_beacon()?;
        let parameters = vec![
            Value::String(Uuid::new_v4().to_string()),
            Value::Integer(epoch.0 as i64),
            Value::String(beacon_str),
            Value::Integer(signed_entity_type.index() as i64),
            Value::String(serde_json::to_string(protocol_message)?),
        ];

        Ok(WhereCondition::new(expression, parameters))
    }
}

impl<'client> Provider<'client> for InsertOpenMessageProvider<'client> {
    type Entity = OpenMessageRecord;

    fn get_connection(&'client self) -> &'client Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert into open_message {condition} returning {projection}")
    }
}

struct UpdateOpenMessageProvider<'client> {
    connection: &'client Connection,
}
impl<'client> UpdateOpenMessageProvider<'client> {
    pub fn new(connection: &'client Connection) -> Self {
        Self { connection }
    }

    fn get_update_condition(&self, open_message: &OpenMessageRecord) -> StdResult<WhereCondition> {
        let expression = "(open_message_id, epoch_setting_id, beacon, signed_entity_type_id, protocol_message, is_certified) values (?*, ?*, ?*, ?*, ?*, ?*)";
        let beacon_str = open_message.signed_entity_type.get_json_beacon()?;
        let parameters = vec![
            Value::String(open_message.open_message_id.to_string()),
            Value::Integer(open_message.epoch.0 as i64),
            Value::String(beacon_str),
            Value::Integer(open_message.signed_entity_type.index() as i64),
            Value::String(serde_json::to_string(&open_message.protocol_message)?),
            Value::Integer(open_message.is_certified as i64),
        ];

        Ok(WhereCondition::new(expression, parameters))
    }
}

impl<'client> Provider<'client> for UpdateOpenMessageProvider<'client> {
    type Entity = OpenMessageRecord;

    fn get_connection(&'client self) -> &'client Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("replace into open_message {condition} returning {projection}")
    }
}

struct DeleteOpenMessageProvider<'client> {
    connection: &'client Connection,
}

impl<'client> DeleteOpenMessageProvider<'client> {
    pub fn new(connection: &'client Connection) -> Self {
        Self { connection }
    }

    fn get_epoch_condition(&self, epoch: Epoch) -> WhereCondition {
        WhereCondition::new(
            "epoch_setting_id <= ?*",
            vec![Value::Integer(epoch.0 as i64)],
        )
    }
}

impl<'client> Provider<'client> for DeleteOpenMessageProvider<'client> {
    type Entity = OpenMessageRecord;

    fn get_connection(&'client self) -> &'client Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("delete from open_message where {condition} returning {projection}")
    }
}

/// Open Message with associated single signatures if any.
#[derive(Debug, Clone)]
pub struct OpenMessageWithSingleSignaturesRecord {
    /// OpenMessage unique identifier
    pub open_message_id: Uuid,

    /// Epoch
    pub epoch: Epoch,

    /// Type of message
    pub signed_entity_type: SignedEntityType,

    /// Message used by the Mithril Protocol
    pub protocol_message: ProtocolMessage,

    /// Has this message been converted into a Certificate?
    pub is_certified: bool,

    /// associated single signatures
    pub single_signatures: Vec<SingleSignatures>,

    /// Message creation datetime, it is set by the database.
    pub created_at: NaiveDateTime,
}

impl SqLiteEntity for OpenMessageWithSingleSignaturesRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let single_signatures = &row.get::<String, _>(7);
        let single_signatures: Vec<SingleSignatures> = serde_json::from_str(single_signatures)
            .map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not parse single signatures JSON: '{single_signatures}'. Error: {e}"
                ))
            })?;

        let open_message = OpenMessageRecord::hydrate(row)?;

        let open_message = Self {
            open_message_id: open_message.open_message_id,
            epoch: open_message.epoch,
            signed_entity_type: open_message.signed_entity_type,
            protocol_message: open_message.protocol_message,
            is_certified: open_message.is_certified,
            single_signatures,
            created_at: open_message.created_at,
        };

        Ok(open_message)
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            (
                "open_message_id",
                "{:open_message:}.open_message_id",
                "text",
            ),
            (
                "epoch_setting_id",
                "{:open_message:}.epoch_setting_id",
                "int",
            ),
            ("beacon", "{:open_message:}.beacon", "text"),
            (
                "signed_entity_type_id",
                "{:open_message:}.signed_entity_type_id",
                "int",
            ),
            ("protocol_message", "{:open_message:}.protocol_message", "text"),
            ("is_certified", "{:open_message:}.is_certified", "bool"),
            ("created_at", "{:open_message:}.created_at", "text"),
            ("single_signatures", "case when {:single_signature:}.signer_id is null then json('[]') else json_group_array(json_object('party_id', {:single_signature:}.signer_id, 'signature', {:single_signature:}.signature, 'indexes', json({:single_signature:}.lottery_indexes))) end", "text")
        ])
    }
}

struct OpenMessageWithSingleSignaturesProvider<'client> {
    connection: &'client Connection,
}

impl<'client> OpenMessageWithSingleSignaturesProvider<'client> {
    pub fn new(connection: &'client Connection) -> Self {
        Self { connection }
    }

    fn get_signed_entity_type_condition(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> WhereCondition {
        WhereCondition::new(
            "signed_entity_type_id = ?*",
            vec![Value::Integer(signed_entity_type.index() as i64)],
        )
    }
}

impl<'client> Provider<'client> for OpenMessageWithSingleSignaturesProvider<'client> {
    type Entity = OpenMessageWithSingleSignaturesRecord;

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

    fn get_connection(&'client self) -> &'client Connection {
        self.connection
    }
}

/// ## Open message repository
///
/// This is a business oriented layer to perform actions on the database through
/// providers.
pub struct OpenMessageRepository {
    connection: Arc<Mutex<Connection>>,
}

impl OpenMessageRepository {
    /// Instanciate service
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }

    /// Return the latest [OpenMessageRecord] for the given Epoch and [SignedEntityType].
    pub async fn get_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessageRecord>> {
        let lock = self.connection.lock().await;
        let provider = OpenMessageProvider::new(&lock);
        let filters = provider
            .get_epoch_condition(signed_entity_type.get_epoch())
            .and_where(provider.get_signed_entity_type_condition(signed_entity_type));
        let mut messages = provider.find(filters)?;

        Ok(messages.next())
    }

    /// Create a new [OpenMessageRecord] in the database.
    pub async fn create_open_message(
        &self,
        epoch: Epoch,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<OpenMessageRecord> {
        let lock = self.connection.lock().await;
        let provider = InsertOpenMessageProvider::new(&lock);
        let filters = provider.get_insert_condition(epoch, signed_entity_type, protocol_message)?;
        let mut cursor = provider.find(filters)?;

        cursor
            .next()
            .ok_or_else(|| panic!("Inserting an open_message should not return nothing."))
    }

    /// Updates an [OpenMessageRecord] in the database.
    pub async fn update_open_message(
        &self,
        open_message: &OpenMessageRecord,
    ) -> StdResult<OpenMessageRecord> {
        let lock = self.connection.lock().await;
        let provider = UpdateOpenMessageProvider::new(&lock);
        let filters = provider.get_update_condition(open_message)?;
        let mut cursor = provider.find(filters)?;

        cursor
            .next()
            .ok_or_else(|| panic!("Updating an open_message should not return nothing."))
    }

    /// Remove all the [OpenMessageRecord] for the given Epoch in the database.
    /// It returns the number of messages removed.
    pub async fn clean_epoch(&self, epoch: Epoch) -> StdResult<usize> {
        let lock = self.connection.lock().await;
        let provider = DeleteOpenMessageProvider::new(&lock);
        let filters = provider.get_epoch_condition(epoch);
        let cursor = provider.find(filters)?;

        Ok(cursor.count())
    }

    /// Return an open message with its associated single signatures if any.
    pub async fn get_open_message_with_single_signatures(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessageWithSingleSignaturesRecord>> {
        let lock = self.connection.lock().await;
        let provider = OpenMessageWithSingleSignaturesProvider::new(&lock);
        let filters = provider.get_signed_entity_type_condition(signed_entity_type);
        let mut messages = provider.find(filters)?;

        Ok(messages.next())
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::Beacon, sqlite::SourceAlias};

    use crate::{dependency_injection::DependenciesBuilder, Configuration};

    use crate::database::provider::test_helper::{
        setup_single_signature_db, setup_single_signature_records,
    };

    use super::*;

    #[test]
    fn open_message_with_single_signature_projection() {
        let projection = OpenMessageWithSingleSignaturesRecord::get_projection();
        let aliases = SourceAlias::new(&[
            ("{:open_message:}", "open_message"),
            ("{:single_signature:}", "single_signature"),
        ]);

        assert_eq!(
            "open_message.open_message_id as open_message_id, open_message.epoch_setting_id as epoch_setting_id, open_message.beacon as beacon, open_message.signed_entity_type_id as signed_entity_type_id, open_message.protocol_message as protocol_message, open_message.is_certified as is_certified, open_message.created_at as created_at, case when single_signature.signer_id is null then json('[]') else json_group_array(json_object('party_id', single_signature.signer_id, 'signature', single_signature.signature, 'indexes', json(single_signature.lottery_indexes))) end as single_signatures".to_string(),
            projection.expand(aliases)
        )
    }

    #[test]
    fn open_message_projection() {
        let projection = OpenMessageRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);

        assert_eq!(
            "open_message.open_message_id as open_message_id, open_message.epoch_setting_id as epoch_setting_id, open_message.beacon as beacon, open_message.signed_entity_type_id as signed_entity_type_id, open_message.protocol_message as protocol_message, open_message.is_certified as is_certified, open_message.created_at as created_at".to_string(),
            projection.expand(aliases)
        )
    }

    #[test]
    fn provider_epoch_condition() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = OpenMessageProvider::new(&connection);
        let (expr, params) = provider.get_epoch_condition(Epoch(12)).expand();

        assert_eq!("epoch_setting_id = ?1".to_string(), expr);
        assert_eq!(vec![Value::Integer(12)], params,);
    }

    #[test]
    fn provider_message_type_condition() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = OpenMessageProvider::new(&connection);
        let beacon = Beacon {
            network: "whatever".to_string(),
            epoch: Epoch(4),
            immutable_file_number: 400,
        };
        let (expr, params) = provider
            .get_signed_entity_type_condition(&SignedEntityType::CardanoImmutableFilesFull(beacon))
            .expand();

        assert_eq!("signed_entity_type_id = ?1".to_string(), expr);
        assert_eq!(vec![Value::Integer(2)], params,);
    }

    #[test]
    fn provider_message_id_condition() {
        let connection = Connection::open(":memory:").unwrap();
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
    fn insert_provider_condition() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = InsertOpenMessageProvider::new(&connection);
        let epoch = Epoch(12);
        let (expr, params) = provider
            .get_insert_condition(
                epoch,
                &SignedEntityType::CardanoImmutableFilesFull(Beacon::default()),
                &ProtocolMessage::new(),
            )
            .unwrap()
            .expand();

        assert_eq!("(open_message_id, epoch_setting_id, beacon, signed_entity_type_id, protocol_message) values (?1, ?2, ?3, ?4, ?5)".to_string(), expr);
        assert_eq!(Value::Integer(12), params[1]);
        assert_eq!(
            Value::String(r#"{"network":"","epoch":0,"immutable_file_number":0}"#.to_string()),
            params[2]
        );
        assert_eq!(Value::Integer(2), params[3]);
        assert!(!params[4].as_string().unwrap().is_empty());
    }

    #[test]
    fn update_provider_condition() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = UpdateOpenMessageProvider::new(&connection);
        let open_message = OpenMessageRecord {
            open_message_id: Uuid::new_v4(),
            epoch: Epoch(12),
            signed_entity_type: SignedEntityType::dummy(),
            protocol_message: ProtocolMessage::new(),
            is_certified: true,
            created_at: NaiveDateTime::default(),
        };
        let (expr, params) = provider
            .get_update_condition(&open_message)
            .unwrap()
            .expand();

        assert_eq!("(open_message_id, epoch_setting_id, beacon, signed_entity_type_id, protocol_message, is_certified) values (?1, ?2, ?3, ?4, ?5, ?6)".to_string(), expr);
        assert_eq!(
            vec![
                Value::String(open_message.open_message_id.to_string()),
                Value::Integer(open_message.epoch.0 as i64),
                Value::String(open_message.signed_entity_type.get_json_beacon().unwrap()),
                Value::Integer(open_message.signed_entity_type.index() as i64),
                Value::String(serde_json::to_string(&open_message.protocol_message).unwrap()),
                Value::Integer(open_message.is_certified as i64),
            ],
            params
        );
    }

    #[test]
    fn delete_provider_epoch_condition() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = DeleteOpenMessageProvider::new(&connection);
        let (expr, params) = provider.get_epoch_condition(Epoch(12)).expand();

        assert_eq!("epoch_setting_id <= ?1".to_string(), expr);
        assert_eq!(vec![Value::Integer(12)], params,);
    }

    async fn get_connection() -> Arc<Mutex<Connection>> {
        let config = Configuration::new_sample();
        let mut builder = DependenciesBuilder::new(config);
        let connection = builder.get_sqlite_connection().await.unwrap();
        {
            let lock = connection.lock().await;
            lock.execute(r#"insert into epoch_setting(epoch_setting_id, protocol_parameters) values (1, '{"k": 100, "m": 5, "phi": 0.65 }'), (2, '{"k": 100, "m": 5, "phi": 0.65 }');"#).unwrap();
        }

        connection
    }

    #[tokio::test]
    async fn repository_get_open_message() {
        let connection = get_connection().await;
        let repository = OpenMessageRepository::new(connection.clone());
        let beacon = Beacon::new("devnet".to_string(), 1, 1);

        let signed_entity_type = SignedEntityType::MithrilStakeDistribution(beacon.epoch);
        repository
            .create_open_message(beacon.epoch, &signed_entity_type, &ProtocolMessage::new())
            .await
            .unwrap();
        let open_message_result = repository
            .get_open_message(&signed_entity_type)
            .await
            .unwrap();
        assert!(open_message_result.is_some());

        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        repository
            .create_open_message(beacon.epoch, &signed_entity_type, &ProtocolMessage::new())
            .await
            .unwrap();
        let open_message_result = repository
            .get_open_message(&signed_entity_type)
            .await
            .unwrap();
        assert!(open_message_result.is_some());
    }

    #[tokio::test]
    async fn repository_create_open_message() {
        let connection = get_connection().await;
        let repository = OpenMessageRepository::new(connection.clone());
        let epoch = Epoch(1);
        let open_message = repository
            .create_open_message(
                epoch,
                &SignedEntityType::CardanoImmutableFilesFull(Beacon::default()),
                &ProtocolMessage::new(),
            )
            .await
            .unwrap();

        assert_eq!(Epoch(1), open_message.epoch);
        assert_eq!(
            SignedEntityType::CardanoImmutableFilesFull(Beacon::default()),
            open_message.signed_entity_type
        );

        let message = {
            let lock = connection.lock().await;
            let provider = OpenMessageProvider::new(&lock);
            let mut cursor = provider
                .find(WhereCondition::new(
                    "open_message_id = ?*",
                    vec![Value::String(open_message.open_message_id.to_string())],
                ))
                .unwrap();

            cursor.next().unwrap_or_else(|| {
                panic!(
                    "OpenMessage ID='{}' should exist in the database.",
                    open_message.open_message_id
                )
            })
        };

        assert_eq!(open_message.protocol_message, message.protocol_message);
        assert_eq!(open_message.epoch, message.epoch);
    }

    #[tokio::test]
    async fn repository_update_open_message() {
        let connection = get_connection().await;
        let repository = OpenMessageRepository::new(connection.clone());
        let epoch = Epoch(1);
        let open_message = repository
            .create_open_message(
                epoch,
                &SignedEntityType::CardanoImmutableFilesFull(Beacon::default()),
                &ProtocolMessage::new(),
            )
            .await
            .unwrap();

        let mut open_message_updated = open_message;
        open_message_updated.is_certified = true;
        let open_message_saved = repository
            .update_open_message(&open_message_updated)
            .await
            .unwrap();

        assert_eq!(open_message_updated, open_message_saved);
    }

    #[tokio::test]
    async fn repository_clean_open_message() {
        let connection = get_connection().await;
        let repository = OpenMessageRepository::new(connection.clone());
        let beacon = Beacon {
            epoch: Epoch(1),
            ..Beacon::default()
        };
        let _ = repository
            .create_open_message(
                beacon.epoch,
                &SignedEntityType::CardanoImmutableFilesFull(beacon.clone()),
                &ProtocolMessage::new(),
            )
            .await
            .unwrap();
        let _ = repository
            .create_open_message(
                beacon.epoch,
                &SignedEntityType::CardanoImmutableFilesFull(Beacon {
                    epoch: Epoch(2),
                    ..beacon
                }),
                &ProtocolMessage::new(),
            )
            .await
            .unwrap();
        let count = repository.clean_epoch(Epoch(2)).await.unwrap();

        assert_eq!(2, count);
    }

    #[tokio::test]
    async fn repository_get_open_message_with_single_signatures_when_signatures_exist() {
        let single_signature_records = setup_single_signature_records(1, 1, 4);

        let connection = Connection::open(":memory:").unwrap();
        setup_single_signature_db(&connection, single_signature_records.clone()).unwrap();
        let repository = OpenMessageRepository::new(Arc::new(Mutex::new(connection)));

        let mut open_message = OpenMessageRecord::dummy();
        open_message.open_message_id = single_signature_records[0].open_message_id;
        repository.update_open_message(&open_message).await.unwrap();

        let open_message_with_single_signatures = repository
            .get_open_message_with_single_signatures(&open_message.signed_entity_type)
            .await
            .unwrap()
            .unwrap();
        assert_eq!(
            4,
            open_message_with_single_signatures.single_signatures.len()
        )
    }

    #[tokio::test]
    async fn repository_get_open_message_with_single_signatures_when_signatures_not_exist() {
        let connection = Connection::open(":memory:").unwrap();
        setup_single_signature_db(&connection, Vec::new()).unwrap();
        let repository = OpenMessageRepository::new(Arc::new(Mutex::new(connection)));

        let open_message = OpenMessageRecord::dummy();
        repository
            .create_open_message(
                open_message.epoch,
                &open_message.signed_entity_type,
                &open_message.protocol_message,
            )
            .await
            .unwrap();

        let open_message_with_single_signatures = repository
            .get_open_message_with_single_signatures(&open_message.signed_entity_type)
            .await
            .unwrap()
            .unwrap();
        assert!(open_message_with_single_signatures
            .single_signatures
            .is_empty())
    }
}
