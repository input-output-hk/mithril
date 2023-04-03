use mithril_common::StdError;

use mithril_common::{
    entities::{Beacon, Epoch, SignedEntityType},
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
#[allow(dead_code)]
pub struct OpenMessage {
    /// OpenMessage unique identifier
    open_message_id: Uuid,

    /// Epoch
    epoch: Epoch,

    /// Beacon, this is the discriminant of this message type in the current
    /// Epoch
    beacon: Beacon,

    /// Type of message
    signed_entity_type: SignedEntityType,

    /// Message content
    message: String,

    /// Message creation datetime, it is set by the database.
    created_at: NaiveDateTime,
}

impl SqLiteEntity for OpenMessage {
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
        let message = row.get::<String, _>(4);
        let epoch_setting_id = row.get::<i64, _>(1);
        let epoch_val = u64::try_from(epoch_setting_id)
            .map_err(|e| panic!("Integer field open_message.epoch_setting_id (value={epoch_setting_id}) is incompatible with u64 Epoch representation. Error = {e}"))?;

        let signed_entity_type_id = usize::try_from(row.get::<i64, _>(3)).map_err(|e| {
            panic!(
                "Integer field open_message.signed_entity_type_id cannot be turned into usize: {e}"
            )
        })?;
        let signed_entity_type = SignedEntityType::from_repr(signed_entity_type_id)
            .ok_or_else(|| HydrationError::InvalidData(format!(
                "Field open_message.signed_type_id can be either 0, 1 or 2, ({signed_entity_type_id} given)."
            )))?;
        let beacon_str = row.get::<String, _>(2);
        let beacon: Beacon = serde_json::from_str(&beacon_str).map_err(|e| {
            HydrationError::InvalidData(format!(
                "Invalid Beacon JSON in open_message.beacon: '{beacon_str}'. Error: {e}"
            ))
        })?;
        let datetime = &row.get::<String, _>(5);
        let created_at =
            NaiveDateTime::parse_from_str(datetime, "%Y-%m-%d %H:%M:%S").map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not turn open_message.created_at field value '{datetime}' to NaiveDateTime. Error: {e}"
                ))
            })?;

        let open_message = Self {
            open_message_id,
            epoch: Epoch(epoch_val),
            beacon,
            signed_entity_type,
            message,
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
            ("message", "{:open_message:}.message", "text"),
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
            vec![Value::Integer(*signed_entity_type as i64)],
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
    type Entity = OpenMessage;

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
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
        beacon: &Beacon,
        signed_entity_type: &SignedEntityType,
        message: &str,
    ) -> StdResult<WhereCondition> {
        let expression = "(open_message_id, epoch_setting_id, beacon, signed_entity_type_id, message) values (?*, ?*, ?*, ?*, ?*)";
        let parameters = vec![
            Value::String(Uuid::new_v4().to_string()),
            Value::Integer(epoch.0 as i64),
            Value::String(serde_json::to_string(beacon)?),
            Value::Integer(*signed_entity_type as i64),
            Value::String(message.to_string()),
        ];

        Ok(WhereCondition::new(expression, parameters))
    }
}

impl<'client> Provider<'client> for InsertOpenMessageProvider<'client> {
    type Entity = OpenMessage;

    fn get_connection(&'client self) -> &'client Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert into open_message {condition} returning {projection}")
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
            "epoch_setting_id = ?*",
            vec![Value::Integer(epoch.0 as i64)],
        )
    }
}

impl<'client> Provider<'client> for DeleteOpenMessageProvider<'client> {
    type Entity = OpenMessage;

    fn get_connection(&'client self) -> &'client Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("delete from open_message where {condition} returning {projection}")
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

    /// Return the latest [OpenMessage] for the given Epoch and [SignedEntityType].
    pub async fn get_open_message(
        &self,
        epoch: Epoch,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>> {
        let lock = self.connection.lock().await;
        let provider = OpenMessageProvider::new(&lock);
        let filters = provider
            .get_epoch_condition(epoch)
            .and_where(provider.get_signed_entity_type_condition(signed_entity_type));
        let mut messages = provider.find(filters)?;

        Ok(messages.next())
    }

    /// Create a new [OpenMessage] in the database.
    pub async fn create_open_message(
        &self,
        epoch: Epoch,
        beacon: &Beacon,
        signed_entity_type: &SignedEntityType,
        message: &str,
    ) -> StdResult<OpenMessage> {
        let lock = self.connection.lock().await;
        let provider = InsertOpenMessageProvider::new(&lock);
        let filters = provider.get_insert_condition(epoch, beacon, signed_entity_type, message)?;
        let mut cursor = provider.find(filters)?;

        cursor
            .next()
            .ok_or_else(|| panic!("Inserting an open_message should not return nothing."))
    }

    /// Remove all the [OpenMessage] for the given Epoch in the database.
    /// It returns the number of messages removed.
    pub async fn clean_epoch(&self, epoch: Epoch) -> StdResult<usize> {
        let lock = self.connection.lock().await;
        let provider = DeleteOpenMessageProvider::new(&lock);
        let filters = provider.get_epoch_condition(epoch);
        let cursor = provider.find(filters)?;

        Ok(cursor.count())
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::sqlite::SourceAlias;

    use crate::{dependency_injection::DependenciesBuilder, Configuration};

    use super::*;

    #[test]
    fn open_message_projection() {
        let projection = OpenMessage::get_projection();
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);

        assert_eq!(
            "open_message.open_message_id as open_message_id, open_message.epoch_setting_id as epoch_setting_id, open_message.beacon as beacon, open_message.signed_entity_type_id as signed_entity_type_id, open_message.message as message, open_message.created_at as created_at".to_string(),
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
        let (expr, params) = provider
            .get_signed_entity_type_condition(&SignedEntityType::CardanoImmutableFilesFull)
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
        let (expr, params) = provider
            .get_insert_condition(
                Epoch(12),
                &Beacon::default(),
                &SignedEntityType::CardanoStakeDistribution,
                "This is a message",
            )
            .unwrap()
            .expand();

        assert_eq!("(open_message_id, epoch_setting_id, beacon, signed_entity_type_id, message) values (?1, ?2, ?3, ?4, ?5)".to_string(), expr);
        assert_eq!(Value::Integer(12), params[1]);
        assert_eq!(
            Value::String(r#"{"network":"","epoch":0,"immutable_file_number":0}"#.to_string()),
            params[2]
        );
        assert_eq!(Value::Integer(1), params[3]);
        assert_eq!(Value::String("This is a message".to_string()), params[4]);
    }

    #[test]
    fn delete_provider_epoch_condition() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = DeleteOpenMessageProvider::new(&connection);
        let (expr, params) = provider.get_epoch_condition(Epoch(12)).expand();

        assert_eq!("epoch_setting_id = ?1".to_string(), expr);
        assert_eq!(vec![Value::Integer(12)], params,);
    }

    async fn get_connection() -> Arc<Mutex<Connection>> {
        let config = Configuration::new_sample();
        let mut builder = DependenciesBuilder::new(config);
        let connection = builder.get_sqlite_connection().await.unwrap();
        {
            let lock = connection.lock().await;
            lock.execute(r#"insert into epoch_setting(epoch_setting_id, protocol_parameters) values (1, '{"k": 100, "m": 5, "phi": 0.65 }');"#).unwrap();
        }

        connection
    }

    #[tokio::test]
    async fn repository_create_open_message() {
        let connection = get_connection().await;
        let repository = OpenMessageRepository::new(connection.clone());
        let open_message = repository
            .create_open_message(
                Epoch(1),
                &Beacon::default(),
                &SignedEntityType::CardanoImmutableFilesFull,
                "this is a message",
            )
            .await
            .unwrap();

        assert_eq!(Epoch(1), open_message.epoch);
        assert_eq!("this is a message".to_string(), open_message.message);
        assert_eq!(
            SignedEntityType::CardanoImmutableFilesFull,
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

        assert_eq!(open_message.message, message.message);
        assert_eq!(open_message.epoch, message.epoch);
    }

    #[tokio::test]
    async fn repository_clean_open_message() {
        let connection = get_connection().await;
        let repository = OpenMessageRepository::new(connection.clone());
        let _ = repository
            .create_open_message(
                Epoch(1),
                &Beacon::default(),
                &SignedEntityType::CardanoImmutableFilesFull,
                "this is a message",
            )
            .await
            .unwrap();
        let _ = repository
            .create_open_message(
                Epoch(1),
                &Beacon::default(),
                &SignedEntityType::MithrilStakeDistribution,
                "this is a stake distribution",
            )
            .await
            .unwrap();
        let count = repository.clean_epoch(Epoch(1)).await.unwrap();

        assert_eq!(2, count);
    }
}
