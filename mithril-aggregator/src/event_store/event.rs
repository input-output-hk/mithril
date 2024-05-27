use anyhow::anyhow;
use chrono::{DateTime, Utc};

use std::{collections::HashMap, sync::Arc};

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    ConnectionExtensions, HydrationError, Projection, Query, SourceAlias, SqLiteEntity,
    SqliteConnection, WhereCondition,
};

/// Event that is sent from a thread to be persisted.
#[derive(Debug, Clone)]
pub struct EventMessage {
    /// The source of the message shall be composed of the name of the thread
    /// that sends the message, the name of the method can be added to it,
    /// separated by `:`. Example: `Runtime::update_beacon` or
    /// `HTTP::register_signer`.
    pub source: String,

    /// The action represent the action that is going to be declared and as such
    /// represents the type of the JSON content.
    pub action: String,

    /// JSON content of the message, its type is declared in the action property.
    pub content: String,

    /// Headers
    pub headers: HashMap<String, String>,
}

impl EventMessage {
    /// Instanciate a new EventMessage.
    pub fn new(source: &str, action: &str, content: &str) -> Self {
        Self {
            source: source.to_string(),
            action: action.to_string(),
            content: content.to_string(),
            headers: HashMap::new(),
        }
    }

    /// forge a new instance adding the given header
    pub fn add_header(mut self, name: &str, value: &str) -> Self {
        let _ = self.headers.insert(name.to_owned(), value.to_owned());

        self
    }
}

/// Event persisted in the Event Store.
pub struct Event {
    /// Sequential number of the event, this is set by the database.
    pub event_id: i64,

    /// timestamp of event creation in the database.
    pub created_at: DateTime<Utc>,

    /// the `source` of the original [EventMessage] this Event originates from.
    pub source: String,

    /// the `action` of the original [EventMessage] this Event originates from.
    pub action: String,

    /// the `content` of the original [EventMessage] this Event originates from.
    pub content: String,
}

impl SqLiteEntity for Event {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let created_at = &row.read::<&str, _>("created_at");

        let myself = Self {
            event_id: row.read::<i64, _>("event_id"),
            created_at: DateTime::parse_from_rfc3339(created_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{created_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
            source: row.read::<&str, _>("source").to_string(),
            action: row.read::<&str, _>("action").to_string(),
            content: row.read::<&str, _>("content").to_string(),
        };

        Ok(myself)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("event_id", "event_id", "int");
        projection.add_field("created_at", "created_at", "string");
        projection.add_field("source", "source", "string");
        projection.add_field("action", "action", "string");
        projection.add_field("content", "content", "string");

        projection
    }
}

struct InsertEventQuery {
    condition: WhereCondition,
}

impl InsertEventQuery {
    fn one(message: EventMessage) -> StdResult<Self> {
        let condition = WhereCondition::new(
            "(source, action, content, created_at) values (?*, ?*, ?*, ?*)",
            vec![
                sqlite::Value::String(message.source),
                sqlite::Value::String(message.action),
                sqlite::Value::String(format!(
                    r#"{{"headers": {}, "content": {}}}"#,
                    serde_json::to_string(&message.headers)?,
                    message.content
                )),
                sqlite::Value::String(Utc::now().to_rfc3339()),
            ],
        );

        Ok(Self { condition })
    }
}

impl Query for InsertEventQuery {
    type Entity = Event;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, data: &str) -> String {
        let projection = Self::Entity::get_projection().expand(SourceAlias::default());

        format!(r#"insert into event {data} returning {projection}"#)
    }
}

/// The EventPersister is the adapter to persist EventMessage turning them into
/// Event.
pub struct EventPersister {
    connection: Arc<SqliteConnection>,
}

impl EventPersister {
    /// Instanciate an EventPersister
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }

    /// Save an EventMessage in the database.
    pub fn persist(&self, message: EventMessage) -> StdResult<Event> {
        self.create_table_if_not_exists();
        let log_message = message.clone();
        let mut rows = self.connection.fetch(InsertEventQuery::one(message)?)?;

        rows.next().ok_or(anyhow!(
            "No record from the database after I saved event message {log_message:?}"
        ))
    }

    fn create_table_if_not_exists(&self) {
        let sql = r#"
        create table if not exists event (
            event_id integer primary key asc autoincrement,
            created_at text not null,
            source text not null,
            action text not null,
            content text nul null
        )"#;

        self.connection.execute(sql).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mithril_common::StdResult;
    use sqlite::Connection;

    #[test]
    fn event_projection() {
        let projection = Event::get_projection();

        assert_eq!(
            "event_id as event_id, created_at as created_at, source as source, action as action, content as content".to_string(),
            projection.expand(SourceAlias::default())
        )
    }

    #[test]
    fn provider_sql() {
        let message = EventMessage::new("source", "action", "content");
        let (parameters, values) = InsertEventQuery::one(message).unwrap().filters().expand();

        assert_eq!(
            "(source, action, content, created_at) values (?1, ?2, ?3, ?4)".to_string(),
            parameters
        );
        assert_eq!(4, values.len());
    }

    #[test]
    fn can_persist_event() -> StdResult<()> {
        let connection = Arc::new(Connection::open_thread_safe(":memory:").unwrap());
        let persister = EventPersister::new(connection);
        let message = EventMessage::new("source", "action", "content");

        let _event = persister.persist(message)?;
        Ok(())
    }
}
