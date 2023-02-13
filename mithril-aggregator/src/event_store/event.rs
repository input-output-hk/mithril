use std::{collections::HashMap, error::Error, sync::Arc};

use chrono::NaiveDateTime;
use mithril_common::sqlite::{HydrationError, Projection, Provider, SqLiteEntity};
use sqlite::Connection;
use std::sync::Mutex;

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
}

impl EventMessage {
    /// Instanciate a new EventMessage.
    pub fn new(source: &str, action: &str, content: &str) -> Self {
        Self {
            source: source.to_string(),
            action: action.to_string(),
            content: content.to_string(),
        }
    }
}

/// Event persisted in the Event Store.
pub struct Event {
    /// Sequential number of the event, this is set by the database.
    pub event_id: i64,

    /// timestamp of event creation in the database.
    pub created_at: NaiveDateTime,

    /// the `source` of the original [EventMessage] this Event originates from.
    pub source: String,

    /// the `action` of the original [EventMessage] this Event originates from.
    pub action: String,

    /// the `content` of the original [EventMessage] this Event originates from.
    pub content: String,
}

impl SqLiteEntity for Event {
    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("event_id", "event_id", "int");
        projection.add_field("created_at", "unixepoch(created_at)", "string");
        projection.add_field("source", "source", "string");
        projection.add_field("action", "action", "string");
        projection.add_field("content", "content", "string");

        projection
    }

    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let myself = Self {
            event_id: row.get::<i64, _>("event_id"),
            created_at: NaiveDateTime::parse_from_str(&row.get::<String, _>("created_at"), "%s")
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "could not parse Unix timestamp from the database: '{e}'"
                    ))
                })?,
            source: row.get::<String, _>("source"),
            action: row.get::<String, _>("action"),
            content: row.get::<String, _>("content"),
        };

        Ok(myself)
    }
}

struct EventPersisterProvider<'conn> {
    connection: &'conn Connection,
    projection: Projection,
}

impl<'conn> EventPersisterProvider<'conn> {
    pub fn new(connection: &'conn Connection) -> Self {
        Self {
            connection,
            projection: Event::get_projection(),
        }
    }
}

impl<'conn> Provider<'conn> for EventPersisterProvider<'conn> {
    type Entity = Event;

    fn get_connection(&'conn self) -> &'conn Connection {
        self.connection
    }

    fn get_projection(&self) -> &Projection {
        &self.projection
    }

    fn get_definition(&self, _condition: Option<&str>) -> String {
        let projection = self.get_projection().expand(HashMap::new());

        format!(
            r#"insert into event (source, action, content) values (?1, ?2, ?3) returning {projection}"#
        )
    }
}

/// The EventPersister is the adapter to persist EventMessage turning them into
/// Event.
pub struct EventPersister {
    connection: Arc<Mutex<Connection>>,
}

impl EventPersister {
    /// Instanciate an EventPersister
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }

    /// Save an EventMessage in the database.
    pub fn persist(&self, message: EventMessage) -> Result<Event, Box<dyn Error>> {
        let connection = &*self.connection.lock().unwrap();
        let provider = EventPersisterProvider::new(connection);
        let log_message = message.clone();
        let mut rows = provider.find(
            None,
            &[
                sqlite::Value::String(message.source),
                sqlite::Value::String(message.action),
                sqlite::Value::String(message.content),
            ],
        )?;

        rows.next().ok_or_else(|| {
            format!("No record from the database after I saved event message {log_message:?}")
                .into()
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn provider_sql() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = EventPersisterProvider::new(&connection);
        assert_eq!(
            r#"insert into event (source, action, content) values (?1, ?2, ?3) returning event_id as event_id, unixepoch(created_at) as created_at, source as source, action as action, content as content"#,
            provider.get_definition(None)
        )
    }
}
