use chrono::{DateTime, Utc};

use std::collections::HashMap;

/// Event that is sent from a thread to be persisted.
#[derive(Debug, Clone, PartialEq)]
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
    pub content: serde_json::Value,

    /// Headers
    pub headers: HashMap<String, String>,
}

impl EventMessage {
    /// Instantiate a new EventMessage.
    pub fn new(source: &str, action: &str, content: serde_json::Value) -> Self {
        Self {
            source: source.to_string(),
            action: action.to_string(),
            content,
            headers: HashMap::new(),
        }
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
