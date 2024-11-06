use chrono::{DateTime, Utc};
use mithril_common::entities::SignerWithStake;
use serde::Serialize;

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
    pub fn new<T>(source: &str, action: &str, content: &T, headers: Vec<(&str, &str)>) -> Self
    where
        T: Serialize,
    {
        let content = serde_json::json!(content);

        EventMessage {
            source: source.to_string(),
            action: action.to_string(),
            content,
            headers: headers
                .into_iter()
                .map(|(h, v)| (h.to_string(), v.to_string()))
                .collect(),
        }
    }

    /// Create a signer registration event message.
    pub fn signer_registration(
        source: &str,
        signer_with_stake: &SignerWithStake,
        signer_node_version: Option<String>,
        epoch_str: &str,
    ) -> Self {
        let mut headers: Vec<(&str, &str)> = match signer_node_version.as_ref() {
            Some(version) => vec![("signer-node-version", version)],
            None => Vec::new(),
        };

        if !epoch_str.is_empty() {
            headers.push(("epoch", epoch_str));
        }

        Self::new::<SignerWithStake>(source, "register_signer", signer_with_stake, headers)
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
