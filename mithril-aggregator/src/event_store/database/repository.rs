//! Migration module
//!
use anyhow::anyhow;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};

use std::sync::Arc;

use crate::event_store::database::query::InsertEventQuery;
use crate::event_store::{event::Event, EventMessage};
/// The EventPersister is the adapter to persist EventMessage turning them into
/// Event.
pub struct EventPersister {
    connection: Arc<SqliteConnection>,
}

impl EventPersister {
    /// Instantiate an EventPersister
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }

    /// Save an EventMessage in the database.
    pub fn persist(&self, message: EventMessage) -> StdResult<Event> {
        let log_message = message.clone();
        let mut rows = self.connection.fetch(InsertEventQuery::one(message)?)?;

        rows.next().ok_or(anyhow!(
            "No record from the database after I saved event message {log_message:?}"
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::event_store::database::test_helper::event_store_db_connection;
    use mithril_common::StdResult;

    #[test]
    fn can_persist_event() -> StdResult<()> {
        let connection = Arc::new(event_store_db_connection().unwrap());

        let persister = EventPersister::new(connection);
        let message = EventMessage::new("source", "action", serde_json::json!("content"));

        let _event = persister.persist(message)?;
        Ok(())
    }

    #[test]
    fn migration_executed_running_database() -> StdResult<()> {
        let connection = Arc::new(event_store_db_connection().unwrap());

        let persister = EventPersister::new(connection);
        let message = EventMessage::new("source", "action", serde_json::json!("content"));

        let _event = persister.persist(message)?;
        Ok(())
    }
}
