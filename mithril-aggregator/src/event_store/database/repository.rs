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
            "No record from the database after saving event message {log_message:?}"
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

    mod metrics_per_day_view {
        use std::time::Duration;

        use crate::event_store::database::test_helper::event_store_db_connection;
        use chrono::{DateTime, Utc};
        use serde::{Deserialize, Serialize};

        use mithril_common::StdResult;

        use sqlite::ConnectionThreadSafe;

        use super::*;
        #[derive(Serialize, Deserialize)]
        struct MetricMessage {
            counter: i64,
            duration: Duration,
            date: DateTime<Utc>,
        }

        fn get_all_metrics(
            connection: Arc<ConnectionThreadSafe>,
        ) -> StdResult<Vec<(String, String, i64)>> {
            let query = "SELECT date, counter_name, value FROM metrics_per_day";
            let mut statement = connection.prepare(query)?;
            let mut result = Vec::new();
            while let Ok(sqlite::State::Row) = statement.next() {
                result.push((
                    statement.read::<String, _>("date")?,
                    statement.read::<String, _>("counter_name")?,
                    statement.read::<i64, _>("value")?,
                ));
            }

            Ok(result)
        }

        /// Insert a metric evnet in the database.
        /// date format is "%Y-%m-%d %H:%M:%S %z", example: "2015-09-05 23:56:04 +0000"
        fn insert_metric_event(
            persister: &EventPersister,
            date: &str,
            metric_name: &str,
            value: i64,
        ) {
            let metric_date =
                DateTime::parse_from_str(&format!("{date} +0000"), "%Y-%m-%d %H:%M:%S %z").unwrap();

            let message = EventMessage::new(
                "Metrics",
                metric_name,
                serde_json::json!(MetricMessage {
                    counter: value,
                    duration: Duration::from_secs(3),
                    date: metric_date.into(),
                }),
            );

            let _event = persister.persist(message).unwrap();
        }

        #[test]
        fn retrieved_inserted_event() {
            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());
            insert_metric_event(&persister, "2024-10-29 23:56:04", "metric_1", 15);

            let result = get_all_metrics(connection).unwrap();

            assert!(result.contains(&("2024-10-29".to_string(), "metric_1".to_string(), 15)));
        }

        #[test]
        fn sum_metric_per_day() {
            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());
            insert_metric_event(&persister, "2024-10-29 21:00:00", "metric_1", 15);
            insert_metric_event(&persister, "2024-10-29 22:00:00", "metric_1", 60);
            insert_metric_event(&persister, "2024-10-29 23:00:00", "metric_2", 100);
            insert_metric_event(&persister, "2024-10-30 17:00:00", "metric_1", 12);
            insert_metric_event(&persister, "2024-10-30 18:00:00", "metric_1", 4);

            let result = get_all_metrics(connection).unwrap();

            assert!(result.contains(&("2024-10-29".to_string(), "metric_1".to_string(), 75)));
            assert!(result.contains(&("2024-10-29".to_string(), "metric_2".to_string(), 100)));
            assert!(result.contains(&("2024-10-30".to_string(), "metric_1".to_string(), 16)));
        }
    }
}
