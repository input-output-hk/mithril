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
        let message = EventMessage::new("source", "action", &"content".to_string(), Vec::new());

        let _event = persister.persist(message)?;
        Ok(())
    }

    #[test]
    fn migration_executed_running_database() -> StdResult<()> {
        let connection = Arc::new(event_store_db_connection().unwrap());

        let persister = EventPersister::new(connection);
        let message = EventMessage::new("source", "action", &"content".to_string(), Vec::new());

        let _event = persister.persist(message)?;
        Ok(())
    }

    mod metrics_per_day_view {

        use std::time::Duration;

        use crate::{
            event_store::database::test_helper::event_store_db_connection, services::UsageReporter,
        };
        use chrono::{DateTime, Utc};

        use mithril_common::StdResult;

        use serde::{Deserialize, Serialize};
        use sqlite::ConnectionThreadSafe;

        use super::*;

        fn get_all_metrics(
            connection: Arc<ConnectionThreadSafe>,
        ) -> StdResult<Vec<(String, String, i64)>> {
            let query = "select date, counter_name, value from metrics_per_day";
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

        fn get_all_metrics_by_origin(
            connection: Arc<ConnectionThreadSafe>,
        ) -> StdResult<Vec<(String, String, String, i64)>> {
            let query = "select date, counter_name, origin, value from metrics_per_day_and_origin";
            let mut statement = connection.prepare(query)?;
            let mut result = Vec::new();
            while let Ok(sqlite::State::Row) = statement.next() {
                result.push((
                    statement.read::<String, _>("date")?,
                    statement.read::<String, _>("counter_name")?,
                    statement.read::<Option<String>, _>("origin")?.unwrap_or_default(),
                    statement.read::<i64, _>("value")?,
                ));
            }

            Ok(result)
        }

        fn get_all_metrics_by_client_type(
            connection: Arc<ConnectionThreadSafe>,
        ) -> StdResult<Vec<(String, String, String, i64)>> {
            let query = "select date, counter_name, client_type, value from metrics_per_day_and_client_type";
            let mut statement = connection.prepare(query)?;
            let mut result = Vec::new();
            while let Ok(sqlite::State::Row) = statement.next() {
                result.push((
                    statement.read::<String, _>("date")?,
                    statement.read::<String, _>("counter_name")?,
                    statement
                        .read::<Option<String>, _>("client_type")?
                        .unwrap_or_default(),
                    statement.read::<i64, _>("value")?,
                ));
            }

            Ok(result)
        }

        /// Insert a metric event in the database.
        /// date format is "%Y-%m-%d %H:%M:%S %z", example: "2015-09-05 23:56:04 +0000"
        fn insert_metric_event_with_origin(
            persister: &EventPersister,
            date: &str,
            metric_name: &str,
            origin: &str,
            value: i64,
        ) {
            let metric_date =
                DateTime::parse_from_str(&format!("{date} +0000"), "%Y-%m-%d %H:%M:%S %z").unwrap();

            let message = UsageReporter::create_metrics_event_message(
                metric_name.to_string(),
                value,
                Duration::from_secs(5),
                origin.to_string(),
                "CLIENT_TYPE_A".to_string(),
                metric_date.into(),
            );

            let _event = persister.persist(message).unwrap();
        }

        fn insert_metric_event_message(
            persister: &EventPersister,
            date: &str,
            metric_name: &str,
            origin: &str,
            client_type: &str,
            value: i64,
        ) {
            let metric_date =
                DateTime::parse_from_str(&format!("{date} +0000"), "%Y-%m-%d %H:%M:%S %z").unwrap();

            let message = UsageReporter::create_metrics_event_message(
                metric_name.to_string(),
                value,
                Duration::from_secs(5),
                origin.to_string(),
                client_type.to_string(),
                metric_date.into(),
            );

            let _event = persister.persist(message).unwrap();
        }

        /// Insert a metric event with the old format (without origin) in the database.
        fn insert_metric_event(
            persister: &EventPersister,
            date: &str,
            metric_name: &str,
            value: i64,
        ) {
            #[derive(Serialize, Deserialize)]
            struct OldMetricEventMessage {
                name: String,
                value: i64,
                period: Duration,
                date: DateTime<Utc>,
            }

            let metric_date =
                DateTime::parse_from_str(&format!("{date} +0000"), "%Y-%m-%d %H:%M:%S %z").unwrap();

            let message = EventMessage::new(
                "Metrics",
                metric_name,
                &OldMetricEventMessage {
                    name: metric_name.to_string(),
                    value,
                    period: Duration::from_secs(5),
                    date: metric_date.into(),
                },
                vec![],
            );

            let _event = persister.persist(message).unwrap();
        }

        #[test]
        fn retrieved_inserted_event() {
            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());
            insert_metric_event_with_origin(
                &persister,
                "2024-10-29 23:56:04",
                "metric_1",
                "ORIGIN",
                15,
            );

            let result = get_all_metrics(connection).unwrap();

            assert!(result.contains(&("2024-10-29".to_string(), "metric_1".to_string(), 15)));
        }

        #[test]
        fn sum_metric_per_day() {
            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());
            insert_metric_event_with_origin(
                &persister,
                "2024-10-29 21:00:00",
                "metric_1",
                "ORIGIN_A",
                15,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-29 22:00:00",
                "metric_1",
                "ORIGIN_B",
                60,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-29 23:00:00",
                "metric_2",
                "ORIGIN",
                100,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-30 17:00:00",
                "metric_1",
                "ORIGIN_A",
                12,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-30 18:00:00",
                "metric_1",
                "ORIGIN_B",
                4,
            );

            let result = get_all_metrics(connection).unwrap();

            assert!(result.contains(&("2024-10-29".to_string(), "metric_1".to_string(), 75)));
            assert!(result.contains(&("2024-10-29".to_string(), "metric_2".to_string(), 100)));
            assert!(result.contains(&("2024-10-30".to_string(), "metric_1".to_string(), 16)));
        }

        #[test]
        fn sum_metric_per_day_and_origin() {
            fn tuple_with_str(t: &(String, String, String, i64)) -> (&str, &str, &str, i64) {
                (t.0.as_str(), t.1.as_str(), t.2.as_str(), t.3)
            }

            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());
            insert_metric_event_with_origin(
                &persister,
                "2024-10-29 21:00:00",
                "metric_1",
                "ORIGIN_A",
                15,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-29 22:00:00",
                "metric_1",
                "ORIGIN_B",
                60,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-29 23:00:00",
                "metric_2",
                "ORIGIN",
                100,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-30 17:00:00",
                "metric_1",
                "ORIGIN_A",
                12,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-30 18:00:00",
                "metric_1",
                "ORIGIN_B",
                4,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-30 17:00:00",
                "metric_1",
                "ORIGIN_A",
                15,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-30 18:00:00",
                "metric_1",
                "ORIGIN_B",
                3,
            );

            let result = get_all_metrics_by_origin(connection).unwrap();
            let result: Vec<_> = result.iter().map(tuple_with_str).collect();

            assert!(result.contains(&("2024-10-29", "metric_1", "ORIGIN_A", 15)));
            assert!(result.contains(&("2024-10-29", "metric_1", "ORIGIN_B", 60)));
            assert!(result.contains(&("2024-10-29", "metric_2", "ORIGIN", 100)));
            assert!(result.contains(&("2024-10-30", "metric_1", "ORIGIN_A", 27)));
            assert!(result.contains(&("2024-10-30", "metric_1", "ORIGIN_B", 7)));
        }

        #[test]
        fn vue_metrics_per_day_and_client_type() {
            fn tuple_with_str(t: &(String, String, String, i64)) -> (&str, &str, &str, i64) {
                (t.0.as_str(), t.1.as_str(), t.2.as_str(), t.3)
            }

            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());

            insert_metric_event_message(
                &persister,
                "2024-10-29 21:00:00",
                "metric_1",
                "ORIGIN_A",
                "CLIENT_TYPE_A",
                15,
            );

            let result = get_all_metrics_by_client_type(connection).unwrap();
            let result: Vec<_> = result.iter().map(tuple_with_str).collect();

            assert!(result.contains(&("2024-10-29", "metric_1", "CLIENT_TYPE_A", 15)));
        }

        #[test]
        fn sum_metric_per_day_and_origin_on_old_event() {
            fn tuple_with_str(t: &(String, String, String, i64)) -> (&str, &str, &str, i64) {
                (t.0.as_str(), t.1.as_str(), t.2.as_str(), t.3)
            }

            let connection = Arc::new(event_store_db_connection().unwrap());

            let persister = EventPersister::new(connection.clone());
            insert_metric_event(&persister, "2024-10-29 21:00:00", "metric_1", 15);
            insert_metric_event(&persister, "2024-10-29 22:00:00", "metric_1", 60);
            insert_metric_event(&persister, "2024-10-29 23:00:00", "metric_2", 100);
            insert_metric_event(&persister, "2024-10-30 17:00:00", "metric_1", 12);
            insert_metric_event(&persister, "2024-10-30 18:00:00", "metric_1", 4);
            insert_metric_event(&persister, "2024-10-30 17:00:00", "metric_1", 15);
            insert_metric_event(&persister, "2024-10-30 18:00:00", "metric_1", 3);

            let result = get_all_metrics_by_origin(connection).unwrap();
            let result: Vec<_> = result.iter().map(tuple_with_str).collect();

            assert!(result.contains(&("2024-10-29", "metric_1", "", 75)));
            assert!(result.contains(&("2024-10-29", "metric_2", "", 100)));
            assert!(result.contains(&("2024-10-30", "metric_1", "", 34)));
        }

        #[test]
        fn sum_metric_per_day_and_origin_with_old_and_new_format() {
            fn tuple_with_str(t: &(String, String, String, i64)) -> (&str, &str, &str, i64) {
                (t.0.as_str(), t.1.as_str(), t.2.as_str(), t.3)
            }

            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());
            insert_metric_event_with_origin(
                &persister,
                "2024-10-29 21:00:00",
                "metric_1",
                "ORIGIN_A",
                15,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-29 22:00:00",
                "metric_1",
                "ORIGIN_B",
                60,
            );
            insert_metric_event_with_origin(
                &persister,
                "2024-10-29 23:00:00",
                "metric_1",
                "ORIGIN_B",
                20,
            );
            insert_metric_event(&persister, "2024-10-29 22:00:00", "metric_1", 23);
            insert_metric_event(&persister, "2024-10-29 23:00:00", "metric_1", 31);

            let result = get_all_metrics_by_origin(connection).unwrap();
            let result: Vec<_> = result.iter().map(tuple_with_str).collect();

            assert!(result.contains(&("2024-10-29", "metric_1", "ORIGIN_A", 15)));
            assert!(result.contains(&("2024-10-29", "metric_1", "ORIGIN_B", 80)));
            assert!(result.contains(&("2024-10-29", "metric_1", "", 54)));
        }
    }

    mod signer_registration_summary {
        use std::sync::Arc;

        use crate::event_store::database::test_helper::event_store_db_connection;
        use mithril_common::entities::{SignerWithStake, Stake};
        use mithril_common::{test_utils::fake_data, StdResult};
        use sqlite::ConnectionThreadSafe;

        use super::{EventMessage, EventPersister};

        /// Insert a signer registration event in the database.
        fn insert_registration_event(
            persister: &EventPersister,
            epoch: &str,
            party_id: &str,
            stake: Stake,
            signer_node_version: &str,
        ) {
            let signers = fake_data::signers_with_stakes(1);
            let signer = SignerWithStake {
                party_id: party_id.to_string(),
                stake,
                ..signers[0].clone()
            };

            let message = EventMessage::signer_registration(
                "Test",
                &signer,
                Some(signer_node_version.to_string()),
                epoch,
            );

            let _event = persister.persist(message).unwrap();
        }

        #[derive(PartialEq)]
        struct StakeSignerVersion {
            epoch: i64,
            version: String,
            total_epoch_stakes: i64,
            stakes_version: i64,
            stakes_ratio: String,
            pool_count: i64,
        }
        impl StakeSignerVersion {
            fn new(
                epoch: i64,
                version: &str,
                total_epoch_stakes: i64,
                stakes_version: i64,
                stakes_ratio: &str,
                pool_count: i64,
            ) -> Self {
                Self {
                    epoch,
                    version: version.to_string(),
                    total_epoch_stakes,
                    stakes_version,
                    stakes_ratio: stakes_ratio.to_string(),
                    pool_count,
                }
            }
        }

        fn get_all_registrations(
            connection: Arc<ConnectionThreadSafe>,
        ) -> StdResult<Vec<StakeSignerVersion>> {
            let query = "select
                    epoch,
                    version,
                    total_epoch_stakes,
                    stakes_version,
                    stakes_ratio,
                    pool_count 
                from signer_registration_summary;";
            let mut statement = connection.prepare(query)?;
            let mut result = Vec::new();
            while let Ok(sqlite::State::Row) = statement.next() {
                result.push(StakeSignerVersion::new(
                    statement.read::<i64, _>("epoch")?,
                    &statement.read::<String, _>("version")?,
                    statement.read::<i64, _>("total_epoch_stakes")?,
                    statement.read::<i64, _>("stakes_version")?,
                    &statement.read::<String, _>("stakes_ratio")?,
                    statement.read::<i64, _>("pool_count")?,
                ));
            }

            Ok(result)
        }

        #[test]
        fn retrieved_node_version() {
            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());

            insert_registration_event(&persister, "3", "A", 15, "0.2.234");
            insert_registration_event(&persister, "4", "A", 15, "15.24.32");
            insert_registration_event(&persister, "5", "A", 15, "0.4.789+ef0c28a");

            let result = get_all_registrations(connection).unwrap();

            assert!(result.contains(&StakeSignerVersion::new(3, "0.2.234", 15, 15, "100 %", 1)));
            assert!(result.contains(&StakeSignerVersion::new(4, "15.24.32", 15, 15, "100 %", 1)));
            assert!(result.contains(&StakeSignerVersion::new(5, "0.4.789", 15, 15, "100 %", 1)));
        }

        #[test]
        fn retrieved_total_by_epoch() {
            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());

            insert_registration_event(&persister, "8", "A", 20, "1.0.2");
            insert_registration_event(&persister, "8", "B", 15, "1.0.2");
            insert_registration_event(&persister, "9", "A", 56, "1.0.2");
            insert_registration_event(&persister, "9", "B", 31, "1.0.2");
            let result = get_all_registrations(connection).unwrap();

            assert!(result.contains(&StakeSignerVersion::new(8, "1.0.2", 35, 35, "100 %", 2)));
            assert!(result.contains(&StakeSignerVersion::new(9, "1.0.2", 87, 87, "100 %", 2)));
        }

        #[test]
        fn retrieved_percentage_per_version() {
            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());

            insert_registration_event(&persister, "8", "A", 90, "1.0.2");
            insert_registration_event(&persister, "8", "B", 30, "1.0.2");
            insert_registration_event(&persister, "8", "C", 80, "1.0.4");
            let result = get_all_registrations(connection).unwrap();

            assert!(result.contains(&StakeSignerVersion::new(8, "1.0.2", 200, 120, "60 %", 2)));
            assert!(result.contains(&StakeSignerVersion::new(8, "1.0.4", 200, 80, "40 %", 1)));
        }

        #[test]
        fn retrieved_percentage_per_epoch() {
            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());

            insert_registration_event(&persister, "8", "A", 6, "1.0.2");
            insert_registration_event(&persister, "8", "B", 4, "1.0.4");
            insert_registration_event(&persister, "9", "A", 28, "1.0.2");
            insert_registration_event(&persister, "9", "B", 12, "1.0.4");
            let result = get_all_registrations(connection).unwrap();

            assert!(result.contains(&StakeSignerVersion::new(8, "1.0.2", 10, 6, "60 %", 1)));
            assert!(result.contains(&StakeSignerVersion::new(8, "1.0.4", 10, 4, "40 %", 1)));
            assert!(result.contains(&StakeSignerVersion::new(9, "1.0.2", 40, 28, "70 %", 1)));
            assert!(result.contains(&StakeSignerVersion::new(9, "1.0.4", 40, 12, "30 %", 1)));
        }

        #[test]
        fn with_multi_registrations_for_an_epoch_only_the_last_recorded_one_is_retained() {
            let connection = Arc::new(event_store_db_connection().unwrap());
            let persister = EventPersister::new(connection.clone());

            insert_registration_event(&persister, "8", "A", 6, "1.0.2");
            insert_registration_event(&persister, "8", "A", 8, "1.0.2");
            insert_registration_event(&persister, "8", "A", 10, "1.0.4");
            insert_registration_event(&persister, "8", "A", 7, "1.0.3");

            let result = get_all_registrations(connection).unwrap();

            assert!(result.contains(&StakeSignerVersion::new(8, "1.0.3", 7, 7, "100 %", 1)));
            assert!(result.len() == 1);
        }
    }
}
