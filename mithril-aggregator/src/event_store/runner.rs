use anyhow::Context;
use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;
use mithril_persistence::sqlite::ConnectionBuilder;
use slog::{debug, info, Logger};
use sqlite::ConnectionThreadSafe;
use std::{path::PathBuf, sync::Arc};
use tokio::sync::mpsc::UnboundedReceiver;

use crate::dependency_injection::DependenciesBuilderError;

use super::{EventMessage, EventPersister};

/// EventMessage receiver service.
pub struct EventStore {
    receiver: UnboundedReceiver<EventMessage>,
    logger: Logger,
}

impl EventStore {
    /// Instantiate the EventMessage receiver service.
    pub fn new(receiver: UnboundedReceiver<EventMessage>, logger: Logger) -> Self {
        Self {
            receiver,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    fn get_sqlite_connection(&self, file: Option<PathBuf>) -> StdResult<ConnectionThreadSafe> {
        let connection_builder = match file {
            Some(path) => ConnectionBuilder::open_file(path.as_path()),
            None => ConnectionBuilder::open_memory(),
        };

        // TODO How can we check the database is correctly created with correct options ?
        // TODO should we use the same function to create database for tests ?
        let connection = connection_builder
            // .with_node_type(ApplicationNodeType::Aggregator)
            // .with_options(&[
            //     ConnectionOptions::EnableForeignKeys,
            //     ConnectionOptions::EnableWriteAheadLog,
            // ])
            // .with_logger(logger.clone())
            .with_migrations(crate::event_store::migration::get_migrations())
            .build()
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "SQLite initialization: failed to build connection.".to_string(),
                error: Some(e),
            })?;
        Ok(connection)
    }

    /// Launch the service. It runs until all the transmitters are gone and all
    /// messages have been processed. This means this service shall be waited
    /// upon completion to ensure all events are properly saved in the database.
    pub async fn run(&mut self, file: Option<PathBuf>) -> StdResult<()> {
        let connection = Arc::new(self.get_sqlite_connection(file)?);
        let persister = EventPersister::new(connection);
        info!(
            self.logger,
            "Starting monitoring event loop to log messages."
        );
        loop {
            if let Some(message) = self.receiver.recv().await {
                debug!(self.logger, "Event received"; "event" => ?message);
                let event = persister
                    .persist(message)
                    .with_context(|| "event persist failure")?;
                debug!(self.logger, "Event ID={} created", event.event_id);
            } else {
                info!(self.logger, "No more events to proceed, quitting…");
                break;
            }
        }

        Ok(())
    }
}
