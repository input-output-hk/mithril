use anyhow::Context;
use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;
use slog::{debug, info, Logger};
use sqlite::ConnectionThreadSafe;
use std::sync::Arc;
use tokio::sync::mpsc::UnboundedReceiver;

use super::database::EventPersister;
use super::EventMessage;

/// EventMessage receiver service.
pub struct EventStore {
    receiver: UnboundedReceiver<EventMessage>,
    connection: Arc<ConnectionThreadSafe>,
    logger: Logger,
}

impl EventStore {
    /// Instantiate the EventMessage receiver service.
    pub fn new(
        receiver: UnboundedReceiver<EventMessage>,
        connection: Arc<ConnectionThreadSafe>,
        logger: Logger,
    ) -> Self {
        Self {
            receiver,
            connection,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Launch the service. It runs until all the transmitters are gone and all
    /// messages have been processed. This means this service shall be waited
    /// upon completion to ensure all events are properly saved in the database.
    pub async fn run(&mut self) -> StdResult<()> {
        let connection = self.connection.clone();
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
