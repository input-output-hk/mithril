use serde::Serialize;
use slog::{warn, Logger};
use std::fmt::Debug;
use tokio::sync::mpsc::UnboundedSender;

use mithril_common::logging::LoggerExtensions;

use super::EventMessage;

/// The transmitter service is used to allow inter process channel
/// communication. This service is used to create multiple transmitters.
pub struct TransmitterService<MSG>
where
    MSG: Debug + Sync + Send,
{
    transmitter: UnboundedSender<MSG>,
    logger: Logger,
}

impl<MSG> TransmitterService<MSG>
where
    MSG: Debug + Sync + Send,
{
    /// Instantiate a new Service by passing a MPSC transmitter.
    pub fn new(transmitter: UnboundedSender<MSG>, logger: Logger) -> Self {
        Self {
            transmitter,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Clone the internal transmitter and return it.
    pub fn get_transmitter(&self) -> UnboundedSender<MSG> {
        self.transmitter.clone()
    }
}

impl TransmitterService<EventMessage> {
    /// Craft and send an [EventMessage] given the serializable data.
    /// This method is done in a way to make as simple as possible to send a
    /// message and make any error not to cause a business failure. A warning is
    /// issued so the resulting error may be discarded.
    pub fn send_event_message<T>(
        &self,
        source: &str,
        action: &str,
        content: &T,
        headers: Vec<(&str, &str)>,
    ) -> Result<(), String>
    where
        T: Serialize,
    {
        let message = EventMessage::create(source, action, content, headers);
        self.send(message)
    }

    pub fn send(&self, message: EventMessage) -> Result<(), String> {
        self.get_transmitter().send(message.clone()).map_err(|e| {
            let error_msg =
                format!("An error occurred when sending message {message:?} to monitoring: '{e}'.");
            warn!(self.logger, "Event message error"; "error" => &error_msg);

            error_msg
        })
    }
}
