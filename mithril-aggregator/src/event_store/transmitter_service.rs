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
    /// Send an [EventMessage].
    pub fn try_send(
        &self,
        message: EventMessage,
    ) -> Result<(), tokio::sync::mpsc::error::SendError<EventMessage>> {
        self.get_transmitter().send(message)
    }

    /// Send an [EventMessage].
    ////
    /// An error when sending a message has no impact on the business.
    /// If there is one, a warning is issued so the resulting error may be safely ignored by the caller.
    pub fn send(&self, message: EventMessage) {
        if let Err(e) = self.try_send(message.clone()) {
            let error_msg =
                format!("An error occurred when sending message {message:?} to monitoring: '{e}'.");
            warn!(self.logger, "Event message error"; "error" => &error_msg);
        };
    }
}
