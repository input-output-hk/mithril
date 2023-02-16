use std::fmt::Debug;

use serde::Serialize;
use slog_scope::warn;
use tokio::sync::mpsc::UnboundedSender;

use super::EventMessage;

/// The transmitter service is used to allow inter process channel
/// communication. This service is used to create multiple transmitters.
pub struct TransmitterService<MSG>
where
    MSG: Debug + Sync + Send,
{
    transmitter: UnboundedSender<MSG>,
}

impl<MSG> TransmitterService<MSG>
where
    MSG: Debug + Sync + Send,
{
    /// Instanciate a new Service by passing a MPSC transmitter.
    pub fn new(transmitter: UnboundedSender<MSG>) -> Self {
        Self { transmitter }
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
        let content = serde_json::to_string(content).map_err(|e| {
            let error_msg = format!("Serialization error while forging event message: {e}");
            warn!("Event message error => «{error_msg}»");

            error_msg
        })?;
        let message = EventMessage {
            source: source.to_string(),
            action: action.to_string(),
            content,
            headers: headers
                .into_iter()
                .map(|(h, v)| (h.to_string(), v.to_string()))
                .collect(),
        };
        self.get_transmitter().send(message.clone()).map_err(|e| {
            let error_msg =
                format!("An error occured when sending message {message:?} to monitoring: '{e}'.");
            warn!("Event message error => «{error_msg}»");

            error_msg
        })
    }
}
