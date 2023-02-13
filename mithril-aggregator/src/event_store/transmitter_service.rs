use std::fmt::Debug;

use tokio::sync::mpsc::UnboundedSender;

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
