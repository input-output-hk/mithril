use std::fmt::Debug;

use tokio::sync::mpsc::UnboundedSender;

pub struct TransmitterService<EVENT>
where
    EVENT: Debug + Sync + Send,
{
    transmitter: UnboundedSender<EVENT>,
}

impl<EVENT> TransmitterService<EVENT>
where
    EVENT: Debug + Sync + Send,
{
    pub fn new(transmitter: UnboundedSender<EVENT>) -> Self {
        Self { transmitter }
    }

    pub fn get_transmitter(&self) -> UnboundedSender<EVENT> {
        self.transmitter.clone()
    }
}
