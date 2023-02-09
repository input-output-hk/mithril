use slog_scope::info;
use tokio::sync::mpsc::UnboundedReceiver;

use std::fmt::Debug;

pub struct EventStore<EVENT>
where
    EVENT: Debug + Sync + Send,
{
    receiver: UnboundedReceiver<EVENT>,
}

impl<EVENT> EventStore<EVENT>
where
    EVENT: Debug + Sync + Send,
{
    pub fn new(receiver: UnboundedReceiver<EVENT>) -> Self {
        Self { receiver }
    }

    pub async fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        loop {
            if let Some(event) = self.receiver.recv().await {
                info!("Event received: {event:?}");
            } else {
                info!("No more events to proceed, quitting…");
                break;
            }
        }

        Ok(())
    }
}
