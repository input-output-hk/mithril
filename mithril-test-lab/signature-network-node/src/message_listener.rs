use std::sync::Arc;

use slog::info;
use tokio::sync::{mpsc, Mutex};

use mithril_common::messages::RegisterSignatureMessage;

use crate::entities::Message;

/// Listens for messages from a mpsc::receiver and pushes signature registration to the
/// available signatures queue
pub struct MessageListener {
    listening_channel: mpsc::Receiver<Message>,
    available_signatures_registrations: Arc<Mutex<Vec<RegisterSignatureMessage>>>,
    logger: slog::Logger,
}

impl MessageListener {
    pub fn new(
        listening_channel: mpsc::Receiver<Message>,
        available_signatures_registrations: Arc<Mutex<Vec<RegisterSignatureMessage>>>,
        parent_logger: &slog::Logger,
    ) -> Self {
        Self {
            listening_channel,
            available_signatures_registrations,
            logger: parent_logger.new(slog::o!("src" => "message_listener")),
        }
    }

    pub async fn listen(&mut self) {
        loop {
            match self.listening_channel.recv().await {
                Some(Message::MithrilRegisterSignature(message)) => {
                    let mut available_signatures_registrations =
                        self.available_signatures_registrations.lock().await;
                    available_signatures_registrations.push(message);
                }
                // Some(msg) => {
                //     info!(self.logger, "Unsupported message: {msg:?}");
                // }
                None => {
                    info!(self.logger, "Channel closed");
                    break;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entities::Message;
    use crate::tests::TestLogger;

    #[tokio::test]
    async fn input_folder_listener_push_signatures_messages_to_available_sig_queue() {
        let (tx, rx) = mpsc::channel(1);
        let available_signatures_registrations = Arc::new(Mutex::new(Vec::new()));
        let mut listener = MessageListener::new(
            rx,
            available_signatures_registrations.clone(),
            &TestLogger::stdout(),
        );

        tokio::spawn(async move {
            listener.listen().await;
        });

        // No messages should have been notified yet
        assert_eq!(
            Vec::<RegisterSignatureMessage>::new(),
            *available_signatures_registrations.lock().await
        );

        tx.send(Message::MithrilRegisterSignature(
            RegisterSignatureMessage::dummy(),
        ))
        .await
        .unwrap();

        // Wait for the message to be notified
        tokio::task::yield_now().await;

        assert_eq!(
            vec![RegisterSignatureMessage::dummy()],
            *available_signatures_registrations.lock().await
        );
    }
}
