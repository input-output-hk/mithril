use std::path::PathBuf;

use slog::info;
use tokio::sync::mpsc;

use mithril_common::messages::RegisterSignatureMessage;

use crate::entities::Message;

/// Listens for messages from a mpsc::receiver write them json serialized to all target_directories
pub struct MessageSender {
    listening_channel: mpsc::Receiver<Message>,
    target_directories: Vec<PathBuf>,
    logger: slog::Logger,
}

impl MessageSender {
    pub fn new(
        listening_channel: mpsc::Receiver<Message>,
        peer_input_directories: Vec<PathBuf>,
    ) -> Self {
        Self {
            listening_channel,
            target_directories: peer_input_directories,
            logger: slog_scope::logger().new(slog::o!("src" => "message_sender")),
        }
    }

    pub async fn listen(&mut self) {
        loop {
            match self.listening_channel.recv().await {
                Some(Message::MithrilRegisterSignature(message)) => {
                    todo!()
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
    use std::path::Path;

    use walkdir::WalkDir;

    use mithril_common::test_utils::TempDir;

    use crate::entities::Message;

    use super::*;

    fn list_directory(path: &Path) -> Vec<PathBuf> {
        WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| e.path().is_file())
            .filter_map(|e| e.map(|f| f.into_path()).ok())
            .collect()
    }

    #[tokio::test]
    async fn write_received_messages_to_one_peer() {
        let dir = TempDir::create(
            "signature-network-node-message-sender",
            "write_received_messages_to_one_peer",
        );
        let target_dir = dir.join("peer-1");
        std::fs::create_dir(&target_dir).unwrap();
        let (tx, rx) = mpsc::channel(1);
        let mut sender = MessageSender::new(rx, vec![target_dir.clone()]);

        tokio::spawn(async move {
            sender.listen().await;
        });

        // No messages should have been written yet
        assert_eq!(Vec::<PathBuf>::new(), list_directory(&target_dir));

        let message = RegisterSignatureMessage::dummy();
        tx.send(Message::MithrilRegisterSignature(message.clone()))
            .await
            .unwrap();

        // Wait for the message to be notified
        tokio::task::yield_now().await;

        assert_eq!(
            vec![PathBuf::from("register_signature.json")],
            list_directory(&target_dir)
        );
    }
}
