use anyhow::Context;
use slog::{debug, info, warn};
use std::path::{Path, PathBuf};
use tokio::fs::File;
use tokio::io::{AsyncWriteExt, BufWriter};
use tokio::sync::mpsc;

use mithril_common::StdResult;

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
        parent_logger: &slog::Logger,
    ) -> Self {
        Self {
            listening_channel,
            target_directories: peer_input_directories,
            logger: parent_logger.new(slog::o!("src" => "message_sender")),
        }
    }

    pub async fn listen(&mut self) {
        info!(self.logger, "Listening for messages to forward to peers");

        loop {
            match self.listening_channel.recv().await {
                Some(message) => {
                    info!(self.logger, "Received message: {:?}", message);

                    for target_dir in &self.target_directories {
                        match Self::write_message(&message, target_dir).await {
                            Ok(file_path) => {
                                debug!(self.logger, "Message written to file"; "file_path" => file_path.display());
                            }
                            Err(error) => {
                                warn!(self.logger, "Failed to write message: {error:?}");
                            }
                        }
                    }
                }
                None => {
                    info!(self.logger, "Channel closed");
                    break;
                }
            }
        }
    }

    async fn write_message(message: &Message, target_dir: &Path) -> StdResult<PathBuf> {
        let file_path = target_dir
            .join(message.file_identifier())
            .with_extension("json");

        let file = File::create_new(&file_path)
            .await
            .with_context(|| format!("Failed to create file: {:?}", file_path.display()))?;
        let mut writer = BufWriter::new(file);
        let json = serde_json::to_string(message).with_context(|| "Failed to serialize message")?;
        writer.write_all(json.as_bytes()).await.with_context(|| {
            format!("Failed to write message to file: {:?}", file_path.display())
        })?;
        writer.flush().await.with_context(|| {
            format!("Failed to flush message to file: {:?}", file_path.display())
        })?;

        Ok(file_path)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::time::Duration;
    use walkdir::WalkDir;

    use mithril_common::messages::RegisterSignatureMessage;
    use mithril_common::test_utils::TempDir;

    use crate::entities::Message;
    use crate::tests::TestLogger;

    use super::*;

    fn list_directory(path: &Path) -> Vec<PathBuf> {
        WalkDir::new(path)
            .min_depth(1)
            .max_depth(1)
            .into_iter()
            .filter_entry(|e| e.file_type().is_file())
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
        let mut sender = MessageSender::new(rx, vec![target_dir.clone()], &TestLogger::stdout());

        tokio::spawn(async move {
            sender.listen().await;
        });

        // No messages should have been written yet
        assert_eq!(Vec::<PathBuf>::new(), list_directory(&target_dir));

        let message = Message::MithrilRegisterSignature(RegisterSignatureMessage::dummy());
        tx.send(message.clone()).await.unwrap();

        // Wait for the message to be forwarded
        tokio::time::sleep(Duration::from_millis(10)).await;

        let expected_file_path = target_dir
            .join(message.file_identifier())
            .with_extension("json");
        assert!(expected_file_path.exists());

        let written_message: Message =
            serde_json::from_reader(std::fs::File::open(expected_file_path).unwrap()).unwrap();
        assert_eq!(message, written_message);
    }

    #[tokio::test]
    async fn write_received_messages_to_multiple_peers() {
        let dir = TempDir::create(
            "signature-network-node-message-sender",
            "write_received_messages_to_multiple_peers",
        );
        let target_dirs = vec![dir.join("peer-1"), dir.join("peer-2"), dir.join("peer-3")];
        for dir in &target_dirs {
            std::fs::create_dir(dir).unwrap();
        }
        let (tx, rx) = mpsc::channel(1);
        let mut sender = MessageSender::new(rx, target_dirs.clone(), &TestLogger::stdout());

        tokio::spawn(async move {
            sender.listen().await;
        });

        // No messages should have been written yet
        assert_eq!(Vec::<PathBuf>::new(), list_directory(&target_dirs[0]));
        assert_eq!(Vec::<PathBuf>::new(), list_directory(&target_dirs[1]));
        assert_eq!(Vec::<PathBuf>::new(), list_directory(&target_dirs[2]));

        let message = Message::MithrilRegisterSignature(RegisterSignatureMessage::dummy());
        let expected_filename = format!("{}.json", message.file_identifier());
        tx.send(message.clone()).await.unwrap();

        // Wait for the message to be forwarded
        tokio::time::sleep(Duration::from_millis(10)).await;

        for target_dir in &target_dirs {
            assert_eq!(
                vec![target_dir.join(expected_filename.clone())],
                list_directory(target_dir)
            );
        }
    }
}
