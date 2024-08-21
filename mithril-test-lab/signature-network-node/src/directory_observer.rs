use anyhow::Context;
use mithril_common::messages::RegisterSignatureMessage;
use mithril_common::StdResult;
use notify::event::{AccessKind, AccessMode};
use notify::{EventKind, Watcher};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;
use tokio::sync::mpsc;

/// Observes a directory for new messages and sends them to a channel
pub struct DirectoryObserver {
    watcher: notify::RecommendedWatcher,
}

impl DirectoryObserver {
    pub fn watch(message_folder: PathBuf, sender: mpsc::Sender<Message>) -> StdResult<Self> {
        let mut watcher =
            notify::recommended_watcher(move |res: notify::Result<notify::Event>| match res {
                Ok(event)
                    if matches!(
                        event.kind,
                        // Note: ideally we would only watch for file creation events, but
                        // at creation files are not fully written yet, so we need to wait for
                        // the file to be closed.
                        // This is a workaround to avoid polling the file for changes, but that
                        // means that we will be notified for every file modification not only
                        // for the creation.
                        // Additional note: writing directly to a file using std::fs::File won't
                        // trigger the event, but using a `BufWriter` around the file will.
                        EventKind::Access(AccessKind::Close(AccessMode::Write))
                    ) =>
                {
                    println!("File modification finished: {:?}", event.paths);

                    for file in event.paths {
                        let file_content = fs::read_to_string(&file).unwrap();
                        match serde_json::from_str::<RegisterSignatureMessage>(&file_content) {
                            Ok(register_signature_message) => {
                                sender
                                    .try_send(Message::MithrilRegisterSignature(
                                        register_signature_message,
                                    ))
                                    .unwrap();

                                fs::remove_file(&file).unwrap();
                            }
                            Err(err) => eprintln!("Error parsing file content: {err:?}"),
                        }
                    }
                }
                Ok(event) => println!("Unsupported event: {:?}", event),
                Err(err) => eprintln!("Error watching directory: {err:?}"),
            })
            .with_context(|| {
                format!(
                    "Failed to create directory watcher. watched dir: '{}'",
                    message_folder.display()
                )
            })?;

        watcher.watch(&message_folder, notify::RecursiveMode::NonRecursive)?;

        Ok(Self { watcher })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Message {
    MithrilRegisterSignature(RegisterSignatureMessage),
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::TempDir;
    use std::fs::File;
    use std::io::BufWriter;
    use std::time::Duration;
    use tokio::sync::mpsc::error::TryRecvError;

    use super::*;

    #[tokio::test]
    async fn detect_new_file_in_folder() {
        let dir = TempDir::create(
            "signature-network-node",
            "message_notifier_detect_new_file_in_folder",
        );

        let (tx, mut rx) = mpsc::channel(1);
        // As long as the notifier is in scope, messages notifications can be sent to the channel
        let _notifier = DirectoryObserver::watch(dir.clone(), tx).unwrap();

        // No messages should have been notified yet
        assert_eq!(Err(TryRecvError::Empty), rx.try_recv());

        // Create a serialized RegisterSignatureMessage in the folder
        let file = File::create_new(dir.join("register_signature.json")).unwrap();
        serde_json::to_writer_pretty(BufWriter::new(file), &RegisterSignatureMessage::dummy())
            .unwrap();

        // Wait for the message to be notified
        tokio::time::sleep(Duration::from_millis(10)).await;
        assert_eq!(
            Ok(Message::MithrilRegisterSignature(
                RegisterSignatureMessage::dummy()
            )),
            rx.try_recv(),
        );
    }

    #[tokio::test]
    async fn after_successfull_read_message_file_is_deleted() {
        let dir = TempDir::create(
            "signature-network-node",
            "after_successfull_read_message_file_is_deleted",
        );

        let (tx, _rx) = mpsc::channel(1);
        // As long as the notifier is in scope, messages notifications can be sent to the channel
        let _notifier = DirectoryObserver::watch(dir.clone(), tx).unwrap();

        // Create a serialized RegisterSignatureMessage in the folder
        let file_path = dir.join("register_signature.json");
        let file = File::create_new(&file_path).unwrap();
        serde_json::to_writer_pretty(BufWriter::new(file), &RegisterSignatureMessage::dummy())
            .unwrap();

        // Wait for the message to be notified
        tokio::time::sleep(Duration::from_millis(10000)).await;

        assert!(!file_path.exists());
    }
}
