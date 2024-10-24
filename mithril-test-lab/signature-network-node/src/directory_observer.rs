use std::fs;
use std::path::Path;

use anyhow::Context;
use notify::event::{AccessKind, AccessMode};
use notify::{EventKind, Watcher};
use slog::{error, info, trace, Logger};
use tokio::sync::mpsc;

use mithril_common::StdResult;

use crate::entities::Message;

/// Observes a directory for new messages and sends them to a channel
pub struct DirectoryObserver {
    /// The directory watcher, it will stop when dropped
    _watcher: notify::RecommendedWatcher,
}

impl DirectoryObserver {
    pub fn watch(
        message_folder: &Path,
        sender: mpsc::Sender<Message>,
        parent_logger: &Logger,
    ) -> StdResult<Self> {
        let logger = parent_logger.new(slog::o!("src" => "directory_observer"));
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
                    info!(
                        logger,
                        "Detected new message(s) in input folder: {:?}", event.paths
                    );

                    for file in event.paths {
                        if let Err(err) = read_message_then_send_to_channel(&file, &sender) {
                            error!(logger, "Failed to read message file"; "err" => ?err);
                        }
                    }
                }
                Ok(event) => trace!(logger, "Unsupported event: {:?}", event),
                Err(err) => error!(logger, "Error watching directory: {err:?}"),
            })
            .with_context(|| {
                format!(
                    "Failed to create directory watcher. watched dir: '{}'",
                    message_folder.display()
                )
            })?;

        watcher.watch(message_folder, notify::RecursiveMode::NonRecursive)?;

        Ok(Self { _watcher: watcher })
    }
}

fn read_message_then_send_to_channel(
    file_path: &Path,
    sender: &mpsc::Sender<Message>,
) -> StdResult<()> {
    let file_content = fs::read_to_string(file_path)
        .with_context(|| format!("Failed to read message file, path: {}", file_path.display()))?;
    let message = serde_json::from_str::<Message>(&file_content)
        .with_context(|| "Failed to parse message file")?;
    sender
        .try_send(message)
        .with_context(|| "Failed to send message to channel")?;
    fs::remove_file(file_path).with_context(|| {
        format!(
            "Failed to remove message file, path: {}",
            file_path.display()
        )
    })?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::BufWriter;
    use std::time::Duration;

    use tokio::sync::mpsc::error::TryRecvError;

    use mithril_common::messages::RegisterSignatureMessage;
    use mithril_common::test_utils::TempDir;

    use crate::tests::TestLogger;

    use super::*;

    #[tokio::test]
    async fn detect_new_file_in_folder() {
        let dir = TempDir::create("signature-network-node", "detect_new_file_in_folder");

        let (tx, mut rx) = mpsc::channel(1);
        // As long as the notifier is in scope, messages notifications can be sent to the channel
        let _notifier = DirectoryObserver::watch(&dir, tx, &TestLogger::stdout()).unwrap();

        // No messages should have been notified yet
        assert_eq!(Err(TryRecvError::Empty), rx.try_recv());

        // Create a serialized RegisterSignatureMessage in the folder
        let file = File::create_new(dir.join("register_signature.json")).unwrap();
        serde_json::to_writer_pretty(
            BufWriter::new(file),
            &Message::MithrilRegisterSignature(RegisterSignatureMessage::dummy()),
        )
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
    async fn after_successful_read_message_file_is_deleted() {
        let dir = TempDir::create(
            "signature-network-node",
            "after_successful_read_message_file_is_deleted",
        );

        let (tx, _rx) = mpsc::channel(1);
        // As long as the notifier is in scope, messages notifications can be sent to the channel
        let _notifier = DirectoryObserver::watch(&dir, tx, &TestLogger::stdout()).unwrap();

        // Create a serialized RegisterSignatureMessage in the folder
        let file_path = dir.join("register_signature.json");
        let file = File::create_new(&file_path).unwrap();
        serde_json::to_writer_pretty(
            BufWriter::new(file),
            &Message::MithrilRegisterSignature(RegisterSignatureMessage::dummy()),
        )
        .unwrap();

        // Wait for the message to be notified
        tokio::time::sleep(Duration::from_millis(10)).await;

        assert!(!file_path.exists());
    }
}
