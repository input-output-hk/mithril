use anyhow::anyhow;
use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;
use slog::{debug, Logger};
use std::{fmt::Debug, sync::Arc, time::Duration};
use tokio::{
    sync::{mpsc::UnboundedSender, Mutex},
    time::Instant,
};

/// A message repeater will send a copy of the message to a channel at a given frequency
pub struct MessageRepeater<M: Clone + Debug + Sync + Send + 'static> {
    message: Arc<Mutex<Option<M>>>,
    tx_message: UnboundedSender<M>,
    delay: Duration,
    next_repeat_at: Arc<Mutex<Option<Instant>>>,
    logger: Logger,
}

impl<M: Clone + Debug + Sync + Send + 'static> MessageRepeater<M> {
    /// Factory for MessageRepeater
    pub fn new(tx_message: UnboundedSender<M>, delay: Duration, logger: &Logger) -> Self {
        Self {
            message: Arc::new(Mutex::new(None)),
            tx_message,
            delay,
            next_repeat_at: Arc::new(Mutex::new(None)),
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    async fn reset_next_repeat_at(&self) {
        debug!(self.logger, "Reset next_repeat_at");
        *self.next_repeat_at.lock().await = Some(Instant::now() + self.delay);
    }

    /// Set the message to repeat
    pub async fn set_message(&self, message: M) {
        debug!(self.logger, "Set message"; "message" => #?message);
        *self.message.lock().await = Some(message);
        self.reset_next_repeat_at().await;
    }

    /// Start repeating the message if any
    pub async fn repeat_message(&self) -> StdResult<()> {
        let wait_delay = match self.next_repeat_at.lock().await.as_ref() {
            None => self.delay,
            Some(next_repeat_at) => next_repeat_at
                .checked_duration_since(Instant::now())
                .unwrap_or_default(),
        };
        tokio::time::sleep(wait_delay).await;
        match self.message.lock().await.as_ref() {
            Some(message) => {
                debug!(self.logger, "Repeat message"; "message" => #?message);
                self.tx_message.send(message.clone()).map_err(|e| anyhow!(e))?
            }
            None => {
                debug!(self.logger, "No message to repeat");
            }
        }
        self.reset_next_repeat_at().await;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use tokio::{sync::mpsc, time};

    use crate::test_tools::TestLogger;

    use super::*;

    #[tokio::test]
    async fn should_repeat_message_when_exists() {
        let (tx, mut rx) = mpsc::unbounded_channel();
        let delay = Duration::from_millis(100);
        let repeater = MessageRepeater::new(tx, delay, &TestLogger::stdout());

        let message = "Hello, world!";
        repeater.set_message(message.to_string()).await;
        repeater.repeat_message().await.unwrap();

        let received = rx.recv().await.unwrap();
        assert_eq!(message, received);
    }

    #[tokio::test]
    async fn should_repeat_message_when_exists_with_expected_delay() {
        let (tx, _rx) = mpsc::unbounded_channel();
        let delay = Duration::from_secs(1);
        let repeater = MessageRepeater::new(tx, delay, &TestLogger::stdout());

        let message = "Hello, world!";
        repeater.set_message(message.to_string()).await;

        let result = tokio::select! {
            _ = time::sleep(delay - Duration::from_millis(100)) => {Err(anyhow!("Timeout"))}
            _ = repeater.repeat_message() => {Ok(())}
        };

        result.expect_err("should have timed out");
    }

    #[tokio::test]
    async fn should_do_nothing_when_message_not_exists() {
        let (tx, rx) = mpsc::unbounded_channel::<String>();
        let delay = Duration::from_millis(100);
        let repeater = MessageRepeater::new(tx, delay, &TestLogger::stdout());

        repeater.repeat_message().await.unwrap();

        assert!(rx.is_empty());
    }

    #[tokio::test]
    async fn should_do_nothing_when_message_not_exists_with_expected_delay() {
        let (tx, _rx) = mpsc::unbounded_channel::<String>();
        let delay = Duration::from_secs(1);
        let repeater = MessageRepeater::new(tx, delay, &TestLogger::stdout());

        let result = tokio::select! {
            _ = time::sleep(delay - Duration::from_millis(100)) => {Err(anyhow!("Timeout"))}
            _ = repeater.repeat_message() => {Ok(())}
        };

        result.expect_err("should have timed out");
    }
}
