use std::{collections::VecDeque, sync::Arc};

use mithril_common::StdResult;
use tokio::sync::{Mutex, Notify};

use crate::{
    DmqMessage,
    model::{SystemUnixTimestampProvider, UnixTimestampProvider},
};

/// A queue for storing DMQ messages.
pub(crate) struct MessageQueue {
    messages: Mutex<VecDeque<DmqMessage>>,
    new_message_notify: Notify,
    timestamp_provider: Arc<dyn UnixTimestampProvider>,
    max_size: usize,
}

impl MessageQueue {
    /// The default maximum size of the message queue.
    const MAX_SIZE_DEFAULT: usize = 10000;

    /// Creates a new instance of [BlockingNonBlockingQueue].
    pub fn new(max_size: usize, timestamp_provider: Arc<dyn UnixTimestampProvider>) -> Self {
        Self {
            messages: Mutex::new(VecDeque::new()),
            new_message_notify: Notify::new(),
            timestamp_provider,
            max_size,
        }
    }

    /// Cleans the queue
    ///
    /// Removes expired messages and ensures the queue does not exceed the maximum size.
    async fn clean_queue(&self) {
        let mut message_queue_guard = self.messages.lock().await;
        // Remove expired messages from the front of the queue
        // There may be other expired messages in the queue, but they will be removed on dequeue
        // This avoids full scan of the queue.
        while let Some(message) = message_queue_guard.front()
            && self.has_message_expired(message).unwrap_or(false)
        {
            message_queue_guard.pop_front();
        }

        while message_queue_guard.len() > self.max_size {
            message_queue_guard.pop_front();
        }
    }

    /// Checks if a message has expired.
    fn has_message_expired(&self, message: &DmqMessage) -> StdResult<bool> {
        let current_timestamp: u32 = self.timestamp_provider.current_timestamp()?.try_into()?;
        Ok(message.msg_payload.expires_at < current_timestamp)
    }

    /// Enqueues a new message into the queue.
    pub async fn enqueue(&self, message: DmqMessage) {
        {
            // Run in a block to avoid Mutex deadlock in clean_queue
            let mut message_queue_guard = self.messages.lock().await;
            (*message_queue_guard).push_back(message);
        }
        self.clean_queue().await;

        self.new_message_notify.notify_waiters();
    }

    /// Returns the messages from the queue in a non blocking way, if available.
    pub async fn dequeue_non_blocking(&self, limit: Option<usize>) -> Vec<DmqMessage> {
        self.clean_queue().await;
        let mut message_queue_guard = self.messages.lock().await;
        let limit = limit.unwrap_or((*message_queue_guard).len());
        let mut messages = Vec::new();
        for _ in 0..limit {
            if let Some(message) = (*message_queue_guard).pop_front()
                && !self.has_message_expired(&message).unwrap_or(false)
            {
                messages.push(message);
            }
        }

        messages
    }

    /// Returns the messages from the queue in a blocking way, waiting for new messages if necessary.
    pub async fn dequeue_blocking(&self, limit: Option<usize>) -> Vec<DmqMessage> {
        loop {
            let messages = self.dequeue_non_blocking(limit).await;
            if !messages.is_empty() {
                return messages;
            }

            self.new_message_notify.notified().await;
        }
    }

    /// Checks if the message queue is empty.
    pub async fn is_empty(&self) -> bool {
        self.len().await == 0
    }

    /// Get the length of the message queue.
    pub async fn len(&self) -> usize {
        let message_queue_guard = self.messages.lock().await;
        (*message_queue_guard).len()
    }
}

impl Default for MessageQueue {
    fn default() -> Self {
        Self::new(
            Self::MAX_SIZE_DEFAULT,
            Arc::new(SystemUnixTimestampProvider),
        )
    }
}

#[cfg(test)]
mod tests {
    use std::{ops::RangeInclusive, time::Duration};

    use anyhow::anyhow;
    use pallas_network::miniprotocols::localmsgsubmission::{
        DmqMsg, DmqMsgOperationalCertificate, DmqMsgPayload,
    };
    use tokio::time::sleep;

    use crate::model::MockUnixTimestampProvider;

    use super::*;

    fn fake_msg() -> DmqMsg {
        DmqMsg {
            msg_payload: DmqMsgPayload {
                msg_id: vec![0, 1],
                msg_body: vec![0, 1, 2],
                kes_period: 10,
                expires_at: 100,
            },
            kes_signature: vec![0, 1, 2, 3],
            operational_certificate: DmqMsgOperationalCertificate {
                kes_vk: vec![12, 13, 14],
                issue_number: 15,
                start_kes_period: 16,
                cert_sig: vec![17],
            },
            cold_verification_key: vec![0, 1, 2, 3, 4, 5],
        }
    }

    fn fake_messages(range: RangeInclusive<u8>, expires_at: u32) -> Vec<DmqMessage> {
        range
            .map(|i| {
                let mut message = fake_msg();
                message.msg_payload.msg_id = vec![i];
                message.msg_payload.expires_at = expires_at;
                message.into()
            })
            .collect::<Vec<_>>()
    }

    fn create_queue(max_size: usize, current_timestamp: u64) -> MessageQueue {
        MessageQueue::new(
            max_size,
            Arc::new({
                let mut mock_timestamp_provider = MockUnixTimestampProvider::new();
                mock_timestamp_provider
                    .expect_current_timestamp()
                    .returning(move || Ok(current_timestamp));

                mock_timestamp_provider
            }),
        )
    }

    #[tokio::test]
    async fn enqueue_and_dequeue_non_blocking_no_limit() {
        let max_size = 100;
        let current_timestamp = 10;
        let expires_at = 100;
        let queue = create_queue(max_size, current_timestamp);
        let messages = fake_messages(1..=5, expires_at);
        for message in messages.clone() {
            queue.enqueue(message).await;
        }
        let limit = None;

        let dequeued_messages = queue.dequeue_non_blocking(limit).await;

        assert_eq!(messages, dequeued_messages);
    }

    #[tokio::test]
    async fn enqueue_and_dequeue_non_blocking_with_limit() {
        let max_size = 100;
        let current_timestamp = 10;
        let expires_at = 100;
        let queue = create_queue(max_size, current_timestamp);
        let messages = fake_messages(1..=5, expires_at);
        for message in messages.clone() {
            queue.enqueue(message).await;
        }
        let limit = Some(2);

        let dequeued_messages = queue.dequeue_non_blocking(limit).await;

        assert_eq!(messages[0..=1].to_vec(), dequeued_messages);
    }

    #[tokio::test]
    async fn enqueue_and_dequeue_blocking_no_limit() {
        let max_size = 100;
        let current_timestamp = 10;
        let expires_at = 100;
        let queue = create_queue(max_size, current_timestamp);
        let messages = fake_messages(1..=5, expires_at);
        for message in messages.clone() {
            queue.enqueue(message).await;
        }
        let limit = None;

        let dequeued_messages = queue.dequeue_blocking(limit).await;

        assert_eq!(messages, dequeued_messages);
    }

    #[tokio::test]
    async fn enqueue_and_dequeue_blocking_with_limit() {
        let max_size = 100;
        let current_timestamp = 10;
        let expires_at = 100;
        let queue = create_queue(max_size, current_timestamp);
        let messages = fake_messages(1..=5, expires_at);
        for message in messages.clone() {
            queue.enqueue(message).await;
        }
        let limit = Some(2);

        let dequeued_messages = queue.dequeue_blocking(limit).await;

        assert_eq!(messages[0..=1].to_vec(), dequeued_messages);
    }

    #[tokio::test]
    async fn dequeue_blocking_blocks_when_no_message_available() {
        let max_size = 100;
        let current_timestamp = 10;
        let queue = create_queue(max_size, current_timestamp);

        let result = tokio::select!(
            _res = sleep(Duration::from_millis(100)) => {Err(anyhow!("Timeout"))},
            _res = queue.dequeue_blocking(None)  => {Ok(())},
        );

        result.expect_err("Should have timed out");
    }

    #[tokio::test]
    async fn queue_drains_oldest_messages_when_full() {
        let max_size = 3;
        let current_timestamp = 10;
        let expires_at = 100;
        let queue = create_queue(max_size, current_timestamp);
        let messages = fake_messages(1..=5, expires_at);
        for message in messages.clone() {
            queue.enqueue(message).await;
        }
        let limit = None;

        let dequeued_messages = queue.dequeue_blocking(limit).await;

        assert_eq!(messages[2..=4].to_vec(), dequeued_messages);
    }

    #[tokio::test]
    async fn queue_drains_expired_message() {
        let max_size = 3;
        let total_expired_messages = 4;
        let total_non_expired_messages = 6;
        let current_timestamp = 10;
        let expires_at_expired = 1;
        let expires_at_non_expired = 100;
        let queue = create_queue(max_size, current_timestamp);
        let expired_messages = fake_messages(1..=total_expired_messages, expires_at_expired);
        let non_expired_messages = fake_messages(
            total_expired_messages + 1..=total_non_expired_messages + total_expired_messages,
            expires_at_non_expired,
        );
        for message in expired_messages.clone() {
            queue.enqueue(message).await;
        }
        for message in non_expired_messages.clone() {
            queue.enqueue(message).await;
        }
        let limit = None;

        let dequeued_messages = queue.dequeue_blocking(limit).await;

        let expected_non_expired_messages_range =
            total_non_expired_messages as usize - max_size..total_non_expired_messages as usize;
        assert_eq!(
            non_expired_messages[expected_non_expired_messages_range].to_vec(),
            dequeued_messages
        );
    }
}
