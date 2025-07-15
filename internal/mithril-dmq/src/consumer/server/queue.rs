use std::collections::VecDeque;

use tokio::sync::{Mutex, Notify};

use crate::DmqMessage;

/// A queue for storing DMQ messages.
pub(crate) struct MessageQueue {
    messages: Mutex<VecDeque<DmqMessage>>,
    new_message_notify: Notify,
}

impl MessageQueue {
    /// Creates a new instance of [BlockingNonBlockingQueue].
    pub fn new() -> Self {
        Self {
            messages: Mutex::new(VecDeque::new()),
            new_message_notify: Notify::new(),
        }
    }

    /// Enqueues a new message into the queue.
    pub async fn enqueue(&self, message: DmqMessage) {
        let mut message_queue_guard = self.messages.lock().await;
        (*message_queue_guard).push_back(message);

        self.new_message_notify.notify_waiters();
    }

    /// Returns the messages from the queue in a non blocking way, if available.
    pub async fn dequeue_non_blocking(&self, limit: Option<usize>) -> Vec<DmqMessage> {
        let mut message_queue_guard = self.messages.lock().await;
        let limit = limit.unwrap_or((*message_queue_guard).len());
        let mut messages = Vec::new();
        for _ in 0..limit {
            if let Some(message) = (*message_queue_guard).pop_front() {
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

#[cfg(test)]
mod tests {
    use std::{ops::RangeInclusive, time::Duration};

    use anyhow::anyhow;
    use pallas_network::miniprotocols::localmsgsubmission::DmqMsg;
    use tokio::time::sleep;

    use super::*;

    fn fake_msg() -> DmqMsg {
        DmqMsg {
            msg_id: vec![0, 1],
            msg_body: vec![0, 1, 2],
            block_number: 10,
            ttl: 100,
            kes_signature: vec![0, 1, 2, 3],
            operational_certificate: vec![0, 1, 2, 3, 4],
            kes_period: 10,
        }
    }

    fn fake_messages(range: RangeInclusive<u8>) -> Vec<DmqMessage> {
        range
            .map(|i| {
                DmqMsg {
                    msg_id: vec![i],
                    ..fake_msg()
                }
                .into()
            })
            .collect::<Vec<_>>()
    }

    #[tokio::test]
    async fn enqueue_and_dequeue_non_blocking_no_limit() {
        let queue = MessageQueue::new();
        let messages = fake_messages(1..=5);
        for message in messages.clone() {
            queue.enqueue(message).await;
        }
        let limit = None;

        let dequeued_messages = queue.dequeue_non_blocking(limit).await;

        assert_eq!(messages, dequeued_messages);
    }

    #[tokio::test]
    async fn enqueue_and_dequeue_non_blocking_with_limit() {
        let queue = MessageQueue::new();
        let messages = fake_messages(1..=5);
        for message in messages.clone() {
            queue.enqueue(message).await;
        }
        let limit = Some(2);

        let dequeued_messages = queue.dequeue_non_blocking(limit).await;

        assert_eq!(messages[0..=1].to_vec(), dequeued_messages);
    }

    #[tokio::test]
    async fn enqueue_and_dequeue_blocking_no_limit() {
        let queue = MessageQueue::new();
        let messages = fake_messages(1..=5);
        for message in messages.clone() {
            queue.enqueue(message).await;
        }
        let limit = None;

        let dequeued_messages = queue.dequeue_blocking(limit).await;

        assert_eq!(messages, dequeued_messages);
    }

    #[tokio::test]
    async fn enqueue_and_dequeue_blocking_with_limit() {
        let queue = MessageQueue::new();
        let messages = fake_messages(1..=5);
        for message in messages.clone() {
            queue.enqueue(message).await;
        }
        let limit = Some(2);

        let dequeued_messages = queue.dequeue_blocking(limit).await;

        assert_eq!(messages[0..=1].to_vec(), dequeued_messages);
    }

    #[tokio::test]
    async fn dequeue_blocking_blocks_when_no_message_available() {
        let queue = MessageQueue::new();

        let result = tokio::select!(
            _res = sleep(Duration::from_millis(100)) => {Err(anyhow!("Timeout"))},
            _res = queue.dequeue_blocking(None)  => {Ok(())},
        );

        result.expect_err("Should have timed out");
    }
}
