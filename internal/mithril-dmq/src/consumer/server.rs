use std::{collections::VecDeque, fs, path::PathBuf};

use anyhow::{Context, anyhow};
use pallas_network::{facades::DmqServer, miniprotocols::localmsgnotification::Request};
use tokio::{
    net::UnixListener,
    select,
    sync::{Mutex, MutexGuard, Notify, mpsc::UnboundedReceiver, watch::Receiver},
};

use slog::{Logger, debug, error, info, warn};

use mithril_common::{CardanoNetwork, StdResult, logging::LoggerExtensions};

use crate::DmqMessage;

/// A queue for storing DMQ messages.
struct BlockingNonBlockingMessageQueue<M> {
    messages: Mutex<VecDeque<M>>,
    new_message_notify: Notify,
}

impl<M> BlockingNonBlockingMessageQueue<M> {
    /// Creates a new instance of [BlockingNonBlockingQueue].
    pub fn new() -> Self {
        Self {
            messages: Mutex::new(VecDeque::new()),
            new_message_notify: Notify::new(),
        }
    }

    /// Enqueues a new message into the queue.
    pub async fn enqueue(&self, message: M) {
        let mut message_queue_guard = self.messages.lock().await;
        (*message_queue_guard).push_back(message);

        self.new_message_notify.notify_waiters();
    }

    /// Returns the messages from the queue in a non blocking way, if available.
    pub async fn dequeue_non_blocking(&self, limit: Option<usize>) -> Vec<M> {
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
    pub async fn dequeue_blocking(&self, limit: Option<usize>) -> Vec<M> {
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

/// Trait for publishing messages from a DMQ node.
/* #[cfg_attr(test, mockall::automock)] */
#[async_trait::async_trait]
pub trait DmqConsumerServer: Send + Sync {
    /// Processes the next message received from the DMQ network.
    async fn process_message(&self) -> StdResult<()>;

    /// Runs the DMQ publisher server.
    async fn run(&self) -> StdResult<()>;
}

/// A DMQ server implementation for messages notification from a DMQ node.
pub struct DmqConsumerServerPallas {
    socket: PathBuf,
    network: CardanoNetwork,
    server: Mutex<Option<DmqServer>>,
    messages_receiver: Mutex<Option<UnboundedReceiver<DmqMessage>>>,
    messages_buffer: BlockingNonBlockingMessageQueue<DmqMessage>,
    stop_rx: Receiver<()>,
    logger: Logger,
}

impl DmqConsumerServerPallas {
    /// Creates a new instance of [DmqConsumerServerPallas].
    pub fn new(
        socket: PathBuf,
        network: CardanoNetwork,
        stop_rx: Receiver<()>,
        logger: Logger,
    ) -> Self {
        Self {
            socket,
            network,
            server: Mutex::new(None),
            messages_receiver: Mutex::new(None),
            messages_buffer: BlockingNonBlockingMessageQueue::new(),
            stop_rx,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Creates and returns a new `DmqServer` connected to the specified socket.
    async fn new_server(&self) -> StdResult<DmqServer> {
        let magic = self.network.code();
        if self.socket.exists() {
            fs::remove_file(self.socket.clone()).unwrap();
        }
        let listener = UnixListener::bind(&self.socket)
            .map_err(|err| anyhow!(err))
            .with_context(|| {
                format!(
                    "DmqConsumerServerPallas failed to bind Unix socket at {}",
                    self.socket.display()
                )
            })?;

        DmqServer::accept(&listener, magic)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "DmqConsumerServerPallas failed to create a new server")
    }

    /// Gets the cached `DmqServer`, creating a new one if it does not exist.
    async fn get_server(&self) -> StdResult<MutexGuard<Option<DmqServer>>> {
        {
            // Run this in a separate block to avoid dead lock on the Mutex
            let server_lock = self.server.lock().await;
            if server_lock.as_ref().is_some() {
                return Ok(server_lock);
            }
        }

        let mut server_lock = self.server.lock().await;
        *server_lock = Some(self.new_server().await?);

        Ok(server_lock)
    }

    /// Drops the current `DmqServer`, if it exists.
    async fn drop_server(&self) -> StdResult<()> {
        debug!(
            self.logger,
            "Drop existing DMQ server";
            "socket" => ?self.socket,
            "network" => ?self.network
        );
        let mut server_lock = self.server.lock().await;
        if let Some(server) = server_lock.take() {
            server.abort().await;
        }

        Ok(())
    }

    #[cfg(test)]
    /// Check if the server already exists (test only).
    async fn has_server(&self) -> bool {
        let server_lock = self.server.lock().await;

        server_lock.as_ref().is_some()
    }

    /// Registers the receiver for DMQ messages (only one receiver is allowed).
    pub async fn register_receiver(
        &self,
        receiver: UnboundedReceiver<DmqMessage>,
    ) -> StdResult<()> {
        debug!(self.logger, "Register message receiver for DMQ messages");
        let mut receiver_guard = self.messages_receiver.lock().await;
        *receiver_guard = Some(receiver);

        Ok(())
    }
}

#[async_trait::async_trait]
impl DmqConsumerServer for DmqConsumerServerPallas {
    async fn process_message(&self) -> StdResult<()> {
        debug!(
            self.logger,
            "Waiting for message received from the DMQ network"
        );
        let mut server_guard = self.get_server().await?;
        let server = server_guard.as_mut().ok_or(anyhow!("DMQ server does not exist"))?;

        let request = server
            .msg_notification()
            .recv_next_request()
            .await
            .map_err(|err| anyhow!("Failed to receive next DMQ message: {}", err))?;

        match request {
            Request::Blocking => {
                debug!(
                    self.logger,
                    "Blocking notification of messages received from the DMQ network"
                );
                let reply_messages = self.messages_buffer.dequeue_blocking(None).await;
                let reply_messages =
                    reply_messages.into_iter().map(|msg| msg.into()).collect::<Vec<_>>();
                server
                    .msg_notification()
                    .send_reply_messages_blocking(reply_messages)
                    .await?;
            }
            Request::NonBlocking => {
                debug!(
                    self.logger,
                    "Non blocking notification of messages received from the DMQ network"
                );
                let reply_messages = self.messages_buffer.dequeue_non_blocking(None).await;
                let reply_messages =
                    reply_messages.into_iter().map(|msg| msg.into()).collect::<Vec<_>>();
                let has_more = !self.messages_buffer.is_empty().await;
                server
                    .msg_notification()
                    .send_reply_messages_non_blocking(reply_messages, has_more)
                    .await?;
                server.msg_notification().recv_done().await?;
            }
        };

        Ok(())
    }

    /// Runs the DMQ publisher server, processing messages in a loop.
    async fn run(&self) -> StdResult<()> {
        info!(
            self.logger,
            "Starting DMQ publisher server";
            "socket" => ?self.socket,
            "network" => ?self.network
        );

        let mut stop_rx = self.stop_rx.clone();
        let mut receiver = self.messages_receiver.lock().await;
        match *receiver {
            Some(ref mut receiver) => loop {
                select! {
                    _ = stop_rx.changed() => {
                        warn!(self.logger, "Stopping signature processor...");

                        return Ok(());
                    }
                    message = receiver.recv() => {
                        if let Some(message) = message {
                            debug!(self.logger, "Received a message from the DMQ network"; "message" => ?message);
                            self.messages_buffer.enqueue(message).await;
                        } else {
                            warn!(self.logger, "DMQ message receiver channel closed");
                            return Ok(());
                        }

                    }
                    res = self.process_message() => {
                        match res {
                            Ok(_) => {
                                debug!(self.logger, "Processed a message successfully");
                            }
                            Err(err) => {
                                error!(self.logger, "Failed to process message"; "error" => ?err);
                                if let Err(drop_err) = self.drop_server().await {
                                    error!(self.logger, "Failed to drop DMQ consumer server"; "error" => ?drop_err);
                                }
                            }
                        }
                    }
                }
            },
            None => {
                return Err(anyhow!("DMQ message receiver is not registered"));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_todo() {
        todo!("Implement tests for DmqConsumerServerPallas");
    }
}
