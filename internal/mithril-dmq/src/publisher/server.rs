use std::{fs, path::PathBuf};

use anyhow::{Context, anyhow};
use pallas_network::{
    facades::DmqServer,
    miniprotocols::{
        localmsgsubmission::DmqMsgValidationError,
        localtxsubmission::{Request, Response},
    },
};
use tokio::{
    net::UnixListener,
    select,
    sync::{Mutex, MutexGuard, mpsc::UnboundedSender, watch::Receiver},
};

use slog::{Logger, debug, error, info, warn};

use mithril_common::{CardanoNetwork, StdResult, logging::LoggerExtensions};

use crate::DmqMessage;

/// Trait for publishing messages from a DMQ node.
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait DmqPublisherServer: Send + Sync {
    /// Processes the next message received from the DMQ client.
    async fn process_message(&self) -> StdResult<()>;

    /// Runs the DMQ publisher server.
    async fn run(&self) -> StdResult<()>;
}

/// A DMQ server implementation for messages publication to a DMQ node.
pub struct DmqPublisherServerPallas {
    socket: PathBuf,
    network: CardanoNetwork,
    server: Mutex<Option<DmqServer>>,
    transmitters: Mutex<Vec<UnboundedSender<DmqMessage>>>,
    stop_rx: Receiver<()>,
    logger: Logger,
}

impl DmqPublisherServerPallas {
    /// Creates a new instance of [DmqPublisherServerPallas].
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
            transmitters: Mutex::new(Vec::new()),
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
                    "DmqPublisherServerPallas failed to bind Unix socket at {}",
                    self.socket.display()
                )
            })?;

        DmqServer::accept(&listener, magic)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "DmqPublisherServerPallas failed to create a new server")
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

    /// Registers a transmitter for DMQ messages.
    pub async fn register_transmitter(
        &self,
        transmitter: UnboundedSender<DmqMessage>,
    ) -> StdResult<()> {
        debug!(self.logger, "Register message transmitter for DMQ messages");
        let mut transmitters_guard = self.transmitters.lock().await;
        transmitters_guard.push(transmitter);

        Ok(())
    }
}

#[async_trait::async_trait]
impl DmqPublisherServer for DmqPublisherServerPallas {
    async fn process_message(&self) -> StdResult<()> {
        debug!(
            self.logger,
            "Waiting for message to publish to the DMQ network";
            "socket" => ?self.socket,
            "network" => ?self.network
        );
        let mut server_guard = self.get_server().await?;
        let server = server_guard.as_mut().ok_or(anyhow!("DMQ server does not exist"))?;

        let request = server
            .msg_submission()
            .recv_next_request()
            .await
            .map_err(|err| anyhow!("Failed to receive next DMQ message: {}", err))?;
        let (dmq_message, response) = match request {
            Request::Submit(dmq_message) => {
                // TODO: validate the message
                debug!(self.logger, "Received message to publish to DMQ");
                (Some(dmq_message), Response::Accepted)
            }
            request => {
                error!(
                    self.logger,
                    "Expected a Submit request, but received: {request:?}"
                );
                (
                    None,
                    Response::Rejected(DmqMsgValidationError(format!(
                        "Expected a Submit request, but received: {request:?}"
                    ))),
                )
            }
        };
        server
            .msg_submission()
            .send_submit_tx_response(response)
            .await
            .map_err(|err| anyhow!("Failed to send response to DMQ publisher client: {}", err))?;

        if let Some(dmq_message) = dmq_message {
            for transmitter in self.transmitters.lock().await.iter() {
                if let Err(err) = transmitter.send(dmq_message.to_owned().into()) {
                    error!(
                        self.logger,
                        "Failed to send DMQ message to transmitter";
                        "error" => ?err
                    );
                }
            }
        }

        Ok(())
    }

    async fn run(&self) -> StdResult<()> {
        info!(
            self.logger,
            "Starting DMQ publisher server";
            "socket" => ?self.socket,
            "network" => ?self.network
        );

        let mut stop_rx = self.stop_rx.clone();
        loop {
            select! {
                _ = stop_rx.changed() => {
                    warn!(self.logger, "Stopping signature processor...");

                    return Ok(());
                }
                res = self.process_message() => {
                    match res {
                        Ok(_) => {
                            debug!(self.logger, "Processed a message successfully");
                        }
                        Err(err) => {
                            error!(self.logger, "Failed to process message"; "error" => ?err);
                            if let Err(drop_err) = self.drop_server().await {
                                error!(self.logger, "Failed to drop DMQ publisher server"; "error" => ?drop_err);
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_todo() {
        todo!("Implement tests for DmqPublisherServerPallas");
    }
}
