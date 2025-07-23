use std::{fmt::Debug, marker::PhantomData, path::PathBuf};

use anyhow::Context;
use pallas_network::{facades::DmqClient, miniprotocols::localtxsubmission::Response};
use slog::{Logger, debug, error};

use mithril_common::{
    CardanoNetwork, StdResult, crypto_helper::TryToBytes, logging::LoggerExtensions,
};

use crate::{DmqMessageBuilder, DmqPublisher};

/// A DMQ publisher implementation.
///
/// This implementation is built upon the n2c mini-protocols DMQ implementation in Pallas.
pub struct DmqPublisherPallas<M: TryToBytes + Debug> {
    socket: PathBuf,
    network: CardanoNetwork,
    dmq_message_builder: DmqMessageBuilder,
    logger: Logger,
    phantom: PhantomData<M>,
}

impl<M: TryToBytes + Debug> DmqPublisherPallas<M> {
    /// Creates a new instance of [DmqPublisherPallas].
    pub fn new(
        socket: PathBuf,
        network: CardanoNetwork,
        dmq_message_builder: DmqMessageBuilder,
        logger: Logger,
    ) -> Self {
        Self {
            socket,
            network,
            dmq_message_builder,
            logger: logger.new_with_component_name::<Self>(),
            phantom: PhantomData,
        }
    }

    /// Creates and returns a new `DmqClient` connected to the specified socket.
    async fn new_client(&self) -> StdResult<DmqClient> {
        let magic = self.network.magic_id();
        DmqClient::connect(&self.socket, magic)
            .await
            .with_context(|| "DmqPublisherPallas failed to create a new client")
    }
}

#[async_trait::async_trait]
impl<M: TryToBytes + Debug + Sync + Send> DmqPublisher<M> for DmqPublisherPallas<M> {
    async fn publish_message(&self, message: M) -> StdResult<()> {
        debug!(
            self.logger,
            "Publish message to DMQ";
            "message" => ?message
        );
        let mut client = self.new_client().await?;
        let message_bytes = &message.to_bytes_vec()?;
        let dmq_message = self
            .dmq_message_builder
            .build(message_bytes)
            .await
            .with_context(|| "Failed to build DMQ message")?;
        client
            .msg_submission()
            .send_submit_tx(dmq_message)
            .await
            .with_context(|| "Failed to submit DMQ message")?;
        let response = client.msg_submission().recv_submit_tx_response().await?;
        if let Err(e) = client.msg_submission().terminate_gracefully().await {
            error!(self.logger, "Failed to send Done"; "error" => ?e);
        }

        if response != Response::Accepted {
            anyhow::bail!("Failed to publish DMQ message: {:?}", response);
        }

        Ok(())
    }
}

#[cfg(all(test, unix))]
mod tests {
    use std::{fs, sync::Arc};

    use pallas_network::miniprotocols::{
        localmsgsubmission::DmqMsgValidationError, localtxsubmission,
    };
    use tokio::{net::UnixListener, task::JoinHandle};

    use mithril_cardano_node_chain::test::double::FakeChainObserver;
    use mithril_common::{
        crypto_helper::KesSignerFake,
        current_function,
        test::{TempDir, double::Dummy},
    };

    use crate::test::{TestLogger, payload::DmqMessageTestPayload};

    use super::*;

    fn create_temp_dir(folder_name: &str) -> PathBuf {
        TempDir::create_with_short_path("dmq_publisher", folder_name)
    }

    fn setup_dmq_server(socket_path: PathBuf, reply_success: bool) -> JoinHandle<()> {
        tokio::spawn({
            async move {
                // server setup
                if socket_path.exists() {
                    fs::remove_file(socket_path.clone()).unwrap();
                }
                let listener = UnixListener::bind(socket_path).unwrap();
                let mut server = pallas_network::facades::DmqServer::accept(&listener, 0)
                    .await
                    .unwrap();

                // init local msg submission server
                let server_msg = server.msg_submission();

                // server waits for request from client and replies to it
                let request = server_msg.recv_next_request().await.unwrap();
                match &request {
                    localtxsubmission::Request::Submit(_) => (),
                    request => panic!("Expected a Submit request, but received: {request:?}"),
                }
                let response = if reply_success {
                    localtxsubmission::Response::Accepted
                } else {
                    localtxsubmission::Response::Rejected(DmqMsgValidationError(
                        "fake error".to_string(),
                    ))
                };
                server_msg.send_submit_tx_response(response).await.unwrap();

                // server receives done from client
                let request = server_msg.recv_next_request().await.unwrap();
                assert_eq!(localtxsubmission::Request::Done, request);
            }
        })
    }

    #[tokio::test]
    async fn pallas_dmq_signature_publisher_success() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let reply_success = true;
        let server = setup_dmq_server(socket_path.clone(), reply_success);
        let client = tokio::spawn(async move {
            let publisher = DmqPublisherPallas::new(
                socket_path,
                CardanoNetwork::TestNet(0),
                DmqMessageBuilder::new(
                    {
                        let (kes_signature, operational_certificate) =
                            KesSignerFake::dummy_signature();
                        let kes_signer = KesSignerFake::new(vec![Ok((
                            kes_signature,
                            operational_certificate.clone(),
                        ))]);

                        Arc::new(kes_signer)
                    },
                    Arc::new(FakeChainObserver::default()),
                )
                .set_ttl(100),
                TestLogger::stdout(),
            );

            publisher.publish_message(DmqMessageTestPayload::dummy()).await
        });

        let (_, res) = tokio::join!(server, client);

        res.unwrap().unwrap();
    }

    #[tokio::test]
    async fn pallas_dmq_signature_publisher_fails() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let reply_success = false;
        let server = setup_dmq_server(socket_path.clone(), reply_success);
        let client = tokio::spawn(async move {
            let publisher = DmqPublisherPallas::new(
                socket_path,
                CardanoNetwork::TestNet(0),
                DmqMessageBuilder::new(
                    {
                        let (kes_signature, operational_certificate) =
                            KesSignerFake::dummy_signature();
                        let kes_signer = KesSignerFake::new(vec![Ok((
                            kes_signature,
                            operational_certificate.clone(),
                        ))]);

                        Arc::new(kes_signer)
                    },
                    Arc::new(FakeChainObserver::default()),
                )
                .set_ttl(100),
                TestLogger::stdout(),
            );

            publisher.publish_message(DmqMessageTestPayload::dummy()).await
        });

        let (_, res) = tokio::join!(server, client);

        res.unwrap().expect_err("Publishing DMQ message should fail");
    }
}
