use std::path::PathBuf;

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use pallas_network::facades::DmqClient;
use slog::{debug, error, Logger};

use mithril_common::{
    entities::{Epoch, SignedEntityType, SingleSignature},
    logging::LoggerExtensions,
    test_utils::fake_data,
    CardanoNetwork, StdResult,
};

use super::SignatureConsumer;

/// A DMQ implementation of the [SignatureConsumer] trait.
/// This implementation is built upon the n2c mini-protocols DMQ implementation in Pallas.
// TODO: make this generic in a dedicated module/crate?
pub struct PallasDMQSignatureConsumer {
    socket: PathBuf,
    network: CardanoNetwork,
    logger: Logger,
}

impl PallasDMQSignatureConsumer {
    /// Creates a new `PallasDMQSignatureConsumer` instance.
    pub fn new(socket: PathBuf, network: CardanoNetwork, logger: Logger) -> Self {
        Self {
            socket,
            network,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Creates and returns a new `DmqClient` connected to the specified socket.
    async fn new_client(&self) -> StdResult<DmqClient> {
        let magic = self.network.code();
        DmqClient::connect(&self.socket, magic)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainReader failed to create a new client")
    }
}

#[async_trait]
impl SignatureConsumer for PallasDMQSignatureConsumer {
    async fn get_signatures(
        &self,
    ) -> mithril_common::StdResult<Vec<(SingleSignature, SignedEntityType)>> {
        debug!(self.logger, "Waiting for single signatures from DMQ...");
        let mut client = self.new_client().await?; // TODO: add client cache
        client
            .msg_notification()
            .send_request_messages_blocking()
            .await
            .map_err(|err| anyhow!("Failed to request notifications from DMQ server: {}", err))?;
        let reply = client.msg_notification().recv_next_reply().await.unwrap();
        debug!(self.logger, "Received single signatures from DMQ"; "messages" => ?reply);
        if let Err(e) = client.msg_notification().send_done().await {
            error!(self.logger, "Failed to send Done"; "error" => ?e);
        }
        reply
            .0
            .into_iter()
            .map(|dmq_message| {
                let _signature_message = dmq_message.msg_body; // TODO: parse message body and extract signature and signed entity type
                Ok((
                    fake_data::single_signature(vec![1, 2, 3]),
                    SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                ))
            })
            .collect::<StdResult<Vec<_>>>()
            .with_context(|| "Failed to convert DMQ messages into signatures")
    }

    fn get_origin_tag(&self) -> String {
        "DMQ".to_string()
    }
}

#[cfg(test)]
mod tests {

    use std::{fs, future, time::Duration, vec};

    use mithril_common::{
        current_function,
        entities::Epoch,
        test_utils::{fake_data, TempDir},
    };
    use pallas_network::miniprotocols::{localmsgnotification, localmsgsubmission::DmqMsg};
    use tokio::{net::UnixListener, task::JoinHandle, time::sleep};

    use crate::test_tools::TestLogger;

    use super::*;

    fn create_temp_dir(folder_name: &str) -> PathBuf {
        TempDir::create_with_short_path("dmq_consumer", folder_name)
    }

    fn fake_msgs() -> Vec<DmqMsg> {
        vec![
            DmqMsg {
                msg_id: vec![0, 1],
                msg_body: vec![0, 1, 2], // TODO: replace with real signature bytes encoding
                block_number: 10,
                ttl: 100,
                kes_signature: vec![0, 1, 2, 3],
                operational_certificate: vec![0, 1, 2, 3, 4],
            },
            DmqMsg {
                msg_id: vec![1, 2],
                msg_body: vec![1, 2, 3],
                block_number: 11,
                ttl: 100,
                kes_signature: vec![1, 2, 3, 4],
                operational_certificate: vec![1, 2, 3, 4, 5],
            },
        ]
    }

    fn setup_dmq_server(socket_path: PathBuf, reply_messages: Vec<DmqMsg>) -> JoinHandle<()> {
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

                // init local msg notification server
                let server_msg = server.msg_notification();

                // server waits for blocking request from client
                let request = server_msg.recv_next_request().await.unwrap();
                assert_eq!(request, localmsgnotification::Request::Blocking);

                if !reply_messages.is_empty() {
                    // server replies with messages if any
                    server_msg
                        .send_reply_messages_blocking(reply_messages)
                        .await
                        .unwrap();
                    assert_eq!(*server_msg.state(), localmsgnotification::State::Idle);

                    // server receives done from client
                    server_msg.recv_done().await.unwrap();
                    assert_eq!(*server_msg.state(), localmsgnotification::State::Done);
                } else {
                    // server waits if no message available
                    future::pending().await
                }
            }
        })
    }

    #[tokio::test]
    async fn pallas_dmq_consumer_publisher_succeeds_when_messages_are_avvailable() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let reply_messages = fake_msgs();
        let server = setup_dmq_server(socket_path.clone(), reply_messages);

        let client = tokio::spawn(async move {
            let signature_consumer = PallasDMQSignatureConsumer::new(
                socket_path,
                CardanoNetwork::TestNet(0),
                TestLogger::stdout(),
            );

            let signatures = signature_consumer.get_signatures().await.unwrap();
            assert_eq!(
                vec![
                    (
                        fake_data::single_signature(vec![1, 2, 3]),
                        SignedEntityType::MithrilStakeDistribution(Epoch(1))
                    ),
                    (
                        fake_data::single_signature(vec![1, 2, 3]),
                        SignedEntityType::MithrilStakeDistribution(Epoch(1))
                    )
                ],
                signatures
            );
        });

        let (_, _) = tokio::join!(server, client);
    }

    #[tokio::test]
    async fn pallas_dmq_consumer_publisher_blocks_when_no_message_available() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let reply_messages = vec![];
        let server = setup_dmq_server(socket_path.clone(), reply_messages);

        let client = tokio::spawn(async move {
            let signature_consumer = PallasDMQSignatureConsumer::new(
                socket_path,
                CardanoNetwork::TestNet(0),
                TestLogger::stdout(),
            );

            signature_consumer.get_signatures().await.unwrap();
        });

        let result = tokio::select!(
            _res = sleep(Duration::from_millis(100)) => {Err(anyhow!("Timeout"))},
            _res =  client  => {Ok(())},
            _res =  server  => {Ok(())},
        );

        result.expect_err("Should have timed out");
    }
}
