use std::path::PathBuf;

use anyhow::{anyhow, Context};
use pallas_network::{facades::DmqClient, miniprotocols::localtxsubmission::Response};
use slog::{debug, error, Logger};

use mithril_common::{
    crypto_helper::TryToBytes,
    entities::{ProtocolMessage, SignedEntityType, SingleSignature},
    logging::LoggerExtensions,
    CardanoNetwork, StdResult,
};

use crate::services::signature_publisher::dmq::dmq_message_builder::DmqMessageBuilder;

use super::SignaturePublisher;

mod dmq_message_builder {
    use std::sync::Arc;

    use anyhow::{anyhow, Context};
    use blake2::{digest::consts::U64, Blake2b, Digest};
    use mithril_common::{chain_observer::ChainObserver, StdResult};
    use pallas_network::miniprotocols::localmsgsubmission::DmqMsg;

    /// The TTL (Time To Live) for DMQ messages in blocks.
    const DMQ_MESSAGE_TTL_IN_BLOCKS: u16 = 100;

    /// A builder for creating DMQ messages.
    pub struct DmqMessageBuilder {
        chain_observer: Arc<dyn ChainObserver>,
        ttl_blocks: u16,
    }

    impl DmqMessageBuilder {
        /// Creates a new instance of `DmqMessageBuilder`.
        pub fn new(chain_observer: Arc<dyn ChainObserver>, ttl_blocks: u16) -> Self {
            Self {
                chain_observer,
                ttl_blocks,
            }
        }

        pub async fn build(&self, message_bytes: &[u8]) -> StdResult<DmqMsg> {
            fn compute_msg_id(dmq_message: &DmqMsg) -> Vec<u8> {
                let mut hasher = Blake2b::<U64>::new();
                hasher.update(&dmq_message.msg_body);
                hasher.update(&dmq_message.block_number.to_be_bytes());
                hasher.update(&dmq_message.ttl.to_be_bytes());
                hasher.update(&dmq_message.kes_signature);
                hasher.update(&dmq_message.operational_certificate);

                (&hasher.finalize()).to_vec()
            }

            let block_number = self
                .chain_observer
                .get_current_chain_point()
                .await
                .with_context(|| "Failed to get current chain point while building DMQ message")?
                .ok_or(anyhow!(
                    "No current chain point available while building DMQ message"
                ))?
                .block_number;
            let block_number = (*block_number)
                .try_into()
                .map_err(|_| anyhow!("Failed to convert block number to u32"))?;
            let kes_signature = vec![]; // TO DO: create a KES signature
            let operational_certificate = vec![]; // TO DO: create an operational certificate
            let mut dmq_message = DmqMsg {
                msg_id: vec![],
                msg_body: message_bytes.to_vec(),
                block_number,
                ttl: self.ttl_blocks,
                kes_signature,
                operational_certificate,
            };
            dmq_message.msg_id = compute_msg_id(&dmq_message);

            Ok(dmq_message)
        }
    }

    #[cfg(test)]
    mod tests {
        use mithril_common::{
            chain_observer::FakeObserver,
            crypto_helper::TryToBytes,
            entities::{BlockNumber, ChainPoint, TimePoint},
            StdResult,
        };

        use super::*;

        mod test_utils {
            use super::*;

            pub(super) struct TestMessage {
                pub(super) content: Vec<u8>,
            }

            impl TryToBytes for TestMessage {
                fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
                    Ok(self.content.clone())
                }
            }
        }

        #[tokio::test]
        async fn test_build_dmq_message() {
            let chain_observer = Arc::new(FakeObserver::new(Some(TimePoint {
                chain_point: ChainPoint {
                    block_number: BlockNumber(123),
                    ..ChainPoint::dummy()
                },
                ..TimePoint::dummy()
            })));
            let builder = DmqMessageBuilder::new(chain_observer, 100);
            let message = test_utils::TestMessage {
                content: b"test".to_vec(),
            };

            let dmq_message = builder
                .build(&message.to_bytes_vec().unwrap())
                .await
                .unwrap();

            assert_eq!(
                DmqMsg {
                    msg_id: vec![
                        26, 113, 171, 177, 174, 241, 244, 241, 209, 92, 210, 7, 119, 105, 94, 133,
                        93, 62, 82, 95, 91, 221, 146, 174, 201, 190, 140, 1, 217, 240, 228, 203,
                        14, 50, 104, 59, 252, 216, 26, 84, 231, 142, 163, 140, 11, 95, 17, 234,
                        242, 39, 230, 160, 194, 219, 128, 42, 53, 125, 218, 48, 209, 3, 210, 154
                    ],
                    msg_body: vec![116, 101, 115, 116],
                    block_number: 123,
                    ttl: 100,
                    kes_signature: vec![],
                    operational_certificate: vec![],
                },
                dmq_message
            );
        }
    }
}

/// A DMQ implementation of the [SignaturePublisher] trait.
/// This implementation is built upon the n2c mini-protocols DMQ implementation in Pallas.
// TODO: make this generic in a dedicated module/crate?
pub struct PallasDMQSignaturePublisher {
    socket: PathBuf,
    network: CardanoNetwork,
    dmq_message_builder: DmqMessageBuilder,
    logger: Logger,
}

impl PallasDMQSignaturePublisher {
    /// Creates a new instance of [PallasDMQSignaturePublisher].
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

#[async_trait::async_trait]
impl SignaturePublisher for PallasDMQSignaturePublisher {
    async fn publish(
        &self,
        _signed_entity_type: &SignedEntityType,
        signature: &SingleSignature,
        _protocol_message: &ProtocolMessage,
    ) -> StdResult<()> {
        debug!(
            self.logger,
            "Send single signature to DMQ";
            "signature" => ?signature
        );
        let mut client = self.new_client().await?;
        let message_bytes = &signature.signature.to_bytes_vec()?; // TODO: we need more information than just the signature here
        let dmq_message = self
            .dmq_message_builder
            .build(message_bytes)
            .await
            .with_context(|| "Failed to build DMQ message")?;
        client
            .msg_submission()
            .send_submit_tx(dmq_message)
            .await
            .map_err(|err| anyhow!("Failed to submit DMQ message: {}", err))?;
        let response = client.msg_submission().recv_submit_tx_response().await?;
        if let Err(e) = client.msg_submission().terminate_gracefully().await {
            error!(self.logger, "Failed to send Done"; "error" => ?e);
        }

        if response != Response::Accepted {
            return Err(anyhow!("Failed to publish DMQ message: {:?}", response));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use std::{fs, sync::Arc, vec};

    use mithril_common::{
        chain_observer::FakeObserver,
        current_function,
        entities::Epoch,
        test_utils::{fake_data, TempDir},
    };
    use pallas_network::miniprotocols::{
        localmsgsubmission::DmqMsgValidationError, localtxsubmission,
    };
    use tokio::{net::UnixListener, task::JoinHandle};

    use crate::test_tools::TestLogger;

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
                if let localtxsubmission::Request::Submit(_) = &request {
                } else {
                    panic!("Expected a submit request, but got: {:?}", request);
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
            let signature_publisher = PallasDMQSignaturePublisher::new(
                socket_path,
                CardanoNetwork::TestNet(0),
                DmqMessageBuilder::new(Arc::new(FakeObserver::default()), 100),
                TestLogger::stdout(),
            );

            signature_publisher
                .publish(
                    &SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                    &fake_data::single_signature(vec![1, 2, 3]),
                    &ProtocolMessage::default(),
                )
                .await
                .unwrap();
        });

        let (_, _) = tokio::join!(server, client);
    }

    #[tokio::test]
    async fn pallas_dmq_signature_publisher_fails() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let reply_success = false;
        let server = setup_dmq_server(socket_path.clone(), reply_success);

        let client = tokio::spawn(async move {
            let signature_publisher = PallasDMQSignaturePublisher::new(
                socket_path,
                CardanoNetwork::TestNet(0),
                DmqMessageBuilder::new(Arc::new(FakeObserver::default()), 100),
                TestLogger::stdout(),
            );

            signature_publisher
                .publish(
                    &SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                    &fake_data::single_signature(vec![1, 2, 3]),
                    &ProtocolMessage::default(),
                )
                .await
                .expect_err("Publishing DMQ message should fail");
        });

        let (_, _) = tokio::join!(server, client);
    }
}
