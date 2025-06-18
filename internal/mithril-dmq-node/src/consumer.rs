use std::{fmt::Debug, marker::PhantomData, path::PathBuf};

use anyhow::{anyhow, Context};
use pallas_network::facades::DmqClient;
use slog::{debug, error, Logger};

use mithril_common::{
    crypto_helper::{OpCert, TryFromBytes},
    entities::PartyId,
    logging::LoggerExtensions,
    CardanoNetwork, StdResult,
};

/// A DMQ consumer implementation.
/// This implementation is built upon the n2c mini-protocols DMQ implementation in Pallas.
pub struct DmqConsumerPallas<M: TryFromBytes + Debug> {
    socket: PathBuf,
    network: CardanoNetwork,
    logger: Logger,
    phantom: PhantomData<M>,
}

impl<M: TryFromBytes + Debug> DmqConsumerPallas<M> {
    /// Creates a new `DmqConsumerPallas` instance.
    pub fn new(socket: PathBuf, network: CardanoNetwork, logger: Logger) -> Self {
        Self {
            socket,
            network,
            logger: logger.new_with_component_name::<Self>(),
            phantom: PhantomData,
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

    /// Consume messages from the DMQ node.
    pub async fn consume_messages(&self) -> StdResult<Vec<(M, PartyId)>> {
        debug!(self.logger, "Waiting for messages from DMQ...");
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
                let opcert = OpCert::try_from_bytes(&dmq_message.operational_certificate)
                    .map_err(|e| anyhow!("Failed to parse operational certificate: {}", e))?;
                let party_id = opcert.compute_protocol_party_id()?;
                let payload = M::try_from_bytes(&dmq_message.msg_body)
                    .map_err(|e| anyhow!("Failed to parse DMQ message body: {}", e))?;

                Ok((payload, party_id))
            })
            .collect::<StdResult<Vec<_>>>()
            .with_context(|| "Failed to parse DMQ messages")
    }
}

#[cfg(test)]
mod tests {

    use std::{fs, future, time::Duration, vec};

    use mithril_common::{crypto_helper::TryToBytes, current_function, test_utils::TempDir};
    use pallas_network::miniprotocols::{localmsgnotification, localmsgsubmission::DmqMsg};
    use tokio::{net::UnixListener, task::JoinHandle, time::sleep};

    use crate::{test::payload::DmqMessageTestPayload, test_tools::TestLogger};

    use super::*;

    fn create_temp_dir(folder_name: &str) -> PathBuf {
        TempDir::create_with_short_path("dmq_consumer", folder_name)
    }

    fn fake_msgs() -> Vec<DmqMsg> {
        vec![
            DmqMsg {
                msg_id: vec![0, 1],
                msg_body: DmqMessageTestPayload::new(b"msg_1").to_bytes_vec().unwrap(),
                block_number: 10,
                ttl: 100,
                kes_signature: vec![0, 1, 2, 3],
                operational_certificate: vec![
                    130, 132, 88, 32, 230, 80, 215, 83, 21, 9, 187, 108, 255, 215, 153, 140, 40,
                    198, 142, 78, 200, 250, 98, 26, 9, 82, 32, 110, 161, 30, 176, 63, 205, 125,
                    203, 41, 0, 0, 88, 64, 212, 171, 206, 39, 218, 5, 255, 3, 193, 52, 44, 198,
                    171, 83, 19, 80, 114, 225, 186, 191, 156, 192, 84, 146, 245, 159, 31, 240, 9,
                    247, 4, 87, 170, 168, 98, 199, 21, 139, 19, 190, 12, 251, 65, 215, 169, 26, 86,
                    37, 137, 188, 17, 14, 178, 205, 175, 93, 39, 86, 4, 138, 187, 234, 95, 5, 88,
                    32, 32, 253, 186, 201, 177, 11, 117, 135, 187, 167, 181, 188, 22, 59, 206, 105,
                    231, 150, 215, 30, 78, 212, 76, 16, 252, 180, 72, 134, 137, 247, 161, 68,
                ],
            },
            DmqMsg {
                msg_id: vec![1, 2],
                msg_body: DmqMessageTestPayload::new(b"msg_2").to_bytes_vec().unwrap(),
                block_number: 11,
                ttl: 100,
                kes_signature: vec![1, 2, 3, 4],
                operational_certificate: vec![
                    130, 132, 88, 32, 230, 80, 215, 83, 21, 9, 187, 108, 255, 215, 153, 140, 40,
                    198, 142, 78, 200, 250, 98, 26, 9, 82, 32, 110, 161, 30, 176, 63, 205, 125,
                    203, 41, 0, 0, 88, 64, 132, 4, 199, 39, 190, 173, 88, 102, 121, 117, 55, 62,
                    39, 189, 113, 96, 175, 24, 171, 240, 74, 42, 139, 202, 128, 185, 44, 130, 209,
                    77, 191, 122, 196, 224, 33, 158, 187, 156, 203, 190, 173, 150, 247, 87, 172,
                    58, 153, 185, 157, 87, 128, 14, 187, 107, 187, 215, 105, 195, 107, 135, 172,
                    43, 173, 9, 88, 32, 77, 75, 24, 6, 47, 133, 2, 89, 141, 224, 69, 202, 123, 105,
                    240, 103, 245, 159, 147, 177, 110, 58, 248, 115, 58, 152, 138, 220, 35, 65,
                    245, 200,
                ],
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
    async fn pallas_dmq_consumer_publisher_succeeds_when_messages_are_available() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let reply_messages = fake_msgs();
        let server = setup_dmq_server(socket_path.clone(), reply_messages);
        let client = tokio::spawn(async move {
            let consumer = DmqConsumerPallas::new(
                socket_path,
                CardanoNetwork::TestNet(0),
                TestLogger::stdout(),
            );

            consumer.consume_messages().await.unwrap()
        });

        let (_, messages) = tokio::join!(server, client);

        assert_eq!(
            vec![
                (
                    DmqMessageTestPayload::new(b"msg_1"),
                    "pool1mxyec46067n3querj9cxkk0g0zlag93pf3ya9vuyr3wgkq2e6t7".to_string()
                ),
                (
                    DmqMessageTestPayload::new(b"msg_2"),
                    "pool17sln0evyk5tfj6zh2qrlk9vttgy6264sfe2fkec5mheasnlx3yd".to_string()
                ),
            ],
            messages.unwrap()
        );
    }

    #[tokio::test]
    async fn pallas_dmq_consumer_publisher_blocks_when_no_message_available() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let reply_messages = vec![];
        let server = setup_dmq_server(socket_path.clone(), reply_messages);
        let client = tokio::spawn(async move {
            let consumer = DmqConsumerPallas::<DmqMessageTestPayload>::new(
                socket_path,
                CardanoNetwork::TestNet(0),
                TestLogger::stdout(),
            );

            consumer.consume_messages().await.unwrap();
        });

        let result = tokio::select!(
            _res = sleep(Duration::from_millis(100)) => {Err(anyhow!("Timeout"))},
            _res =  client  => {Ok(())},
            _res =  server  => {Ok(())},
        );

        result.expect_err("Should have timed out");
    }
}
