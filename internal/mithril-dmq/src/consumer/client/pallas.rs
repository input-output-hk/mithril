use std::{fmt::Debug, marker::PhantomData, path::PathBuf};

use anyhow::{Context, anyhow};
use pallas_network::{facades::DmqClient, miniprotocols::localmsgnotification::State};
use slog::{Logger, debug, error};
use tokio::sync::{Mutex, MutexGuard};

use mithril_common::{
    CardanoNetwork, StdResult,
    crypto_helper::{OpCert, TryFromBytes},
    entities::PartyId,
    logging::LoggerExtensions,
};

use crate::DmqConsumerClient;

/// A DMQ client consumer implementation.
///
/// This implementation is built upon the n2c mini-protocols DMQ implementation in Pallas.
pub struct DmqConsumerClientPallas<M: TryFromBytes + Debug> {
    socket: PathBuf,
    network: CardanoNetwork,
    client: Mutex<Option<DmqClient>>,
    logger: Logger,
    phantom: PhantomData<M>,
}

impl<M: TryFromBytes + Debug> DmqConsumerClientPallas<M> {
    /// Creates a new `DmqConsumerClientPallas` instance.
    pub fn new(socket: PathBuf, network: CardanoNetwork, logger: Logger) -> Self {
        Self {
            socket,
            network,
            client: Mutex::new(None),
            logger: logger.new_with_component_name::<Self>(),
            phantom: PhantomData,
        }
    }

    /// Creates and returns a new `DmqClient` connected to the specified socket.
    async fn new_client(&self) -> StdResult<DmqClient> {
        debug!(
            self.logger,
            "Create new DMQ client";
            "socket" => ?self.socket,
            "network" => ?self.network
        );
        DmqClient::connect(&self.socket, self.network.magic_id())
            .await
            .with_context(|| "DmqConsumerClientPallas failed to create a new client")
    }

    /// Gets the cached `DmqClient`, creating a new one if it does not exist.
    async fn get_client(&self) -> StdResult<MutexGuard<'_, Option<DmqClient>>> {
        {
            // Run this in a separate block to avoid dead lock on the Mutex
            let client_lock = self.client.lock().await;
            if client_lock.as_ref().is_some() {
                return Ok(client_lock);
            }
        }

        let mut client_lock = self.client.lock().await;
        *client_lock = Some(self.new_client().await?);

        Ok(client_lock)
    }

    /// Drops the current `DmqClient`, if it exists.
    async fn drop_client(&self) -> StdResult<()> {
        debug!(
            self.logger,
            "Drop existing DMQ client";
            "socket" => ?self.socket,
            "network" => ?self.network
        );
        let mut client_lock = self.client.try_lock()?;
        if let Some(client) = client_lock.take() {
            client.abort().await;
        }

        Ok(())
    }

    #[cfg(test)]
    /// Check if the client already exists (test only).
    async fn has_client(&self) -> bool {
        let client_lock = self.client.lock().await;

        client_lock.as_ref().is_some()
    }

    async fn consume_messages_internal(&self) -> StdResult<Vec<(M, PartyId)>> {
        debug!(self.logger, "Waiting for messages from DMQ...");
        let mut client_guard = self.get_client().await?;
        let client = client_guard.as_mut().ok_or(anyhow!("DMQ client does not exist"))?;
        if *client.msg_notification().state() == State::Idle {
            client
                .msg_notification()
                .send_request_messages_blocking()
                .await
                .with_context(|| "Failed to request notifications from DMQ server: {}")?;
        }

        let reply = client
            .msg_notification()
            .recv_next_reply()
            .await
            .with_context(|| "Failed to receive notifications from DMQ server")?;
        debug!(self.logger, "Received single signatures from DMQ"; "messages" => ?reply);

        reply
            .0
            .into_iter()
            .map(|dmq_message| {
                let opcert = OpCert::try_from_bytes(&dmq_message.operational_certificate)
                    .with_context(|| "Failed to parse operational certificate")?;
                let party_id = opcert.compute_protocol_party_id()?;
                let payload = M::try_from_bytes(&dmq_message.msg_body)
                    .with_context(|| "Failed to parse DMQ message body")?;

                Ok((payload, party_id))
            })
            .collect::<StdResult<Vec<_>>>()
            .with_context(|| "Failed to parse DMQ messages")
    }
}

#[async_trait::async_trait]
impl<M: TryFromBytes + Debug + Sync + Send> DmqConsumerClient<M> for DmqConsumerClientPallas<M> {
    async fn consume_messages(&self) -> StdResult<Vec<(M, PartyId)>> {
        let messages = self.consume_messages_internal().await;
        if messages.is_err() {
            self.drop_client().await?;
        }

        messages
    }
}

impl<M: TryFromBytes + Debug> Drop for DmqConsumerClientPallas<M> {
    fn drop(&mut self) {
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async {
                if let Err(e) = self.drop_client().await {
                    error!(self.logger, "Failed to drop DMQ consumer client: {}", e);
                }
            });
        });
    }
}

#[cfg(all(test, unix))]
mod tests {

    use std::{fs, future, time::Duration, vec};

    use mithril_common::{crypto_helper::TryToBytes, current_function, test::TempDir};
    use pallas_network::{
        facades::DmqServer,
        miniprotocols::{localmsgnotification, localmsgsubmission::DmqMsg},
    };
    use tokio::{net::UnixListener, task::JoinHandle, time::sleep};

    use crate::test::{TestLogger, payload::DmqMessageTestPayload};

    use super::*;

    fn create_temp_dir(folder_name: &str) -> PathBuf {
        TempDir::create_with_short_path("dmq_consumer", folder_name)
    }

    fn fake_msgs() -> Vec<DmqMsg> {
        vec![
            DmqMsg {
                msg_id: vec![0, 1],
                msg_body: DmqMessageTestPayload::new(b"msg_1").to_bytes_vec().unwrap(),
                kes_signature: vec![0, 1, 2, 3],
                kes_period: 10,
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
                cold_verification_key: vec![0, 1, 2, 3, 4, 5],
                expires_at: 100,
            },
            DmqMsg {
                msg_id: vec![1, 2],
                msg_body: DmqMessageTestPayload::new(b"msg_2").to_bytes_vec().unwrap(),
                kes_signature: vec![1, 2, 3, 4],
                kes_period: 11,
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
                cold_verification_key: vec![0, 1, 2, 3, 4, 5],
                expires_at: 101,
            },
        ]
    }

    fn setup_dmq_server(
        socket_path: PathBuf,
        reply_messages: Vec<DmqMsg>,
    ) -> JoinHandle<DmqServer> {
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
                    server_msg.send_reply_messages_blocking(reply_messages).await.unwrap();
                    assert_eq!(*server_msg.state(), localmsgnotification::State::Idle);
                } else {
                    // server waits if no message available
                    future::pending().await
                }

                server
            }
        })
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn pallas_dmq_consumer_client_succeeds_when_messages_are_available() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let reply_messages = fake_msgs();
        let server = setup_dmq_server(socket_path.clone(), reply_messages);
        let client = tokio::spawn(async move {
            // sleep to avoid refused connection from the server
            tokio::time::sleep(Duration::from_millis(10)).await;

            let consumer = DmqConsumerClientPallas::new(
                socket_path,
                CardanoNetwork::TestNet(0),
                TestLogger::stdout(),
            );

            consumer.consume_messages().await.unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let messages = client_res.unwrap();

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
            messages
        );
    }

    #[tokio::test]
    async fn pallas_dmq_consumer_client_blocks_when_no_message_available() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let reply_messages = vec![];
        let server = setup_dmq_server(socket_path.clone(), reply_messages);
        let client = tokio::spawn(async move {
            // sleep to avoid refused connection from the server
            tokio::time::sleep(Duration::from_millis(10)).await;

            let consumer = DmqConsumerClientPallas::<DmqMessageTestPayload>::new(
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

    #[tokio::test(flavor = "multi_thread")]
    async fn pallas_dmq_consumer_client_is_dropped_when_returning_error() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let reply_messages = fake_msgs();
        let server = setup_dmq_server(socket_path.clone(), reply_messages);
        let client = tokio::spawn(async move {
            // sleep to avoid refused connection from the server
            tokio::time::sleep(Duration::from_millis(10)).await;

            let consumer = DmqConsumerClientPallas::<DmqMessageTestPayload>::new(
                socket_path,
                CardanoNetwork::TestNet(0),
                TestLogger::stdout(),
            );

            consumer.consume_messages().await.unwrap();

            consumer
        });

        let (server_res, client_res) = tokio::join!(server, client);
        let consumer = client_res.unwrap();
        let server = server_res.unwrap();
        server.abort().await;

        let client = tokio::spawn(async move {
            assert!(consumer.has_client().await, "Client should exist");

            consumer
                .consume_messages()
                .await
                .expect_err("Consuming messages should fail");

            assert!(
                !consumer.has_client().await,
                "Client should have been dropped after error"
            );

            consumer
        });
        client.await.unwrap();
    }
}
