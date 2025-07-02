use std::path::{Path, PathBuf};

use anyhow::{Context, anyhow};
use async_trait::async_trait;
use pallas_network::{
    facades::NodeClient,
    miniprotocols::chainsync::{BlockContent, NextResponse},
};
use pallas_traverse::MultiEraBlock;
use slog::{Logger, debug};

use mithril_common::StdResult;
use mithril_common::entities::CardanoNetwork;
use mithril_common::logging::LoggerExtensions;

use crate::entities::{ChainBlockNextAction, RawCardanoPoint, ScannedBlock};

use super::ChainBlockReader;

/// [PallasChainReader] reads blocks with 'chainsync' mini-protocol
pub struct PallasChainReader {
    socket: PathBuf,
    network: CardanoNetwork,
    client: Option<NodeClient>,
    logger: Logger,
}

impl PallasChainReader {
    /// Creates a new `PallasChainReader` with the specified socket and network.
    pub fn new(socket: &Path, network: CardanoNetwork, logger: Logger) -> Self {
        Self {
            socket: socket.to_owned(),
            network,
            client: None,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Creates and returns a new `NodeClient` connected to the specified socket.
    async fn new_client(&self) -> StdResult<NodeClient> {
        let magic = self.network.code();
        NodeClient::connect(&self.socket, magic)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainReader failed to create a new client")
    }

    /// Returns a mutable reference to the client.
    async fn get_client(&mut self) -> StdResult<&mut NodeClient> {
        if self.client.is_none() {
            self.client = Some(self.new_client().await?);
            debug!(self.logger, "Connected to a new client");
        }

        self.client
            .as_mut()
            .with_context(|| "PallasChainReader failed to get a client")
    }

    #[cfg(all(test, unix))]
    /// Check if the client already exists (test only).
    fn has_client(&self) -> bool {
        self.client.is_some()
    }

    /// Drops the client by aborting the connection and setting it to `None`.
    fn drop_client(&mut self) {
        if let Some(client) = self.client.take() {
            tokio::spawn(async move {
                let _ = client.abort().await;
            });
        }
    }

    /// Intersects the point of the chain with the given point.
    async fn find_intersect_point(&mut self, point: &RawCardanoPoint) -> StdResult<()> {
        let logger = self.logger.clone();
        let client = self.get_client().await?;
        let chainsync = client.chainsync();

        if chainsync.has_agency() {
            debug!(logger, "Has agency, finding intersect point..."; "point" => ?point);
            chainsync.find_intersect(vec![point.to_owned().into()]).await?;
        } else {
            debug!(logger, "Doesn't have agency, no need to find intersect point";);
        }

        Ok(())
    }

    /// Processes a block content next response and returns the appropriate chain block next action.
    async fn process_chain_block_next_action(
        &mut self,
        next: NextResponse<BlockContent>,
    ) -> StdResult<Option<ChainBlockNextAction>> {
        match next {
            NextResponse::RollForward(raw_block, _forward_tip) => {
                let multi_era_block = MultiEraBlock::decode(&raw_block)
                    .with_context(|| "PallasChainReader failed to decode raw block")?;
                let parsed_block = ScannedBlock::convert(multi_era_block);
                Ok(Some(ChainBlockNextAction::RollForward { parsed_block }))
            }
            NextResponse::RollBackward(rollback_point, _) => {
                Ok(Some(ChainBlockNextAction::RollBackward {
                    rollback_point: RawCardanoPoint::from(rollback_point),
                }))
            }
            NextResponse::Await => Ok(None),
        }
    }
}

impl Drop for PallasChainReader {
    fn drop(&mut self) {
        self.drop_client();
    }
}

#[async_trait]
impl ChainBlockReader for PallasChainReader {
    async fn set_chain_point(&mut self, point: &RawCardanoPoint) -> StdResult<()> {
        match self.find_intersect_point(point).await {
            Ok(()) => Ok(()),
            Err(err) => {
                self.drop_client();

                return Err(err);
            }
        }
    }

    async fn get_next_chain_block(&mut self) -> StdResult<Option<ChainBlockNextAction>> {
        let client = self.get_client().await?;
        let chainsync = client.chainsync();
        let next = match chainsync.has_agency() {
            true => chainsync.request_next().await,
            false => chainsync.recv_while_must_reply().await,
        };
        match next {
            Ok(next) => self.process_chain_block_next_action(next).await,
            Err(err) => {
                self.drop_client();

                return Err(err.into());
            }
        }
    }
}

// Windows does not support Unix sockets, nor pallas_network::facades::NodeServer
#[cfg(all(test, unix))]
mod tests {
    use pallas_network::{
        facades::NodeServer,
        miniprotocols::{
            Point,
            chainsync::{BlockContent, Tip},
        },
    };
    use std::fs;
    use tokio::net::UnixListener;

    use mithril_common::{current_function, entities::BlockNumber, test_utils::TempDir};

    use crate::test::TestLogger;

    use super::*;

    /// Enum representing the action to be performed by the server.
    enum ServerAction {
        RollBackward,
        RollForward,
    }

    /// Enum representing whether the node has agency or not.
    #[derive(Debug, PartialEq)]
    enum HasAgency {
        Yes,
        No,
    }

    /// Returns a fake specific point for testing purposes.
    fn get_fake_specific_point() -> Point {
        Point::Specific(
            1654413,
            hex::decode("7de1f036df5a133ce68a82877d14354d0ba6de7625ab918e75f3e2ecb29771c2")
                .unwrap(),
        )
    }

    /// Returns a fake block number for testing purposes.
    fn get_fake_block_number() -> BlockNumber {
        BlockNumber(1337)
    }

    /// Returns a fake cardano raw point for testing purposes.
    fn get_fake_raw_point_backwards() -> RawCardanoPoint {
        RawCardanoPoint::from(get_fake_specific_point())
    }

    /// Creates a new work directory in the system's temporary folder.
    fn create_temp_dir(folder_name: &str) -> PathBuf {
        TempDir::create_with_short_path("pallas_chain_observer_test", folder_name)
    }

    fn get_fake_raw_block() -> Vec<u8> {
        let raw_block =
            include_str!("../../../../../mithril-test-lab/test_data/blocks/shelley1.block");

        hex::decode(raw_block).unwrap()
    }

    fn get_fake_scanned_block() -> ScannedBlock {
        let raw_block = get_fake_raw_block();
        let multi_era_block = MultiEraBlock::decode(&raw_block).unwrap();

        ScannedBlock::convert(multi_era_block)
    }

    /// Sets up a mock server for related tests.
    ///
    /// Use the `action` parameter to specify the action to be performed by the server.
    async fn setup_server(
        socket_path: PathBuf,
        action: ServerAction,
        has_agency: HasAgency,
    ) -> tokio::task::JoinHandle<NodeServer> {
        tokio::spawn({
            async move {
                if socket_path.exists() {
                    fs::remove_file(&socket_path).expect("Previous socket removal failed");
                }

                let known_point = get_fake_specific_point();
                let tip_block_number = get_fake_block_number();
                let unix_listener = UnixListener::bind(socket_path.as_path()).unwrap();
                let mut server = NodeServer::accept(&unix_listener, 10).await.unwrap();

                let chainsync_server = server.chainsync();

                chainsync_server.recv_while_idle().await.unwrap();

                chainsync_server
                    .send_intersect_found(
                        known_point.clone(),
                        Tip(known_point.clone(), *tip_block_number),
                    )
                    .await
                    .unwrap();

                chainsync_server.recv_while_idle().await.unwrap();

                if has_agency == HasAgency::No {
                    chainsync_server.send_await_reply().await.unwrap();
                }

                match action {
                    ServerAction::RollBackward => {
                        chainsync_server
                            .send_roll_backward(
                                known_point.clone(),
                                Tip(known_point.clone(), *tip_block_number),
                            )
                            .await
                            .unwrap();
                    }
                    ServerAction::RollForward => {
                        let block = BlockContent(get_fake_raw_block());
                        chainsync_server
                            .send_roll_forward(block, Tip(known_point.clone(), *tip_block_number))
                            .await
                            .unwrap();
                    }
                }

                server
            }
        })
    }

    #[tokio::test]
    async fn get_next_chain_block_rolls_backward() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let known_point = get_fake_specific_point();
        let server = setup_server(
            socket_path.clone(),
            ServerAction::RollBackward,
            HasAgency::Yes,
        )
        .await;
        let client = tokio::spawn(async move {
            let mut chain_reader = PallasChainReader::new(
                socket_path.as_path(),
                CardanoNetwork::TestNet(10),
                TestLogger::stdout(),
            );

            chain_reader
                .set_chain_point(&RawCardanoPoint::from(known_point.clone()))
                .await
                .unwrap();

            chain_reader.get_next_chain_block().await.unwrap().unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let chain_block = client_res.expect("Client failed to get next chain block");
        match chain_block {
            ChainBlockNextAction::RollBackward { rollback_point } => {
                assert_eq!(rollback_point, get_fake_raw_point_backwards());
            }
            _ => panic!("Unexpected chain block action"),
        }
    }

    #[tokio::test]
    async fn get_next_chain_block_rolls_forward() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let known_point = get_fake_specific_point();
        let server = setup_server(
            socket_path.clone(),
            ServerAction::RollForward,
            HasAgency::Yes,
        )
        .await;
        let client = tokio::spawn(async move {
            let mut chain_reader = PallasChainReader::new(
                socket_path.as_path(),
                CardanoNetwork::TestNet(10),
                TestLogger::stdout(),
            );

            chain_reader
                .set_chain_point(&RawCardanoPoint::from(known_point.clone()))
                .await
                .unwrap();

            chain_reader.get_next_chain_block().await.unwrap().unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let chain_block = client_res.expect("Client failed to get next chain block");
        match chain_block {
            ChainBlockNextAction::RollForward { parsed_block } => {
                assert_eq!(parsed_block, get_fake_scanned_block());
            }
            _ => panic!("Unexpected chain block action"),
        }
    }

    #[tokio::test]
    async fn get_next_chain_block_has_no_agency() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let known_point = get_fake_specific_point();
        let server = setup_server(
            socket_path.clone(),
            ServerAction::RollForward,
            HasAgency::No,
        )
        .await;
        let client = tokio::spawn(async move {
            let mut chain_reader = PallasChainReader::new(
                socket_path.as_path(),
                CardanoNetwork::TestNet(10),
                TestLogger::stdout(),
            );

            chain_reader
                .set_chain_point(&RawCardanoPoint::from(known_point.clone()))
                .await
                .unwrap();

            // forces the client to change the chainsync server agency state
            let client = chain_reader.get_client().await.unwrap();
            client.chainsync().request_next().await.unwrap();

            // make sure that the chainsync client returns an error when attempting to find intersection without agency
            client
                .chainsync()
                .find_intersect(vec![known_point.clone()])
                .await
                .expect_err("chainsync find_intersect without agency should fail");

            // make sure that setting the chain point is harmless when the chainsync client does not have agency
            chain_reader
                .set_chain_point(&RawCardanoPoint::from(known_point.clone()))
                .await
                .unwrap();

            chain_reader.get_next_chain_block().await.unwrap().unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let chain_block = client_res.expect("Client failed to get next chain block");
        match chain_block {
            ChainBlockNextAction::RollForward { parsed_block } => {
                assert_eq!(parsed_block, get_fake_scanned_block());
            }
            _ => panic!("Unexpected chain block action"),
        }
    }

    #[tokio::test]
    async fn cached_client_is_dropped_when_returning_error() {
        let socket_path = create_temp_dir(current_function!()).join("node.socket");
        let socket_path_clone = socket_path.clone();
        let known_point = get_fake_specific_point();
        let server = setup_server(
            socket_path.clone(),
            ServerAction::RollForward,
            HasAgency::Yes,
        )
        .await;
        let client = tokio::spawn(async move {
            let mut chain_reader = PallasChainReader::new(
                socket_path_clone.as_path(),
                CardanoNetwork::TestNet(10),
                TestLogger::stdout(),
            );

            chain_reader
                .set_chain_point(&RawCardanoPoint::from(known_point.clone()))
                .await
                .unwrap();

            chain_reader.get_next_chain_block().await.unwrap().unwrap();

            chain_reader
        });

        let (server_res, client_res) = tokio::join!(server, client);
        let chain_reader = client_res.expect("Client failed to get chain reader");
        let server = server_res.expect("Server failed to get server");
        server.abort().await;

        let client = tokio::spawn(async move {
            let mut chain_reader = chain_reader;

            assert!(chain_reader.has_client(), "Client should exist");

            chain_reader
                .get_next_chain_block()
                .await
                .expect_err("Chain reader get_next_chain_block should fail");

            assert!(
                !chain_reader.has_client(),
                "Client should have been dropped after error"
            );

            chain_reader
        });
        client.await.unwrap();
    }
}
