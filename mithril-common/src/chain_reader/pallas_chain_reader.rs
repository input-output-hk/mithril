use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use pallas_network::{
    facades::NodeClient,
    miniprotocols::chainsync::{BlockContent, NextResponse},
};

use crate::{entities::ChainPoint, CardanoNetwork, StdResult};

use super::{ChainBlockNextAction, ChainBlockReader};

/// ][PallasChainReader] which reads blocks with chainsync mini protocol
pub struct PallasChainReader {
    socket: PathBuf,
    network: CardanoNetwork,
    client: Option<NodeClient>,
}

impl PallasChainReader {
    /// Creates a new `PallasChainReader` with the specified socket and network.
    pub fn new(socket: &Path, network: CardanoNetwork) -> Self {
        Self {
            socket: socket.to_owned(),
            network,
            client: None,
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
        }

        self.client
            .as_mut()
            .with_context(|| "PallasChainReader failed to get client")
    }

    /// Intersects the point of the chain with the given point.
    async fn intersect_point(&mut self, point: &ChainPoint) -> StdResult<()> {
        let client = self.get_client().await?;
        let chainsync = client.chainsync();

        chainsync
            .find_intersect(vec![point.to_owned().into()])
            .await?;

        Ok(())
    }

    /// Processes the next chain block and returns the appropriate action.
    async fn process_next_chain_block(
        &mut self,
        next: NextResponse<BlockContent>,
    ) -> StdResult<Option<ChainBlockNextAction>> {
        match next {
            NextResponse::RollForward(raw_block, forward_tip) => {
                Ok(Some(ChainBlockNextAction::RollForward {
                    next_point: forward_tip.into(),
                    raw_block: raw_block.to_vec(),
                }))
            }
            NextResponse::RollBackward(rollback_point, _) => {
                Ok(Some(ChainBlockNextAction::RollBackward {
                    rollback_point: rollback_point.into(),
                }))
            }
            NextResponse::Await => Ok(None),
        }
    }
}

impl Drop for PallasChainReader {
    fn drop(&mut self) {
        if let Some(client) = self.client.take() {
            tokio::spawn(async move {
                let _ = client.abort().await;
            });
        }
    }
}

#[async_trait]
impl ChainBlockReader for PallasChainReader {
    async fn set_chain_point(&mut self, point: &ChainPoint) -> StdResult<()> {
        self.intersect_point(point).await
    }

    async fn get_next_chain_block(&mut self) -> StdResult<Option<ChainBlockNextAction>> {
        let client = self.get_client().await?;
        let chainsync = client.chainsync();

        let next = match chainsync.has_agency() {
            true => chainsync.request_next().await?,
            false => chainsync.recv_while_must_reply().await?,
        };

        self.process_next_chain_block(next).await
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use pallas_network::{
        facades::NodeServer,
        miniprotocols::{
            chainsync::{BlockContent, Tip},
            Point,
        },
    };
    use tokio::net::UnixListener;

    use super::*;

    use crate::test_utils::TempDir;

    /// Enum representing the action to be performed by the server.
    enum ServerAction {
        RollBackwards,
        RollForwards,
    }

    /// Enum representing whether the node has agency or not.
    #[derive(Debug, PartialEq)]
    enum HasAgency {
        Yes,
        No,
    }

    /// Returns a fake chain point for testing purposes.
    fn get_fake_chain_point_backwards() -> ChainPoint {
        ChainPoint::from(Point::Specific(
            1654413,
            hex::decode("7de1f036df5a133ce68a82877d14354d0ba6de7625ab918e75f3e2ecb29771c2")
                .unwrap(),
        ))
    }

    /// Returns a fake chain point for testing purposes.
    fn get_fake_chain_point_forwards() -> ChainPoint {
        Tip(
            Point::Specific(
                1654413,
                hex::decode("7de1f036df5a133ce68a82877d14354d0ba6de7625ab918e75f3e2ecb29771c2")
                    .unwrap(),
            ),
            1337,
        )
        .into()
    }

    /// Returns a fake intersection point for testing purposes.
    fn get_fake_intersection_point() -> Point {
        Point::Specific(
            1654413,
            hex::decode("7de1f036df5a133ce68a82877d14354d0ba6de7625ab918e75f3e2ecb29771c2")
                .unwrap(),
        )
    }

    /// Creates a new work directory in the system's temporary folder.
    fn create_temp_dir(folder_name: &str) -> PathBuf {
        TempDir::create_with_short_path("pallas_chain_observer_test", folder_name)
    }

    /// Sets up a mock server for related tests.
    ///
    /// Use the `action` parameter to specify the action to be performed by the server.
    async fn setup_server(
        socket_path: PathBuf,
        action: ServerAction,
        has_agency: HasAgency,
    ) -> tokio::task::JoinHandle<()> {
        tokio::spawn({
            async move {
                if socket_path.exists() {
                    fs::remove_file(&socket_path).expect("Previous socket removal failed");
                }

                let known_point = get_fake_intersection_point();
                let unix_listener = UnixListener::bind(socket_path.as_path()).unwrap();
                let mut server = NodeServer::accept(&unix_listener, 10).await.unwrap();

                let chainsync_server = server.chainsync();

                chainsync_server.recv_while_idle().await.unwrap();

                chainsync_server
                    .send_intersect_found(known_point.clone(), Tip(known_point.clone(), 1337))
                    .await
                    .unwrap();

                chainsync_server.recv_while_idle().await.unwrap();

                if has_agency == HasAgency::No {
                    chainsync_server.send_await_reply().await.unwrap();
                }

                match action {
                    ServerAction::RollBackwards => {
                        chainsync_server
                            .send_roll_backward(known_point.clone(), Tip(known_point.clone(), 1337))
                            .await
                            .unwrap();
                    }
                    ServerAction::RollForwards => {
                        let block = BlockContent(hex::decode("c0ffeec0ffeec0ffee").unwrap());
                        chainsync_server
                            .send_roll_forward(block, Tip(known_point.clone(), 1337))
                            .await
                            .unwrap();
                    }
                }
            }
        })
    }

    #[tokio::test]
    async fn get_next_chain_block_roll_backwards() {
        let socket_path =
            create_temp_dir("get_next_chain_block_roll_backwards").join("node.socket");
        let known_point = get_fake_intersection_point();
        let server = setup_server(
            socket_path.clone(),
            ServerAction::RollBackwards,
            HasAgency::Yes,
        )
        .await;
        let client = tokio::spawn(async move {
            let mut chain_reader =
                PallasChainReader::new(socket_path.as_path(), CardanoNetwork::TestNet(10));

            chain_reader
                .set_chain_point(&ChainPoint::from(known_point.clone()))
                .await
                .unwrap();

            chain_reader.get_next_chain_block().await.unwrap().unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let chain_block = client_res.expect("Client failed to get next chain block");
        match chain_block {
            ChainBlockNextAction::RollBackward { rollback_point } => {
                assert_eq!(rollback_point, get_fake_chain_point_backwards());
            }
            _ => panic!("Unexpected chain block action"),
        }
    }

    #[tokio::test]
    async fn get_next_chain_block_roll_forwards() {
        let socket_path = create_temp_dir("get_next_chain_block_roll_forwards").join("node.socket");
        let known_point = Point::Specific(
            1654413,
            hex::decode("7de1f036df5a133ce68a82877d14354d0ba6de7625ab918e75f3e2ecb29771c2")
                .unwrap(),
        );
        let server = setup_server(
            socket_path.clone(),
            ServerAction::RollForwards,
            HasAgency::Yes,
        )
        .await;
        let client = tokio::spawn(async move {
            let mut chain_reader =
                PallasChainReader::new(socket_path.as_path(), CardanoNetwork::TestNet(10));

            chain_reader
                .set_chain_point(&ChainPoint::from(known_point.clone()))
                .await
                .unwrap();

            chain_reader.get_next_chain_block().await.unwrap().unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let chain_block = client_res.expect("Client failed to get next chain block");
        match chain_block {
            ChainBlockNextAction::RollForward {
                next_point,
                raw_block,
            } => {
                assert_eq!(next_point, get_fake_chain_point_forwards());
                assert_eq!(
                    raw_block.to_vec(),
                    hex::decode("c0ffeec0ffeec0ffee").unwrap()
                );
            }
            _ => panic!("Unexpected chain block action"),
        }
    }

    #[tokio::test]
    async fn get_next_chain_block_has_no_agency() {
        let socket_path = create_temp_dir("get_next_chain_block_has_no_agency").join("node.socket");
        let known_point = get_fake_intersection_point();
        let server = setup_server(
            socket_path.clone(),
            ServerAction::RollForwards,
            HasAgency::No,
        )
        .await;
        let client = tokio::spawn(async move {
            let mut chain_reader =
                PallasChainReader::new(socket_path.as_path(), CardanoNetwork::TestNet(10));

            chain_reader
                .set_chain_point(&ChainPoint::from(known_point.clone()))
                .await
                .unwrap();

            // forces the client to change the chainsync server agency state
            let client = chain_reader.get_client().await.unwrap();
            client.chainsync().request_next().await.unwrap();

            chain_reader.get_next_chain_block().await.unwrap().unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let chain_block = client_res.expect("Client failed to get next chain block");
        match chain_block {
            ChainBlockNextAction::RollForward {
                next_point,
                raw_block,
            } => {
                assert_eq!(next_point, get_fake_chain_point_forwards());
                assert_eq!(
                    raw_block.to_vec(),
                    hex::decode("c0ffeec0ffeec0ffee").unwrap()
                );
            }
            _ => panic!("Unexpected chain block action"),
        }
    }
}
