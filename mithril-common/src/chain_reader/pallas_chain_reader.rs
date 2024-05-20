use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use pallas_network::{
    facades::NodeClient,
    miniprotocols::chainsync::{self, NextResponse},
};

use crate::{entities::ChainPoint, CardanoNetwork, StdResult};

use super::{ChainBlockNextAction, ChainBlockReader, RawChainBlock};

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

    async fn get_client(&mut self) -> StdResult<&mut NodeClient> {
        if self.client.is_none() {
            self.client = Some(self.new_client().await?);
        }

        Ok(self.client.as_mut().unwrap())
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
    async fn process_next_chain_block(
        &mut self,
        next: chainsync::NextResponse<chainsync::BlockContent>,
    ) -> StdResult<Option<ChainBlockNextAction>> {
        match next {
            NextResponse::RollForward(raw_block, forward_tip) => {
                Ok(Some(ChainBlockNextAction::RollForward {
                    next_point: forward_tip.into(),
                    raw_block: RawChainBlock(raw_block),
                }))
            }
            NextResponse::RollBackward(rollback_point, _) => {
                println!("Rolling back to {:?}", rollback_point);
                Ok(Some(ChainBlockNextAction::RollBackward {
                    rollback_point: rollback_point.into(),
                }))
            }
            NextResponse::Await => Ok(None),
        }
    }

    async fn get_next_chain_block(
        &mut self,
        point: &ChainPoint,
    ) -> StdResult<Option<ChainBlockNextAction>> {
        let client = self.get_client().await?;
        let chainsync = client.chainsync();
        println!("1.find_intersect start");

        let intersection = chainsync
            .find_intersect(vec![point.to_owned().into()])
            .await?;
        println!("2.find_intersect done");
        println!("intersection: {:?}", intersection);

        let next = match chainsync.has_agency() {
            true => chainsync.request_next().await?,
            false => chainsync.recv_while_must_reply().await?,
        };

        println!("3.request_next done");
        println!("next: {:?}", next);

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

    fn get_fake_chain_point() -> ChainPoint {
        ChainPoint::from(Point::Specific(
            1654413,
            hex::decode("7de1f036df5a133ce68a82877d14354d0ba6de7625ab918e75f3e2ecb29771c2")
                .unwrap(),
        ))
    }

    /// Creates a new work directory in the system's temporary folder.
    fn create_temp_dir(folder_name: &str) -> PathBuf {
        TempDir::create_with_short_path("pallas_chain_observer_test", folder_name)
    }

    /// Sets up a mock server for related tests.
    ///
    /// Use the `intersections` parameter to define exactly how many
    /// local state queries should be intersepted by the `mock_server`
    /// and avoid any panic errors.
    async fn setup_server(socket_path: PathBuf, intersections: u32) -> tokio::task::JoinHandle<()> {
        tokio::spawn({
            async move {
                if socket_path.exists() {
                    fs::remove_file(&socket_path).expect("Previous socket removal failed");
                }

                let known_point = Point::Specific(
                    1654413,
                    hex::decode("7de1f036df5a133ce68a82877d14354d0ba6de7625ab918e75f3e2ecb29771c2")
                        .unwrap(),
                );

                let unix_listener = UnixListener::bind(socket_path.as_path()).unwrap();
                let mut server = NodeServer::accept(&unix_listener, 10).await.unwrap();

                let chansync_server = server.chainsync();

                chansync_server.recv_while_idle().await.unwrap();

                chansync_server
                    .send_intersect_found(known_point.clone(), Tip(known_point.clone(), 1337))
                    .await
                    .unwrap();

                // chansync_server receives request next from client, sends rollbackwards
                chansync_server.recv_while_idle().await.unwrap();

                chansync_server
                    .send_roll_backward(known_point.clone(), Tip(known_point.clone(), 1337))
                    .await
                    .unwrap();

                // server receives request next from client, sends rollforwards
                chansync_server.recv_while_idle().await.unwrap();

                // mock
                let block = BlockContent(hex::decode("c0ffeec0ffeec0ffee").unwrap());

                chansync_server
                    .send_roll_forward(block, Tip(known_point.clone(), 1337))
                    .await
                    .unwrap();

                // server receives request next from client, sends await reply
                // then rollforwards
                chansync_server.recv_while_idle().await.unwrap();

                chansync_server.send_await_reply().await.unwrap();
                server.abort().await;
            }
        })
    }

    #[tokio::test]
    async fn get_next_chain_block() {
        let socket_path = create_temp_dir("get_next_chain_block").join("node.socket");
        let known_point = Point::Specific(
            1654413,
            hex::decode("7de1f036df5a133ce68a82877d14354d0ba6de7625ab918e75f3e2ecb29771c2")
                .unwrap(),
        );
        let server = setup_server(socket_path.clone(), 1).await;
        let client = tokio::spawn(async move {
            let mut chain_reader =
                PallasChainReader::new(socket_path.as_path(), CardanoNetwork::TestNet(10));
            chain_reader
                .get_next_chain_block(&ChainPoint::from(known_point))
                .await
                .unwrap()
                .unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let chain_block = client_res.expect("Client failed to get next chain block");
        match chain_block {
            ChainBlockNextAction::RollBackward { rollback_point } => {
                assert_eq!(rollback_point, get_fake_chain_point());
            }
            _ => panic!("Unexpected chain block action"),
        }
    }
}
