use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use pallas_network::{facades::NodeClient, miniprotocols::chainsync::NextResponse};

use crate::{entities::ChainPoint, CardanoNetwork, StdResult};

use super::{ChainBlockNextAction, ChainBlockReader, RawChainBlock};

/// ][PallasChainReader] which reads blocks with chainsync mini protocol
pub struct PallasChainReader {
    socket: PathBuf,
    network: CardanoNetwork,
    client: Option<NodeClient>,
}

impl PallasChainReader {
    /// Factory
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
            .with_context(|| "PallasChainObserver failed to create new client")
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
    async fn get_next_chain_block(
        &mut self,
        point: &ChainPoint,
    ) -> StdResult<Option<ChainBlockNextAction>> {
        let client = self.get_client().await?;

        let chainsync = client.chainsync();

        let (intersect_point, _) = chainsync
            .find_intersect(vec![point.to_owned().into()])
            .await?;
        let _ = intersect_point
            .ok_or_else(|| anyhow!("PallasChainReader failed to find intersect"))
            .unwrap();

        match chainsync.request_next().await? {
            NextResponse::RollForward(raw_block, forward_tip) => {
                Ok(Some(ChainBlockNextAction::RollForward {
                    next_point: forward_tip.into(),
                    raw_block: RawChainBlock(raw_block),
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

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf};
    use tokio::net::UnixListener;

    use crate::{chain_reader::ChainBlockNextAction, entities::ChainPoint};

    /// Sets up a mock server for related tests.
    async fn setup_server(
        socket_path: PathBuf,
        queries: Vec<(ChainPoint, ChainBlockNextAction)>,
    ) -> tokio::task::JoinHandle<()> {
        tokio::spawn({
            async move {
                if socket_path.exists() {
                    fs::remove_file(&socket_path).expect("Previous socket removal failed");
                }

                let unix_listener = UnixListener::bind(socket_path.as_path()).unwrap();
                let mut server = pallas_network::facades::NodeServer::accept(&unix_listener, 10)
                    .await
                    .unwrap();
                for query in queries {
                    let (point, action) = query;
                    match action {
                        ChainBlockNextAction::RollForward {
                            raw_block,
                            next_point,
                        } => {
                            server
                                .chainsync()
                                .send_intersect_found(point.into(), next_point.clone().into())
                                .await
                                .unwrap();
                            server
                                .chainsync()
                                .send_roll_forward(raw_block.0, next_point.into())
                                .await
                                .unwrap();
                        }
                        ChainBlockNextAction::RollBackward { rollback_point } => {
                            server
                                .chainsync()
                                .send_roll_backward(point.into(), rollback_point.into())
                                .await
                                .unwrap();
                        }
                    }
                }
            }
        })
    }
}
