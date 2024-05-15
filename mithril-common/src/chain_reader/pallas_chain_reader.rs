use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use pallas_network::{facades::NodeClient, miniprotocols::chainsync::NextResponse};
use pallas_traverse::MultiEraBlock;
use slog::Logger;

use crate::{entities::ChainPoint, CardanoNetwork, StdResult};

use super::{ChainBlockNextAction, ChainBlockReader};

/// ][PallasChainReader] which reads blocks with chainsync mini protocol
pub struct PallasChainReader {
    socket: PathBuf,
    network: CardanoNetwork,
    logger: Logger,
}

impl PallasChainReader {
    /// Factory
    pub fn new(socket: &Path, network: CardanoNetwork, logger: Logger) -> Self {
        Self {
            socket: socket.to_owned(),
            network,
            logger,
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
}

#[async_trait]
impl<'a> ChainBlockReader<'a> for PallasChainReader {
    async fn get_next_chain_block(
        &self,
        point: &ChainPoint,
    ) -> StdResult<Option<ChainBlockNextAction<'a>>> {
        let mut client = self.new_client().await?;

        let chainsync = client.chainsync();

        let (intersect_point, _) = chainsync
            .find_intersect(vec![point.to_owned().into()])
            .await?;
        let _ = intersect_point
            .ok_or_else(|| anyhow!("PallasChainReader failed to find intersect"))
            .unwrap();

        match chainsync.request_next().await? {
            NextResponse::RollForward(raw_block, forward_tip) => {
                let block = MultiEraBlock::decode(&raw_block.to_owned())?;
                Ok(Some(ChainBlockNextAction::RollForward {
                    next_point: forward_tip.into(),
                    block,
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
