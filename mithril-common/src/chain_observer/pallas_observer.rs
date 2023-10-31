use async_trait::async_trait;
use pallas::network::facades::NodeClient;
use pallas::network::miniprotocols::localstate::queries::{BlockQuery, QueryV10, Request};
use std::path::{Path, PathBuf};

use crate::chain_observer::interface::*;
use crate::chain_observer::{ChainAddress, TxDatum};
use crate::crypto_helper::{KESPeriod, OpCert};
use crate::entities::StakeDistribution;
use crate::{entities::Epoch, StdResult};

/// A runner that uses Pallas library to interact with a Cardano node using N2C Ouroboros mini-protocols
pub struct PallasObserver {
    socket: PathBuf,
    magic: u64,
}

impl PallasObserver {
    /// Creates a new PallasObserver
    pub fn new(socket: &Path, magic: u64) -> Self {
        Self {
            socket: socket.to_owned(),
            magic,
        }
    }

    async fn new_client(&self) -> StdResult<NodeClient> {
        let client = NodeClient::connect(&self.socket, self.magic).await?;

        Ok(client)
    }
}

#[async_trait]
impl ChainObserver for PallasObserver {
    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
        let client = self
            .new_client()
            .await
            .map_err(ChainObserverError::General)?;

        let response = client
            .statequery()
            .query(Request::BlockQuery(BlockQuery::GetEpochNo))
            .await
            .map_err(|err| ChainObserverError::General(err.into()))?;

        client.chainsync().send_done().await;
        client.abort();

        // finish: https://github.com/txpipe/pallas/issues/315
        // TODO: map query response to Mithril model
        unimplemented!()
    }

    async fn get_current_datums(
        &self,
        address: &ChainAddress,
    ) -> Result<Vec<TxDatum>, ChainObserverError> {
        let client = self
            .new_client()
            .await
            .map_err(ChainObserverError::General)?;

        // finish https://github.com/txpipe/pallas/issues/317
        // TODO: execute query and map results
        unimplemented!()
    }

    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError> {
        let client = self
            .new_client()
            .await
            .map_err(ChainObserverError::General)?;

        let response = client
            .statequery()
            .query(Request::BlockQuery(BlockQuery::GetStakeDistribution))
            .await
            .map_err(|err| ChainObserverError::General(err.into()))?;

        client.chainsync().send_done().await;
        client.abort();

        // finish: https://github.com/txpipe/pallas/issues/316
        // TODO: map query response to Mithril model
        unimplemented!()
    }

    async fn get_current_kes_period(
        &self,
        opcert: &OpCert,
    ) -> Result<Option<KESPeriod>, ChainObserverError> {
        // TODO: find out how to access KES data
        unimplemented!()
    }
}
