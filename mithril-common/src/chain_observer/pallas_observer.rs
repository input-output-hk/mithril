use anyhow::anyhow;
use async_trait::async_trait;
use pallas_network::facades::NodeClient;
use pallas_network::miniprotocols::localstate::queries_v16;
use std::path::{Path, PathBuf};

use crate::chain_observer::interface::*;
use crate::chain_observer::{ChainAddress, TxDatum};
use crate::crypto_helper::{KESPeriod, OpCert};
use crate::entities::StakeDistribution;
use crate::CardanoNetwork;
use crate::{entities::Epoch, StdResult};

use super::CardanoCliChainObserver;

/// A runner that uses Pallas library to interact with a Cardano node using N2C Ouroboros mini-protocols
pub struct PallasChainObserver {
    socket: PathBuf,
    network: CardanoNetwork,
    fallback: Option<super::cli_observer::CardanoCliChainObserver>,
}

impl PallasChainObserver {
    /// Creates a new PallasObserver while accepting a fallback CliRunner
    pub fn new_with_fallback(socket: &Path, network: CardanoNetwork, cli_path: &Path) -> Self {
        let fallback = CardanoCliChainObserver::new(Box::new(super::CardanoCliRunner::new(
            cli_path.to_owned(),
            socket.to_owned(),
            network,
        )));

        Self {
            socket: socket.to_owned(),
            network,
            fallback: Some(fallback),
        }
    }

    /// Creates a new PallasObserver
    pub fn new(socket: &Path, network: CardanoNetwork) -> Self {
        Self {
            socket: socket.to_owned(),
            network,
            fallback: None,
        }
    }

    async fn new_client(&self) -> StdResult<NodeClient> {
        let magic = self.network.code();
        let client = NodeClient::connect(&self.socket, magic).await?;

        Ok(client)
    }

    fn get_fallback(&self) -> StdResult<&CardanoCliChainObserver> {
        self.fallback.as_ref().ok_or(anyhow!(
            "Unimplemented and no fallback configured for PallasObserver",
        ))
    }
}

#[async_trait]
impl ChainObserver for PallasChainObserver {
    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
        let mut client = self
            .new_client()
            .await
            .map_err(ChainObserverError::General)?;

        let statequery = client.statequery();

        statequery.acquire(None).await.unwrap();

        // TODO: maybe implicitely get the current era as default
        let era = queries_v16::get_current_era(statequery)
            .await
            .map_err(|err| ChainObserverError::General(err.into()))?;

        let epoch = queries_v16::get_block_epoch_number(statequery, era)
            .await
            .map_err(|err| ChainObserverError::General(err.into()))?;

        statequery.send_release().await.unwrap();
        statequery.send_done().await.unwrap();

        client.chainsync().send_done().await.unwrap();
        client.abort();

        Ok(Some(Epoch(epoch as u64)))
    }

    async fn get_current_datums(
        &self,
        address: &ChainAddress,
    ) -> Result<Vec<TxDatum>, ChainObserverError> {
        let fallback = self.get_fallback().map_err(ChainObserverError::General)?;
        fallback.get_current_datums(address).await
    }

    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError> {
        let fallback = self.get_fallback().map_err(ChainObserverError::General)?;
        fallback.get_current_stake_distribution().await
    }

    async fn get_current_kes_period(
        &self,
        opcert: &OpCert,
    ) -> Result<Option<KESPeriod>, ChainObserverError> {
        let fallback = self.get_fallback().map_err(ChainObserverError::General)?;
        fallback.get_current_kes_period(opcert).await
    }
}
