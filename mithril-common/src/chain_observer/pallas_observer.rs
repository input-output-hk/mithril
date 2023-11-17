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

        let era = queries_v16::get_current_era(statequery)
            .await
            .map_err(|err| ChainObserverError::General(err.into()))?;

        let epoch = queries_v16::get_block_epoch_number(statequery, era)
            .await
            .map_err(|err| ChainObserverError::General(err.into()))?;

        statequery.send_release().await.unwrap();
        statequery.send_done().await.unwrap();

        client.chainsync().send_done().await.unwrap();
        drop(client.plexer_handle);

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

#[cfg(test)]
mod tests {
    use std::fs;

    use pallas_codec::utils::AnyCbor;
    use pallas_network::miniprotocols::localstate::{self, ClientQueryRequest};
    use tokio::net::UnixListener;

    use super::*;
    use crate::CardanoNetwork;

    /// pallas responses mock server.
    async fn mock_server(server: &mut pallas_network::facades::NodeServer) -> AnyCbor {
        let query: localstate::queries_v16::Request =
            match server.statequery().recv_while_acquired().await.unwrap() {
                ClientQueryRequest::Query(q) => q.into_decode().unwrap(),
                x => panic!("unexpected message from client: {x:?}"),
            };

        match query {
            localstate::queries_v16::Request::LedgerQuery(
                localstate::queries_v16::LedgerQuery::HardForkQuery(
                    localstate::queries_v16::HardForkQuery::GetCurrentEra,
                ),
            ) => AnyCbor::from_encode(4),
            localstate::queries_v16::Request::LedgerQuery(
                localstate::queries_v16::LedgerQuery::BlockQuery(
                    _,
                    localstate::queries_v16::BlockQuery::GetEpochNo,
                ),
            ) => AnyCbor::from_encode([8]),
            _ => panic!("unexpected query from client: {query:?}"),
        }
    }

    #[tokio::test]
    async fn get_current_epoch_with_fallback() {
        let server = tokio::spawn({
            async move {
                let socket_path = Path::new("node.socket");
                if socket_path.exists() {
                    fs::remove_file(socket_path).unwrap();
                }
                let unix_listener = UnixListener::bind(socket_path).unwrap();

                let mut server = pallas_network::facades::NodeServer::accept(&unix_listener, 10)
                    .await
                    .unwrap();

                server.statequery().recv_while_idle().await.unwrap();
                server.statequery().send_acquired().await.unwrap();

                let result = mock_server(&mut server).await;
                server.statequery().send_result(result).await.unwrap();

                let result = mock_server(&mut server).await;
                server.statequery().send_result(result).await.unwrap();
            }
        });

        let client = tokio::spawn(async move {
            let observer = super::PallasChainObserver::new_with_fallback(
                &std::path::PathBuf::from("node.socket"),
                CardanoNetwork::TestNet(10),
                &std::path::PathBuf::from("/tmp/cardano-cli"),
            );
            let epoch = observer.get_current_epoch().await.unwrap().unwrap();
            assert_eq!(epoch, 8);
        });

        _ = tokio::join!(client, server);
    }
}
