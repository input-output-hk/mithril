use anyhow::{anyhow, Context};
use async_trait::async_trait;
use pallas_network::facades::NodeClient;
use pallas_network::miniprotocols::localstate::{queries_v16, Client};
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
    fallback: super::cli_observer::CardanoCliChainObserver,
}

impl From<anyhow::Error> for ChainObserverError {
    fn from(err: anyhow::Error) -> Self {
        ChainObserverError::General(err)
    }
}

impl PallasChainObserver {
    /// Creates a new PallasObserver while accepting a fallback CliRunner
    pub fn new(socket: &Path, network: CardanoNetwork, fallback: CardanoCliChainObserver) -> Self {
        Self {
            socket: socket.to_owned(),
            network,
            fallback,
        }
    }

    /// Creates and returns a new `NodeClient` connected to the specified socket.
    async fn new_client(&self) -> StdResult<NodeClient> {
        let magic = self.network.code();
        let client = NodeClient::connect(&self.socket, magic).await?;

        Ok(client)
    }

    /// Returns a reference to the fallback `CardanoCliChainObserver` instance.
    fn get_fallback(&self) -> StdResult<&CardanoCliChainObserver> {
        Ok(&self.fallback)
    }

    /// Creates and returns a new `NodeClient`, handling any potential errors.
    async fn get_client(&self) -> StdResult<NodeClient> {
        self.new_client()
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "Failed to create new client")
    }

    /// Fetches the current epoch number using the provided `statequery` client.
    async fn get_epoch(&self, statequery: &mut Client) -> StdResult<u32> {
        statequery
            .acquire(None)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "Failed to acquire statequery")?;

        let era = queries_v16::get_current_era(statequery)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "Failed to get current era")?;

        let epoch = queries_v16::get_block_epoch_number(statequery, era)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "Failed to get block epoch number")?;

        Ok(epoch)
    }

    /// Processes a state query with the `NodeClient`, releasing the state query.
    async fn process_statequery(&self, client: &mut NodeClient) -> StdResult<()> {
        let statequery = client.statequery();
        statequery
            .send_release()
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "Send release failed")?;

        statequery
            .send_done()
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "Send done failed")?;

        Ok(())
    }

    /// Synchronizes the `NodeClient` with the cardano server using `chainsync`.
    async fn sync(&self, client: &mut NodeClient) -> StdResult<()> {
        client
            .chainsync()
            .send_done()
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "Chainsync send done failed")?;
        Ok(())
    }

    /// Post-processes a state query afterwards.
    async fn post_process_statequery(&self, client: &mut NodeClient) -> StdResult<()> {
        self.process_statequery(client).await?;
        self.sync(client).await?;

        Ok(())
    }
}

#[async_trait]
impl ChainObserver for PallasChainObserver {
    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
        let mut client = self.get_client().await?;

        let epoch = self.get_epoch(client.statequery()).await?;

        self.post_process_statequery(&mut client).await?;

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
    use crate::{chain_observer::test_cli_runner::TestCliRunner, CardanoNetwork};

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

    /// Creates a new work directory in the system's temporary folder.
    fn create_temp_dir(folder_name: &str) -> PathBuf {
        let temp_dir = std::env::temp_dir().join(folder_name);
        if temp_dir.exists() {
            fs::remove_dir_all(&temp_dir).expect("Previous work dir removal failed");
        }
        fs::create_dir_all(&temp_dir).expect("Work dir creation failed");
        temp_dir
    }

    /// Sets up a mock server.
    async fn setup_server() -> tokio::task::JoinHandle<()> {
        tokio::spawn({
            async move {
                let temp_dir = create_temp_dir("pallas_chain_observer_test");
                let socket_path = temp_dir.join("node.socket").as_path().to_owned();
                if socket_path.exists() {
                    fs::remove_file(&socket_path).expect("Previous socket removal failed");
                }

                let unix_listener = UnixListener::bind(socket_path.as_path()).unwrap();
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
        })
    }

    #[tokio::test]
    async fn get_current_epoch_with_fallback() {
        let server = setup_server().await;
        let client = tokio::spawn(async move {
            let socket_path = std::env::temp_dir().join("pallas_chain_observer_test/node.socket");
            let fallback = CardanoCliChainObserver::new(Box::<TestCliRunner>::default());
            let observer = super::PallasChainObserver::new(
                socket_path.as_path(),
                CardanoNetwork::TestNet(10),
                fallback,
            );
            observer.get_current_epoch().await.unwrap().unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let epoch = client_res.expect("Client failed");
        assert_eq!(epoch, 8);
    }
}
