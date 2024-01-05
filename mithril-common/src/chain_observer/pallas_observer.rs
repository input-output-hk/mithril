use anyhow::{anyhow, Context};
use async_trait::async_trait;
use pallas_addresses::Address;
use pallas_codec::utils::CborWrap;
use pallas_network::facades::NodeClient;
use pallas_network::miniprotocols::localstate::queries_v16::{Addr, Addrs, UTxOByAddress, Values};
use pallas_network::miniprotocols::localstate::{queries_v16, Client};
use pallas_primitives::ToCanonicalJson;
use std::path::{Path, PathBuf};

use crate::chain_observer::interface::*;
use crate::chain_observer::{ChainAddress, TxDatum};
use crate::crypto_helper::{KESPeriod, OpCert};
use crate::entities::StakeDistribution;
use crate::CardanoNetwork;
use crate::{entities::Epoch, StdResult};

use super::model::{inspect, Datum, Datums};
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
    fn get_fallback(&self) -> &CardanoCliChainObserver {
        &self.fallback
    }

    /// Creates and returns a new `NodeClient`, handling any potential errors.
    async fn get_client(&self) -> StdResult<NodeClient> {
        self.new_client()
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver Failed to create new client")
    }

    /// Fetches the current epoch number using the provided `statequery` client.
    async fn get_epoch(&self, statequery: &mut Client) -> StdResult<u32> {
        statequery
            .acquire(None)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to acquire statequery")?;

        let era = queries_v16::get_current_era(statequery)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to get current era")?;

        let epoch = queries_v16::get_block_epoch_number(statequery, era)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to get block epoch number")?;

        Ok(epoch)
    }

    /// Returns inline datum bytes from the given `Values` instance.
    fn get_datum_bytes(&self, utxo: &Values) -> Vec<u8> {
        let bytes = utxo.inline_datum.as_ref().unwrap().1.clone();
        let bytes = CborWrap(bytes).to_vec();
        inspect(bytes)
    }

    /// Returns inline datums from the given `Values` instance.
    fn inspect_datum(&self, utxo: &Values) -> Datum {
        let datum = self.get_datum_bytes(utxo);

        inspect(datum)
    }

    /// Serializes datum to `TxDatum` instance.
    fn serialize_datum(&self, utxo: &Values) -> TxDatum {
        let datum = self.inspect_datum(utxo);
        let serialized = serde_json::to_string(&datum.to_json()).expect("Failed to serialize");

        TxDatum(serialized)
    }

    /// Maps the given `UTxOByAddress` instance to Datums.
    fn map_datums(&self, transaction: UTxOByAddress) -> Datums {
        transaction
            .utxo
            .iter()
            .filter_map(
                |(_, utxo)| utxo.inline_datum.as_ref().map(
                    |_| self.serialize_datum(utxo)
                )
            )
            .collect()
    }

    /// Returns a vector of `TxDatum` instances.
    async fn get_utxo_datums(
        &self,
        client: &mut NodeClient,
        address: &ChainAddress,
    ) -> Result<Datums, ChainObserverError> {
        let statequery = client.statequery();
        let utxo = self.get_utxo_by_address(statequery, address).await?;

        Ok(self.map_datums(utxo))
    }

    /// Fetches the current UTxO by address using the provided `statequery` client.
    async fn get_utxo_by_address(
        &self,
        statequery: &mut Client,
        address: &ChainAddress,
    ) -> StdResult<UTxOByAddress> {
        statequery
            .acquire(None)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver Failed to acquire statequery")?;

        let era = queries_v16::get_current_era(statequery)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver Failed to get current era")?;

        let addr: Address = Address::from_bech32(address)
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver Failed to parse address")?;

        let addr: Addr = addr.to_vec().into();
        let addrs: Addrs = vec![addr];
        let utxo = queries_v16::get_utxo_by_address(statequery, era, addrs)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver Failed to get utxo")?;

        Ok(utxo)
    }

    /// Processes a state query with the `NodeClient`, releasing the state query.
    async fn process_statequery(&self, client: &mut NodeClient) -> StdResult<()> {
        let statequery = client.statequery();
        statequery
            .send_release()
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver send release failed")?;

        statequery
            .send_done()
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver send done failed")?;

        Ok(())
    }

    /// Synchronizes the `NodeClient` with the cardano server using `chainsync`.
    async fn sync(&self, client: &mut NodeClient) -> StdResult<()> {
        client
            .chainsync()
            .send_done()
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver chainsync send done failed")?;
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

        client.abort().await;

        Ok(Some(Epoch(epoch as u64)))
    }

    async fn get_current_datums(
        &self,
        address: &ChainAddress,
    ) -> Result<Datums, ChainObserverError> {
        let mut client = self.get_client().await?;

        let datums = self.get_utxo_datums(&mut client, address).await?;

        self.post_process_statequery(&mut client).await?;

        client.abort().await;

        Ok(datums)
    }

    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError> {
        let fallback = self.get_fallback();
        fallback.get_current_stake_distribution().await
    }

    async fn get_current_kes_period(
        &self,
        opcert: &OpCert,
    ) -> Result<Option<KESPeriod>, ChainObserverError> {
        let fallback = self.get_fallback();
        fallback.get_current_kes_period(opcert).await
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use pallas_codec::utils::{AnyCbor, AnyUInt, KeyValuePairs, TagWrap};
    use pallas_crypto::hash::Hash;
    use pallas_network::miniprotocols::localstate::{self, queries_v16::Value, ClientQueryRequest};
    use tokio::net::UnixListener;

    use super::*;
    use crate::{chain_observer::test_cli_runner::TestCliRunner, CardanoNetwork};

    fn get_fake_utxo_by_address() -> UTxOByAddress {
        let tx_hex = "1e4e5cf2889d52f1745b941090f04a65dea6ce56c5e5e66e69f65c8e36347c17";
        let txbytes: [u8; 32] = hex::decode(tx_hex).unwrap().try_into().unwrap();
        let transaction_id = Hash::from(txbytes);
        let index = AnyUInt::MajorByte(2);
        let lovelace = AnyUInt::MajorByte(2);
        let hex_datum = "98A718D81879189F18D81879189F1858181C18C918CF18711866181E185316189118BA1850187018DA18EA185E189918BF181B18C018E10718841837181C183218B7188C1218FC1858181C18621884183E181918D9187B1832187518AF184D18C906188A188F18E5183E18381853183D1618A1187F18DB1819186E18FA1860186B18FF18D81879189F18401840181B0000000118E51861184E18CB0018FF18D81879189F000018FF18D81879189F189F18D81879189F1858181C182D181E15181918E7182A182B187C18EA18C518F1184F18D1187118C318B118A3184418E618F6184C06186718F418BF18621828041858181C187A1896184718D2041888187018A01872186F187818621818186318E01837189718DC1718B918461847183A183518DE18D4185F187518FF18FF189F181A1819188B18DB18BA18FF18FF18FF";
        let datum = hex::decode(hex_datum).unwrap().into();
        let tag = TagWrap::<_, 24>::new(datum);
        let inline_datum = Some((1_u16, tag));

        let address: Address =
            Address::from_bech32("addr_test1vr80076l3x5uw6n94nwhgmv7ssgy6muzf47ugn6z0l92rhg2mgtu0")
                .unwrap();
        let address: Addr = address.to_vec().into();
        let values = localstate::queries_v16::Values {
            address,
            amount: Value::Coin(lovelace),
            inline_datum,
        };

        let utxo = KeyValuePairs::from(vec![(
            localstate::queries_v16::UTxO {
                transaction_id,
                index,
            },
            values,
        )]);

        localstate::queries_v16::UTxOByAddress { utxo }
    }

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
            localstate::queries_v16::Request::LedgerQuery(
                localstate::queries_v16::LedgerQuery::BlockQuery(
                    _,
                    localstate::queries_v16::BlockQuery::GetUTxOByAddress(_),
                ),
            ) => AnyCbor::from_encode(get_fake_utxo_by_address()),
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

    #[tokio::test]
    async fn get_current_datums_with_fallback() {
        let server = setup_server().await;
        let client = tokio::spawn(async move {
            let socket_path = std::env::temp_dir().join("pallas_chain_observer_test/node.socket");
            let fallback = CardanoCliChainObserver::new(Box::<TestCliRunner>::default());
            let observer = super::PallasChainObserver::new(
                socket_path.as_path(),
                CardanoNetwork::TestNet(10),
                fallback,
            );
            let address =
                "addr_test1vr80076l3x5uw6n94nwhgmv7ssgy6muzf47ugn6z0l92rhg2mgtu0".to_string();
            observer.get_current_datums(&address).await.unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let datums = client_res.expect("Client failed");
        assert_eq!(vec![TxDatum(r#"{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"c9cf71661e531691ba5070daea5e99bf1bc0e10784371c32b78c12fc"},{"bytes":"62843e19d97b3275af4dc9068a8fe53e38533d16a17fdb196efa606b"}]},{"constructor":0,"fields":[{"bytes":""},{"bytes":""},{"int":8143326923},{"int":0}]},{"constructor":0,"fields":[{"int":0},{"int":0}]},{"constructor":0,"fields":[{"list":[{"constructor":0,"fields":[{"bytes":"2d1e1519e72a2b7ceac5f14fd171c3b1a344e6f64c0667f4bf622804"},{"bytes":"7a9647d2048870a0726f78621863e03797dc17b946473a35ded45f75"}]}]},{"list":[{"int":428596154}]}]}]}"#.to_string())], datums);
    }
}
