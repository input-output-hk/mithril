use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use pallas_addresses::Address;
use pallas_codec::utils::{Bytes, CborWrap, TagWrap};
use pallas_network::{
    facades::NodeClient,
    miniprotocols::{
        localstate::{
            queries_v16::{
                self, Addr, Addrs, ChainBlockNumber, Genesis, PostAlonsoTransactionOutput,
                StakeSnapshot, Stakes, TransactionOutput, UTxOByAddress,
            },
            Client,
        },
        Point,
    },
};
use pallas_primitives::ToCanonicalJson;

use crate::{
    chain_observer::{interface::*, ChainAddress, TxDatum},
    crypto_helper::{encode_bech32, KESPeriod, OpCert},
    entities::{BlockNumber, ChainPoint, Epoch, SlotNumber, StakeDistribution},
    CardanoNetwork, StdResult,
};

use super::model::{try_inspect, Datum, Datums};

/// A runner that uses Pallas library to interact with a Cardano node using N2C Ouroboros mini-protocols
pub struct PallasChainObserver {
    socket: PathBuf,
    network: CardanoNetwork,
}

impl From<anyhow::Error> for ChainObserverError {
    fn from(err: anyhow::Error) -> Self {
        ChainObserverError::General(err)
    }
}

impl PallasChainObserver {
    /// Creates a new PallasObserver
    pub fn new(socket: &Path, network: CardanoNetwork) -> Self {
        Self {
            socket: socket.to_owned(),
            network,
        }
    }

    /// Creates and returns a new `NodeClient` connected to the specified socket.
    async fn new_client(&self) -> StdResult<NodeClient> {
        let magic = self.network.code();
        let client = NodeClient::connect(&self.socket, magic).await?;

        Ok(client)
    }

    /// Creates and returns a new `NodeClient`, handling any potential errors.
    async fn get_client(&self) -> StdResult<NodeClient> {
        self.new_client()
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to create new client")
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

    /// Returns inline datum tag from the given `Values` instance.
    fn get_datum_tag(&self, utxo: &PostAlonsoTransactionOutput) -> StdResult<TagWrap<Bytes, 24>> {
        Ok(utxo
            .inline_datum
            .as_ref()
            .with_context(|| "PallasChainObserver failed to get inline datum")?
            .1
            .clone())
    }

    /// Returns inline datums from the given `Values` instance.
    fn inspect_datum(&self, utxo: &PostAlonsoTransactionOutput) -> StdResult<Datum> {
        let datum = self.get_datum_tag(utxo)?;
        let datum = CborWrap(datum).to_vec();

        try_inspect::<Datum>(datum)
    }

    /// Serializes datum to `TxDatum` instance.
    fn serialize_datum(&self, utxo: &PostAlonsoTransactionOutput) -> StdResult<TxDatum> {
        let datum = self.inspect_datum(utxo)?;
        let serialized = serde_json::to_string(&datum.to_json())
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to serialize datum")?;

        Ok(TxDatum(serialized))
    }

    /// Maps the given `UTxOByAddress` instance to Datums.
    fn map_datums(&self, transaction: UTxOByAddress) -> StdResult<Datums> {
        transaction
            .utxo
            .iter()
            .filter_map(|(_, utxo)| match utxo {
                TransactionOutput::Current(output) => output
                    .inline_datum
                    .as_ref()
                    .map(|_| self.serialize_datum(output)),
                _ => None,
            })
            .collect::<StdResult<Datums>>()
    }

    /// Returns a vector of `TxDatum` instances.
    async fn get_utxo_datums(
        &self,
        client: &mut NodeClient,
        address: &ChainAddress,
    ) -> Result<Datums, ChainObserverError> {
        let statequery = client.statequery();
        let utxo = self.get_utxo_by_address(statequery, address).await?;

        Ok(self.map_datums(utxo)?)
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
            .with_context(|| "PallasChainObserver failed to acquire statequery")?;

        let era = queries_v16::get_current_era(statequery)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to get current era")?;

        let addr: Address = Address::from_bech32(address)
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to parse address")?;

        let addr: Addr = addr.to_vec().into();
        let addrs: Addrs = vec![addr];
        let utxo = queries_v16::get_utxo_by_address(statequery, era, addrs)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to get utxo")?;

        Ok(utxo)
    }

    /// Fetches the current stake distribution using the provided `statequery` client.
    async fn do_stake_snapshots_state_query(
        &self,
        statequery: &mut Client,
    ) -> StdResult<StakeSnapshot> {
        statequery
            .acquire(None)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to acquire statequery")?;

        let era = queries_v16::get_current_era(statequery)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to get current era")?;

        let state_snapshot = queries_v16::get_stake_snapshots(statequery, era, BTreeSet::new())
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to get stake snapshot")?;

        Ok(state_snapshot)
    }

    /// Returns the stake pool hash from the given bytestring.
    fn get_stake_pool_hash(&self, key: &Bytes) -> Result<String, ChainObserverError> {
        let pool_id_bech32 = encode_bech32("pool", key)
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to encode stake pool hash")?;
        Ok(pool_id_bech32)
    }

    /// Fetches the current stake distribution using the provided `statequery` client.
    async fn get_stake_distribution(
        &self,
        client: &mut NodeClient,
    ) -> Result<Option<StakeDistribution>, ChainObserverError> {
        let statequery = client.statequery();

        let stake_snapshot = self.do_stake_snapshots_state_query(statequery).await?;

        let mut stake_distribution = StakeDistribution::new();

        let have_stakes_in_two_epochs = |stakes: &Stakes| stakes.snapshot_mark_pool > 0;
        for (key, stakes) in stake_snapshot
            .snapshots
            .stake_snapshots
            .iter()
            .filter(|(_, stakes)| have_stakes_in_two_epochs(stakes))
        {
            let pool_hash = self.get_stake_pool_hash(key)?;
            stake_distribution.insert(pool_hash, stakes.snapshot_mark_pool);
        }

        Ok(Some(stake_distribution))
    }

    /// # Calculate Current KES Period
    ///
    /// It calculates the current Key Evolving Signature (KES) period
    /// based on the provided `chain_point` and `slots_per_kes_period`.
    ///
    /// The calculation formula is represented as:
    ///
    /// `current_kes_period = ⌊current_slot_number/slots_per_kes_period⌋`
    ///
    /// where:
    /// - `current_slot_number` represents the current slot number given by the `point` on the chain.
    /// - `slots_per_kes_period` represents the number of slots in a KES period.
    /// - `⌊x⌋` is the floor function which rounds the greatest integer less than or equal to `x`.
    ///
    /// ## Example:
    ///
    /// let (chain_point, slots_per_kes_period) = (Point::new(1), 10);
    /// match calculate_kes_period(&self, chain_point, slots_per_kes_period) {
    ///     Ok(kes_period) => println!("Current KES Period: {}", kes_period),
    ///     Err(e) => println!("Error occurred: {}", e),
    /// }
    async fn calculate_kes_period(
        &self,
        chain_point: Point,
        slots_per_kes_period: u64,
    ) -> Result<KESPeriod, ChainObserverError> {
        if slots_per_kes_period == 0 {
            return Err(anyhow!("slots_per_kes_period must be greater than 0"))
                .with_context(|| "PallasChainObserver failed to calculate kes period")?;
        }

        let current_kes_period = chain_point.slot_or_default() / slots_per_kes_period;
        Ok(u32::try_from(current_kes_period)
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to convert kes period")?)
    }

    /// Fetches the current chain point using the provided `statequery` client.
    async fn do_get_chain_point_state_query(&self, statequery: &mut Client) -> StdResult<Point> {
        let chain_point = queries_v16::get_chain_point(statequery)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to get chain point")?;

        Ok(chain_point)
    }

    /// Fetches the current chain point using the provided `NodeClient`.
    async fn do_get_chain_block_no(&self, statequery: &mut Client) -> StdResult<ChainBlockNumber> {
        let chain_block_number = queries_v16::get_chain_block_no(statequery)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to get chain block number")?;

        Ok(chain_block_number)
    }

    /// Fetches the current era using the provided `statequery` client.
    async fn do_get_current_era_state_query(&self, statequery: &mut Client) -> StdResult<u16> {
        let era = queries_v16::get_current_era(statequery)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to get current era")?;

        Ok(era)
    }

    /// Fetches the current genesis config using the provided `statequery` client.
    async fn do_get_genesis_config_state_query(
        &self,
        statequery: &mut Client,
    ) -> StdResult<Vec<Genesis>> {
        let era = self.do_get_current_era_state_query(statequery).await?;
        let genesis_config = queries_v16::get_genesis_config(statequery, era)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to get genesis config")?;

        Ok(genesis_config)
    }

    /// Fetches the current chain point using the provided `NodeClient`.
    async fn get_chain_point(&self, statequery: &mut Client) -> StdResult<ChainPoint> {
        statequery
            .acquire(None)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to acquire statequery")?;

        let chain_point = self.do_get_chain_point_state_query(statequery).await?;

        let header_hash = match chain_point {
            Point::Origin => None,
            Point::Specific(_at_slot, ref hash) => Some(hex::encode(hash)),
        };

        let chain_block_number = self.do_get_chain_block_no(statequery).await?;

        Ok(ChainPoint {
            slot_number: SlotNumber(chain_point.slot_or_default()),
            block_hash: header_hash.unwrap_or_default(),
            block_number: BlockNumber(chain_block_number.block_number as u64),
        })
    }

    /// Fetches chain point and genesis config through the local statequery.
    /// The KES period is calculated afterwards.
    async fn get_kes_period(
        &self,
        client: &mut NodeClient,
    ) -> Result<Option<KESPeriod>, ChainObserverError> {
        let statequery = client.statequery();

        statequery
            .acquire(None)
            .await
            .map_err(|err| anyhow!(err))
            .with_context(|| "PallasChainObserver failed to acquire statequery")?;

        let chain_point = self.do_get_chain_point_state_query(statequery).await?;

        let genesis_config = self.do_get_genesis_config_state_query(statequery).await?;

        let config = genesis_config
            .first()
            .with_context(|| "PallasChainObserver failed to extract the config")?;

        let current_kes_period = self
            .calculate_kes_period(chain_point, config.slots_per_kes_period as u64)
            .await?;

        Ok(Some(current_kes_period))
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

    async fn get_current_chain_point(&self) -> Result<Option<ChainPoint>, ChainObserverError> {
        let mut client = self.get_client().await?;

        let chain_point = self.get_chain_point(client.statequery()).await?;

        self.post_process_statequery(&mut client).await?;

        client.abort().await;

        Ok(Some(chain_point))
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
        let mut client = self.get_client().await?;

        let stake_distribution = self.get_stake_distribution(&mut client).await?;

        self.post_process_statequery(&mut client).await?;

        client.abort().await;

        Ok(stake_distribution)
    }

    async fn get_current_kes_period(
        &self,
        _opcert: &OpCert,
    ) -> Result<Option<KESPeriod>, ChainObserverError> {
        let mut client = self.get_client().await?;

        let current_kes_period = self.get_kes_period(&mut client).await?;

        self.post_process_statequery(&mut client).await?;

        client.abort().await;

        Ok(current_kes_period)
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use kes_summed_ed25519::{kes::Sum6Kes, traits::KesSk};
    use pallas_codec::utils::{AnyCbor, AnyUInt, KeyValuePairs, TagWrap};
    use pallas_crypto::hash::Hash;
    use pallas_network::miniprotocols::{
        localstate::{
            queries_v16::{
                BlockQuery, ChainBlockNumber, Fraction, Genesis, HardForkQuery, LedgerQuery,
                Request, Snapshots, StakeSnapshot, SystemStart, Value,
            },
            ClientQueryRequest,
        },
        Point,
    };
    use tokio::net::UnixListener;

    use crate::test_utils::TempDir;
    use crate::{crypto_helper::ColdKeyGenerator, CardanoNetwork};

    use super::*;

    fn get_fake_utxo_by_address() -> UTxOByAddress {
        let tx_hex = "1e4e5cf2889d52f1745b941090f04a65dea6ce56c5e5e66e69f65c8e36347c17";
        let tx_bytes: [u8; 32] = hex::decode(tx_hex).unwrap().try_into().unwrap();
        let transaction_id = Hash::from(tx_bytes);
        let index = AnyUInt::MajorByte(2);
        let lovelace = AnyUInt::MajorByte(2);
        let hex_datum = "D8799F58407B226D61726B657273223A5B7B226E616D65223A227468616C6573222C2265706F6368223A307D5D2C227369676E6174757265223A22383566323265626261645840333335376338656132646630363230393766396131383064643335643966336261316432363832633732633864313232383866616438636238643063656565625838366134643665383465653865353631376164323037313836366363313930373466326137366538373864663166393733346438343061227DFF";
        let datum = hex::decode(hex_datum).unwrap().into();
        let tag = TagWrap::<_, 24>::new(datum);
        let inline_datum = Some((1_u16, tag));

        let address: Address =
            Address::from_bech32("addr_test1vr80076l3x5uw6n94nwhgmv7ssgy6muzf47ugn6z0l92rhg2mgtu0")
                .unwrap();
        let address: Addr = address.to_vec().into();
        let values = TransactionOutput::Current(PostAlonsoTransactionOutput {
            address,
            amount: Value::Coin(lovelace),
            inline_datum,
            script_ref: None,
        });
        let utxo = KeyValuePairs::from(vec![(
            queries_v16::UTxO {
                transaction_id,
                index,
            },
            values,
        )]);

        UTxOByAddress { utxo }
    }

    fn get_fake_stake_snapshot() -> StakeSnapshot {
        let stake_snapshots = KeyValuePairs::from(vec![
            (
                Bytes::from(
                    hex::decode("00000036d515e12e18cd3c88c74f09a67984c2c279a5296aa96efe89")
                        .unwrap(),
                ),
                Stakes {
                    snapshot_mark_pool: 300000000001,
                    snapshot_set_pool: 300000000002,
                    snapshot_go_pool: 300000000000,
                },
            ),
            (
                Bytes::from(
                    hex::decode("000000f66e28b0f18aef20555f4c4954234e3270dfbbdcc13f54e799")
                        .unwrap(),
                ),
                Stakes {
                    snapshot_mark_pool: 600000000001,
                    snapshot_set_pool: 600000000002,
                    snapshot_go_pool: 600000000000,
                },
            ),
            (
                Bytes::from(
                    hex::decode("00000110093effbf3ce788aebd3e7506b80322bd3995ad432e61fad5")
                        .unwrap(),
                ),
                Stakes {
                    snapshot_mark_pool: 1200000000001,
                    snapshot_set_pool: 1200000000002,
                    snapshot_go_pool: 1200000000000,
                },
            ),
            (
                Bytes::from(
                    hex::decode("00000ffff93effbf3ce788aebd3e7506b80322bd3995ad432e61fad5")
                        .unwrap(),
                ),
                Stakes {
                    snapshot_mark_pool: 0,
                    snapshot_set_pool: 1300000000002,
                    snapshot_go_pool: 0,
                },
            ),
        ]);

        StakeSnapshot {
            snapshots: Snapshots {
                stake_snapshots,
                snapshot_stake_mark_total: 2100000000003,
                snapshot_stake_set_total: 2100000000006,
                snapshot_stake_go_total: 2100000000000,
            },
        }
    }

    fn get_fake_genesis_config() -> Vec<Genesis> {
        let genesis = Genesis {
            system_start: SystemStart {
                year: 2021,
                day_of_year: 150,
                picoseconds_of_day: 0,
            },
            network_magic: 42,
            network_id: 42,
            active_slots_coefficient: Fraction { num: 6, dem: 10 },
            security_param: 2160,
            epoch_length: 432000,
            slots_per_kes_period: 129600,
            max_kes_evolutions: 62,
            slot_length: 1,
            update_quorum: 5,
            max_lovelace_supply: AnyUInt::MajorByte(2),
        };

        vec![genesis]
    }

    /// pallas responses mock server.
    async fn mock_server(server: &mut pallas_network::facades::NodeServer) -> AnyCbor {
        let query: queries_v16::Request =
            match server.statequery().recv_while_acquired().await.unwrap() {
                ClientQueryRequest::Query(q) => q.into_decode().unwrap(),
                x => panic!("unexpected message from client: {x:?}"),
            };

        match query {
            Request::GetChainPoint => {
                AnyCbor::from_encode(Point::Specific(52851885, vec![1, 2, 3]))
            }
            Request::GetChainBlockNo => AnyCbor::from_encode(ChainBlockNumber {
                slot_timeline: 1,
                block_number: 52851885,
            }),
            Request::LedgerQuery(LedgerQuery::HardForkQuery(HardForkQuery::GetCurrentEra)) => {
                AnyCbor::from_encode(4)
            }
            Request::LedgerQuery(LedgerQuery::BlockQuery(_, BlockQuery::GetEpochNo)) => {
                AnyCbor::from_encode([8])
            }
            Request::LedgerQuery(LedgerQuery::BlockQuery(_, BlockQuery::GetGenesisConfig)) => {
                AnyCbor::from_encode(get_fake_genesis_config())
            }
            Request::LedgerQuery(LedgerQuery::BlockQuery(_, BlockQuery::GetUTxOByAddress(_))) => {
                AnyCbor::from_encode(get_fake_utxo_by_address())
            }
            Request::LedgerQuery(LedgerQuery::BlockQuery(_, BlockQuery::GetStakeSnapshots(_))) => {
                AnyCbor::from_encode(get_fake_stake_snapshot())
            }
            _ => panic!("unexpected query from client: {query:?}"),
        }
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

                let unix_listener = UnixListener::bind(socket_path.as_path()).unwrap();
                let mut server = pallas_network::facades::NodeServer::accept(&unix_listener, 10)
                    .await
                    .unwrap();

                server.statequery().recv_while_idle().await.unwrap();
                server.statequery().send_acquired().await.unwrap();

                for _ in 0..intersections {
                    let result = mock_server(&mut server).await;
                    server.statequery().send_result(result).await.unwrap();
                }
            }
        })
    }

    #[tokio::test]
    async fn get_current_epoch() {
        let socket_path = create_temp_dir("get_current_epoch").join("node.socket");
        let server = setup_server(socket_path.clone(), 2).await;
        let client = tokio::spawn(async move {
            let observer =
                PallasChainObserver::new(socket_path.as_path(), CardanoNetwork::TestNet(10));
            observer.get_current_epoch().await.unwrap().unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let epoch = client_res.expect("Client failed");
        assert_eq!(epoch, 8);
    }

    #[tokio::test]
    async fn get_current_datums() {
        let socket_path = create_temp_dir("get_current_datums").join("node.socket");
        let server = setup_server(socket_path.clone(), 2).await;
        let client = tokio::spawn(async move {
            let observer =
                PallasChainObserver::new(socket_path.as_path(), CardanoNetwork::TestNet(10));
            let address =
                "addr_test1vr80076l3x5uw6n94nwhgmv7ssgy6muzf47ugn6z0l92rhg2mgtu0".to_string();
            observer.get_current_datums(&address).await.unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let datums = client_res.expect("Client failed");
        assert_eq!(vec![TxDatum(r#"{"constructor":0,"fields":[{"bytes":"7b226d61726b657273223a5b7b226e616d65223a227468616c6573222c2265706f6368223a307d5d2c227369676e6174757265223a2238356632326562626164"},{"bytes":"33333537633865613264663036323039376639613138306464333564396633626131643236383263373263386431323238386661643863623864306365656562"},{"bytes":"366134643665383465653865353631376164323037313836366363313930373466326137366538373864663166393733346438343061227d"}]}"#.to_string())], datums);
    }

    #[tokio::test]
    async fn get_current_stake_distribution() {
        let socket_path = create_temp_dir("get_current_stake_distribution").join("node.socket");
        let server = setup_server(socket_path.clone(), 2).await;
        let client = tokio::spawn(async move {
            let observer =
                super::PallasChainObserver::new(socket_path.as_path(), CardanoNetwork::TestNet(10));
            observer.get_current_stake_distribution().await.unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let computed_stake_distribution = client_res.unwrap().unwrap();

        let mut expected_stake_distribution = StakeDistribution::new();
        expected_stake_distribution.insert(
            "pool1qqqqqdk4zhsjuxxd8jyvwncf5eucfskz0xjjj64fdmlgj735lr9".to_string(),
            300000000001,
        );
        expected_stake_distribution.insert(
            "pool1qqqqpanw9zc0rzh0yp247nzf2s35uvnsm7aaesfl2nnejaev0uc".to_string(),
            600000000001,
        );
        expected_stake_distribution.insert(
            "pool1qqqqzyqf8mlm70883zht60n4q6uqxg4a8x266sewv8ad2grkztl".to_string(),
            1200000000001,
        );

        assert_eq!(expected_stake_distribution, computed_stake_distribution);
    }

    #[tokio::test]
    async fn get_current_kes_period() {
        let socket_path = create_temp_dir("get_current_kes_period").join("node.socket");
        let server = setup_server(socket_path.clone(), 3).await;
        let client = tokio::spawn(async move {
            let observer =
                PallasChainObserver::new(socket_path.as_path(), CardanoNetwork::TestNet(10));

            let keypair = ColdKeyGenerator::create_deterministic_keypair([0u8; 32]);
            let mut dummy_key_buffer = [0u8; Sum6Kes::SIZE + 4];
            let mut dummy_seed = [0u8; 32];
            let (_, kes_verification_key) = Sum6Kes::keygen(&mut dummy_key_buffer, &mut dummy_seed);
            let operational_certificate = OpCert::new(kes_verification_key, 0, 0, keypair);
            observer
                .get_current_kes_period(&operational_certificate)
                .await
                .unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let kes_period = client_res.unwrap().unwrap();
        assert_eq!(407, kes_period);
    }

    #[tokio::test]
    async fn calculate_kes_period() {
        let socket_path = create_temp_dir("get_current_kes_period").join("node.socket");
        let observer = PallasChainObserver::new(socket_path.as_path(), CardanoNetwork::TestNet(10));
        let current_kes_period = observer
            .calculate_kes_period(Point::Specific(53536042, vec![1, 2, 3]), 129600)
            .await
            .unwrap();

        assert_eq!(413, current_kes_period);

        let current_kes_period = observer
            .calculate_kes_period(Point::Specific(53524800, vec![1, 2, 3]), 129600)
            .await
            .unwrap();

        assert_eq!(413, current_kes_period);

        let current_kes_period = observer
            .calculate_kes_period(Point::Specific(53649999, vec![1, 2, 3]), 129600)
            .await
            .unwrap();

        assert_eq!(413, current_kes_period);
    }

    #[tokio::test]
    async fn get_chain_point() {
        let socket_path = create_temp_dir("get_chain_point").join("node.socket");
        let server = setup_server(socket_path.clone(), 1).await;
        let client = tokio::spawn(async move {
            let observer =
                PallasChainObserver::new(socket_path.as_path(), CardanoNetwork::TestNet(10));
            let mut client = observer.get_client().await.unwrap();
            let statequery = client.statequery();
            statequery.acquire(None).await.unwrap();
            let chain_point = observer
                .do_get_chain_point_state_query(statequery)
                .await
                .unwrap();
            observer.post_process_statequery(&mut client).await.unwrap();
            client.abort().await;
            chain_point
        });

        let (_, client_res) = tokio::join!(server, client);
        let chain_point = client_res.expect("Client failed");
        assert_eq!(chain_point, Point::Specific(52851885, vec![1, 2, 3]));
    }

    #[tokio::test]
    async fn get_genesis_config() {
        let socket_path = create_temp_dir("get_genesis_config").join("node.socket");
        let server = setup_server(socket_path.clone(), 2).await;
        let client = tokio::spawn(async move {
            let observer =
                PallasChainObserver::new(socket_path.as_path(), CardanoNetwork::TestNet(10));
            let mut client = observer.get_client().await.unwrap();
            let statequery = client.statequery();
            statequery.acquire(None).await.unwrap();
            let genesis_config = observer
                .do_get_genesis_config_state_query(statequery)
                .await
                .unwrap();
            observer.post_process_statequery(&mut client).await.unwrap();
            client.abort().await;
            genesis_config
        });

        let (_, client_res) = tokio::join!(server, client);
        let genesis_config = client_res.expect("Client failed");
        assert_eq!(genesis_config, get_fake_genesis_config());
    }

    #[tokio::test]
    async fn get_current_era() {
        let socket_path = create_temp_dir("get_current_era").join("node.socket");
        let server = setup_server(socket_path.clone(), 1).await;
        let client = tokio::spawn(async move {
            let observer =
                PallasChainObserver::new(socket_path.as_path(), CardanoNetwork::TestNet(10));
            let mut client = observer.get_client().await.unwrap();
            let statequery = client.statequery();
            statequery.acquire(None).await.unwrap();
            let era = observer
                .do_get_current_era_state_query(statequery)
                .await
                .unwrap();
            observer.post_process_statequery(&mut client).await.unwrap();
            client.abort().await;
            era
        });

        let (_, client_res) = tokio::join!(server, client);
        let era = client_res.expect("Client failed");
        assert_eq!(era, 4);
    }

    #[tokio::test]
    async fn get_current_chain_point() {
        let socket_path = create_temp_dir("get_current_chain_point").join("node.socket");
        let server = setup_server(socket_path.clone(), 2).await;
        let client = tokio::spawn(async move {
            let observer =
                PallasChainObserver::new(socket_path.as_path(), CardanoNetwork::TestNet(10));
            observer.get_current_chain_point().await.unwrap()
        });

        let (_, client_res) = tokio::join!(server, client);
        let chain_point = client_res.expect("Client failed");
        assert_eq!(
            chain_point,
            Some(ChainPoint {
                slot_number: SlotNumber(52851885),
                block_hash: "010203".to_string(),
                block_number: BlockNumber(52851885)
            })
        );
    }
}
