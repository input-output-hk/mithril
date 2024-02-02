use crate::{
    assertions, Aggregator, AggregatorConfig, Client, Devnet, RelayAggregator, RelaySigner, Signer,
    DEVNET_MAGIC_ID,
};
use anyhow::anyhow;
use mithril_common::chain_observer::{
    CardanoCliChainObserver, CardanoCliRunner, ChainObserver, PallasChainObserver,
};
use mithril_common::entities::{ProtocolParameters, SignedEntityTypeDiscriminants};
use mithril_common::{CardanoNetwork, StdResult};
use slog_scope::info;
use std::borrow::BorrowMut;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tokio::time::sleep;

use super::signer::SignerConfig;

pub struct MithrilInfrastructureConfig {
    pub server_port: u64,
    pub devnet: Devnet,
    pub work_dir: PathBuf,
    pub bin_dir: PathBuf,
    pub cardano_node_version: String,
    pub mithril_run_interval: u32,
    pub mithril_era: String,
    pub mithril_era_reader_adapter: String,
    pub signed_entity_types: Vec<String>,
    pub run_only_mode: bool,
    pub use_p2p_network_mode: bool,
}

pub struct MithrilInfrastructure {
    work_dir: PathBuf,
    bin_dir: PathBuf,
    devnet: Devnet,
    aggregator: Aggregator,
    signers: Vec<Signer>,
    relay_aggregators: Vec<RelayAggregator>,
    relay_signers: Vec<RelaySigner>,
    cardano_chain_observer: Arc<dyn ChainObserver>,
    run_only_mode: bool,
    is_signing_cardano_transactions: bool,
}
impl MithrilInfrastructure {
    pub async fn start(config: &MithrilInfrastructureConfig) -> StdResult<Self> {
        config.devnet.run().await?;
        let devnet_topology = config.devnet.topology();
        let bft_node = devnet_topology
            .bft_nodes
            .first()
            .ok_or_else(|| anyhow!("No BFT node available for the aggregator"))?;
        let chain_observer_type = "pallas";

        let mut aggregator = Aggregator::new(&AggregatorConfig {
            server_port: config.server_port,
            bft_node,
            cardano_cli_path: &config.devnet.cardano_cli_path(),
            work_dir: &config.work_dir,
            bin_dir: &config.bin_dir,
            cardano_node_version: &config.cardano_node_version,
            mithril_run_interval: config.mithril_run_interval,
            mithril_era: &config.mithril_era,
            mithril_era_reader_adapter: &config.mithril_era_reader_adapter,
            mithril_era_marker_address: &config.devnet.mithril_era_marker_address()?,
            signed_entity_types: &config.signed_entity_types,
            chain_observer_type,
        })?;
        aggregator.set_protocol_parameters(&ProtocolParameters {
            k: 75,
            m: 100,
            phi_f: 0.95,
        });
        if config.mithril_era_reader_adapter == "cardano-chain" {
            assertions::register_era_marker(&mut aggregator, &config.devnet, &config.mithril_era)
                .await?;
            sleep(Duration::from_secs(5)).await;
        }
        aggregator.serve()?;

        let mut relay_aggregators: Vec<RelayAggregator> = vec![];
        let mut relay_signers: Vec<RelaySigner> = vec![];
        if config.use_p2p_network_mode {
            info!("Starting the Mithril infrastructure in P2P mode (experimental)");

            let mut relay_aggregator = RelayAggregator::new(
                config.server_port + 100,
                aggregator.endpoint(),
                &config.work_dir,
                &config.bin_dir,
            )?;
            relay_aggregator.start()?;

            for (index, pool_node) in devnet_topology.pool_nodes.iter().enumerate() {
                let mut relay_signer = RelaySigner::new(
                    config.server_port + index as u64 + 200,
                    relay_aggregator.peer_addr().to_owned(),
                    aggregator.endpoint(),
                    pool_node,
                    &config.work_dir,
                    &config.bin_dir,
                )?;
                relay_signer.start()?;

                relay_signers.push(relay_signer);
            }

            relay_aggregators.push(relay_aggregator);
        }

        let mut signers: Vec<Signer> = vec![];
        for (index, pool_node) in devnet_topology.pool_nodes.iter().enumerate() {
            // 50% of signers with key certification if allow unverified signer registration
            // Or 100% of signers otherwise
            // TODO: Should be removed once the signer certification is fully deployed
            let enable_certification =
                index % 2 == 0 || cfg!(not(feature = "allow_skip_signer_certification"));
            let aggregator_endpoint = if config.use_p2p_network_mode {
                relay_signers[index].endpoint()
            } else {
                aggregator.endpoint()
            };
            let mut signer = Signer::new(&SignerConfig {
                aggregator_endpoint,
                pool_node,
                cardano_cli_path: &config.devnet.cardano_cli_path(),
                work_dir: &config.work_dir,
                bin_dir: &config.bin_dir,
                mithril_run_interval: config.mithril_run_interval,
                mithril_era: &config.mithril_era,
                mithril_era_reader_adapter: &config.mithril_era_reader_adapter,
                mithril_era_marker_address: &config.devnet.mithril_era_marker_address()?,
                enable_certification,
            })?;
            signer.start()?;

            signers.push(signer);
        }

        let fallback = CardanoCliChainObserver::new(Box::new(CardanoCliRunner::new(
            config.devnet.cardano_cli_path(),
            bft_node.socket_path.clone(),
            CardanoNetwork::DevNet(DEVNET_MAGIC_ID),
        )));

        let cardano_chain_observer = Arc::new(PallasChainObserver::new(
            &bft_node.socket_path,
            CardanoNetwork::DevNet(DEVNET_MAGIC_ID),
            fallback,
        ));

        Ok(Self {
            work_dir: config.work_dir.to_path_buf(),
            bin_dir: config.bin_dir.to_path_buf(),
            devnet: config.devnet.clone(),
            aggregator,
            signers,
            relay_aggregators,
            relay_signers,
            cardano_chain_observer,
            run_only_mode: config.run_only_mode,
            is_signing_cardano_transactions: config.signed_entity_types.contains(
                &SignedEntityTypeDiscriminants::CardanoTransactions
                    .as_ref()
                    .to_string(),
            ),
        })
    }

    pub fn devnet(&self) -> &Devnet {
        &self.devnet
    }

    pub fn aggregator(&self) -> &Aggregator {
        &self.aggregator
    }

    pub fn aggregator_mut(&mut self) -> &mut Aggregator {
        self.aggregator.borrow_mut()
    }

    pub fn signers(&self) -> &[Signer] {
        &self.signers
    }

    pub fn signers_mut(&mut self) -> &mut [Signer] {
        self.signers.as_mut_slice()
    }

    pub fn relay_aggregators(&self) -> &[RelayAggregator] {
        &self.relay_aggregators
    }

    pub fn relay_signers(&self) -> &[RelaySigner] {
        &self.relay_signers
    }

    pub fn chain_observer(&self) -> Arc<dyn ChainObserver> {
        self.cardano_chain_observer.clone()
    }

    pub fn build_client(&self) -> StdResult<Client> {
        Client::new(self.aggregator.endpoint(), &self.work_dir, &self.bin_dir)
    }

    pub fn run_only_mode(&self) -> bool {
        self.run_only_mode
    }

    pub fn is_signing_cardano_transactions(&self) -> bool {
        self.is_signing_cardano_transactions
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> StdResult<()> {
        self.aggregator().tail_logs(number_of_line).await?;
        for signer in self.signers() {
            signer.tail_logs(number_of_line).await?;
        }
        for relay_aggregator in self.relay_aggregators() {
            relay_aggregator.tail_logs(number_of_line).await?;
        }
        for relay_signer in self.relay_signers() {
            relay_signer.tail_logs(number_of_line).await?;
        }

        Ok(())
    }
}
