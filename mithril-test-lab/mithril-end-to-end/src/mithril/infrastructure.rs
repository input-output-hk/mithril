use anyhow::Context;
use mithril_common::chain_observer::{ChainObserver, PallasChainObserver};
use mithril_common::entities::{Epoch, PartyId, ProtocolParameters};
use mithril_common::{CardanoNetwork, StdResult};
use slog_scope::info;
use std::borrow::BorrowMut;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use crate::{
    assertions, Aggregator, AggregatorConfig, Client, Devnet, PoolNode, RelayAggregator,
    RelayPassive, RelaySigner, Signer, DEVNET_MAGIC_ID,
};

use super::signer::SignerConfig;

pub struct MithrilInfrastructureConfig {
    pub server_port: u64,
    pub devnet: Devnet,
    pub work_dir: PathBuf,
    pub artifacts_dir: PathBuf,
    pub bin_dir: PathBuf,
    pub cardano_node_version: String,
    pub mithril_run_interval: u32,
    pub mithril_era: String,
    pub mithril_era_reader_adapter: String,
    pub signed_entity_types: Vec<String>,
    pub run_only_mode: bool,
    pub use_p2p_network_mode: bool,
    pub use_p2p_passive_relays: bool,
    pub use_era_specific_work_dir: bool,
}

pub struct MithrilInfrastructure {
    artifacts_dir: PathBuf,
    bin_dir: PathBuf,
    devnet: Devnet,
    aggregator: Aggregator,
    signers: Vec<Signer>,
    relay_aggregators: Vec<RelayAggregator>,
    relay_signers: Vec<RelaySigner>,
    relay_passives: Vec<RelayPassive>,
    cardano_chain_observer: Arc<dyn ChainObserver>,
    run_only_mode: bool,
    current_era: String,
    era_reader_adapter: String,
    use_era_specific_work_dir: bool,
}

impl MithrilInfrastructure {
    pub async fn start(config: &MithrilInfrastructureConfig) -> StdResult<Self> {
        let chain_observer_type = "pallas";
        config.devnet.run().await?;
        let devnet_topology = config.devnet.topology();
        // Clap check that we always have at least 2 pools, no need to be defensive here
        let aggregator_cardano_node = &devnet_topology.pool_nodes[0];
        let signer_cardano_nodes = &devnet_topology.pool_nodes[1..];
        let signer_party_ids = signer_cardano_nodes
            .iter()
            .map(|s| s.party_id())
            .collect::<StdResult<Vec<PartyId>>>()?;

        let aggregator =
            Self::start_aggregator(config, aggregator_cardano_node, chain_observer_type).await?;

        let (relay_aggregators, relay_signers, relay_passives) =
            Self::start_relays(config, aggregator.endpoint(), &signer_party_ids)?;

        let signers = Self::start_signers(
            config,
            aggregator.endpoint(),
            signer_cardano_nodes,
            &relay_signers,
        )?;

        let cardano_chain_observer = Arc::new(PallasChainObserver::new(
            &aggregator_cardano_node.socket_path,
            CardanoNetwork::DevNet(DEVNET_MAGIC_ID),
        ));

        Ok(Self {
            bin_dir: config.bin_dir.to_path_buf(),
            artifacts_dir: config.artifacts_dir.to_path_buf(),
            devnet: config.devnet.clone(),
            aggregator,
            signers,
            relay_aggregators,
            relay_signers,
            relay_passives,
            cardano_chain_observer,
            run_only_mode: config.run_only_mode,
            current_era: config.mithril_era.clone(),
            era_reader_adapter: config.mithril_era_reader_adapter.clone(),
            use_era_specific_work_dir: config.use_era_specific_work_dir,
        })
    }

    async fn register_startup_era(
        aggregator: &mut Aggregator,
        config: &MithrilInfrastructureConfig,
    ) -> StdResult<()> {
        let era_epoch = Epoch(0);
        if config.mithril_era_reader_adapter == "cardano-chain" {
            assertions::register_era_marker(
                aggregator,
                &config.devnet,
                &config.mithril_era,
                era_epoch,
            )
            .await?;
        }

        Ok(())
    }

    pub async fn register_switch_to_next_era(&mut self, next_era: &str) -> StdResult<()> {
        let next_era_epoch = self
            .chain_observer()
            .get_current_epoch()
            .await?
            .unwrap_or_default()
            + 1;
        if self.era_reader_adapter == "cardano-chain" {
            assertions::register_era_marker(
                &mut self.aggregator,
                &self.devnet,
                next_era,
                next_era_epoch,
            )
            .await?;
        }
        self.current_era = next_era.to_owned();

        Ok(())
    }

    async fn start_aggregator(
        config: &MithrilInfrastructureConfig,
        pool_node: &PoolNode,
        chain_observer_type: &str,
    ) -> StdResult<Aggregator> {
        let aggregator_artifacts_directory = config.artifacts_dir.join("mithril-aggregator");
        if !aggregator_artifacts_directory.exists() {
            fs::create_dir_all(&aggregator_artifacts_directory).with_context(|| {
                format!(
                    "Could not create artifacts directory '{}'",
                    aggregator_artifacts_directory.display()
                )
            })?;
        }

        let mut aggregator = Aggregator::new(&AggregatorConfig {
            server_port: config.server_port,
            pool_node,
            cardano_cli_path: &config.devnet.cardano_cli_path(),
            work_dir: &config.work_dir,
            artifacts_dir: &aggregator_artifacts_directory,
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
            m: 105,
            phi_f: 0.95,
        });

        Self::register_startup_era(&mut aggregator, config).await?;

        aggregator.serve()?;

        Ok(aggregator)
    }

    fn start_relays(
        config: &MithrilInfrastructureConfig,
        aggregator_endpoint: String,
        signers_party_ids: &[PartyId],
    ) -> StdResult<(Vec<RelayAggregator>, Vec<RelaySigner>, Vec<RelayPassive>)> {
        if !config.use_p2p_network_mode {
            return Ok((vec![], vec![], vec![]));
        }

        let mut relay_aggregators: Vec<RelayAggregator> = vec![];
        let mut relay_signers: Vec<RelaySigner> = vec![];
        let mut relay_passives: Vec<RelayPassive> = vec![];

        info!("Starting the Mithril infrastructure in P2P mode (experimental)");

        let mut relay_aggregator = RelayAggregator::new(
            config.server_port + 100,
            &aggregator_endpoint,
            &config.work_dir,
            &config.bin_dir,
        )?;
        relay_aggregator.start()?;

        let mut relay_passive_id = 1;
        if config.use_p2p_passive_relays {
            let mut relay_passive_aggregator = RelayPassive::new(
                config.server_port + 200,
                relay_aggregator.peer_addr().to_owned(),
                format!("{relay_passive_id}"),
                &config.work_dir,
                &config.bin_dir,
            )?;
            relay_passive_aggregator.start()?;
            relay_passives.push(relay_passive_aggregator);
        }

        for (index, party_id) in signers_party_ids.iter().enumerate() {
            let mut relay_signer = RelaySigner::new(
                config.server_port + index as u64 + 300,
                config.server_port + index as u64 + 400,
                relay_aggregator.peer_addr().to_owned(),
                &aggregator_endpoint,
                party_id.clone(),
                &config.work_dir,
                &config.bin_dir,
            )?;
            relay_signer.start()?;

            if config.use_p2p_passive_relays {
                relay_passive_id += 1;
                let mut relay_passive_signer = RelayPassive::new(
                    config.server_port + index as u64 + 500,
                    relay_signer.peer_addr().to_owned(),
                    format!("{relay_passive_id}"),
                    &config.work_dir,
                    &config.bin_dir,
                )?;
                relay_passive_signer.start()?;
                relay_passives.push(relay_passive_signer);
            }

            relay_signers.push(relay_signer);
        }

        relay_aggregators.push(relay_aggregator);

        Ok((relay_aggregators, relay_signers, relay_passives))
    }

    fn start_signers(
        config: &MithrilInfrastructureConfig,
        aggregator_endpoint: String,
        pool_nodes: &[PoolNode],
        relay_signers: &[RelaySigner],
    ) -> StdResult<Vec<Signer>> {
        let mut signers: Vec<Signer> = vec![];

        for (index, pool_node) in pool_nodes.iter().enumerate() {
            // 50% of signers with key certification if allow unverified signer registration
            // Or 100% of signers otherwise
            let enable_certification =
                index % 2 == 0 || cfg!(not(feature = "allow_skip_signer_certification"));
            let aggregator_endpoint = if config.use_p2p_network_mode {
                relay_signers[index].endpoint()
            } else {
                aggregator_endpoint.clone()
            };

            let mut signer = Signer::new(&SignerConfig {
                signer_number: index + 1,
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

        Ok(signers)
    }

    pub async fn stop_nodes(&mut self) -> StdResult<()> {
        // Note: The aggregator should be stopped *last* since signers depends on it
        info!("Stopping Mithril infrastructure");
        for signer in self.signers.as_mut_slice() {
            signer.stop().await?;
        }

        self.aggregator.stop().await?;

        Ok(())
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

    pub fn relay_passives(&self) -> &[RelayPassive] {
        &self.relay_passives
    }

    pub fn chain_observer(&self) -> Arc<dyn ChainObserver> {
        self.cardano_chain_observer.clone()
    }

    pub fn build_client(&self) -> StdResult<Client> {
        let work_dir = {
            let mut artifacts_dir = self.artifacts_dir.join("mithril-client");
            if self.use_era_specific_work_dir {
                artifacts_dir = artifacts_dir.join(format!("era.{}", self.current_era));
            }
            if !artifacts_dir.exists() {
                fs::create_dir_all(&artifacts_dir)?;
            }

            artifacts_dir
        };

        Client::new(self.aggregator.endpoint(), &work_dir, &self.bin_dir)
    }

    pub fn run_only_mode(&self) -> bool {
        self.run_only_mode
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
        for relay_passive in self.relay_passives() {
            relay_passive.tail_logs(number_of_line).await?;
        }

        Ok(())
    }

    pub async fn last_error_in_logs(&self, number_of_error: u64) -> StdResult<()> {
        self.aggregator()
            .last_error_in_logs(number_of_error)
            .await?;

        Ok(())
    }
}
