use slog_scope::info;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_cardano_node_chain::chain_observer::{ChainObserver, PallasChainObserver};
use mithril_common::entities::{Epoch, PartyId, ProtocolParameters};
use mithril_common::{CardanoNetwork, StdResult};

use crate::mithril::relay_signer::RelaySignerConfiguration;
use crate::{
    Aggregator, AggregatorConfig, Client, DEVNET_MAGIC_ID, Devnet, FullNode, PoolNode,
    RelayAggregator, RelayPassive, RelaySigner, Signer, assertions,
};

use super::signer::SignerConfig;

pub struct MithrilInfrastructureConfig {
    pub number_of_aggregators: u8,
    pub number_of_signers: u8,
    pub server_port: u64,
    pub devnet: Devnet,
    pub work_dir: PathBuf,
    pub store_dir: PathBuf,
    pub artifacts_dir: PathBuf,
    pub bin_dir: PathBuf,
    pub cardano_node_version: String,
    pub mithril_run_interval: u32,
    pub mithril_era: String,
    pub mithril_era_reader_adapter: String,
    pub signed_entity_types: Vec<String>,
    pub run_only_mode: bool,
    pub use_relays: bool,
    pub relay_signer_registration_mode: String,
    pub relay_signature_registration_mode: String,
    pub use_p2p_passive_relays: bool,
    pub use_era_specific_work_dir: bool,
}

impl MithrilInfrastructureConfig {
    pub fn has_leader_follower_signer_registration(&self) -> bool {
        if &self.relay_signer_registration_mode == "passthrough" {
            self.number_of_aggregators > 1
        } else {
            false
        }
    }

    #[cfg(test)]
    pub fn dummy() -> Self {
        Self {
            number_of_aggregators: 1,
            number_of_signers: 1,
            server_port: 8080,
            devnet: Devnet::default(),
            work_dir: PathBuf::from("/tmp/work"),
            store_dir: PathBuf::from("/tmp/store"),
            artifacts_dir: PathBuf::from("/tmp/artifacts"),
            bin_dir: PathBuf::from("/tmp/bin"),
            cardano_node_version: "1.0.0".to_string(),
            mithril_run_interval: 10,
            mithril_era: "era1".to_string(),
            mithril_era_reader_adapter: "adapter1".to_string(),
            signed_entity_types: vec!["type1".to_string()],
            run_only_mode: false,
            use_relays: false,
            relay_signer_registration_mode: "passthrough".to_string(),
            relay_signature_registration_mode: "passthrough".to_string(),
            use_p2p_passive_relays: false,
            use_era_specific_work_dir: false,
        }
    }
}

pub struct MithrilInfrastructure {
    artifacts_dir: PathBuf,
    bin_dir: PathBuf,
    devnet: Devnet,
    aggregators: Vec<Aggregator>,
    signers: Vec<Signer>,
    relay_aggregators: Vec<RelayAggregator>,
    relay_signers: Vec<RelaySigner>,
    relay_passives: Vec<RelayPassive>,
    cardano_chain_observer: Arc<dyn ChainObserver>,
    run_only_mode: bool,
    current_era: RwLock<String>,
    era_reader_adapter: String,
    use_era_specific_work_dir: bool,
}

impl MithrilInfrastructure {
    pub async fn start(config: &MithrilInfrastructureConfig) -> StdResult<Self> {
        let chain_observer_type = "pallas";
        config.devnet.run().await?;
        let devnet_topology = config.devnet.topology();
        let aggregator_cardano_nodes = &devnet_topology.full_nodes;
        let signer_cardano_nodes = &devnet_topology.pool_nodes;
        let signer_party_ids = signer_cardano_nodes
            .iter()
            .map(|s| s.party_id())
            .collect::<StdResult<Vec<PartyId>>>()?;
        let relay_signer_registration_mode = &config.relay_signer_registration_mode;
        let relay_signature_registration_mode = &config.relay_signature_registration_mode;

        let aggregators =
            Self::start_aggregators(config, aggregator_cardano_nodes, chain_observer_type).await?;
        let aggregator_endpoints = aggregators
            .iter()
            .map(|aggregator| aggregator.endpoint())
            .collect::<Vec<_>>();
        let leader_aggregator_endpoint = aggregator_endpoints[0].to_owned();

        let (relay_aggregators, relay_signers, relay_passives) = Self::start_relays(
            config,
            &aggregator_endpoints,
            &signer_party_ids,
            relay_signer_registration_mode.to_owned(),
            relay_signature_registration_mode.to_owned(),
        )?;

        let signers = Self::start_signers(
            config,
            leader_aggregator_endpoint,
            signer_cardano_nodes,
            &relay_signers,
        )
        .await?;

        let cardano_chain_observer = Arc::new(PallasChainObserver::new(
            &aggregator_cardano_nodes[0].socket_path,
            CardanoNetwork::DevNet(DEVNET_MAGIC_ID),
        ));

        Ok(Self {
            bin_dir: config.bin_dir.to_path_buf(),
            artifacts_dir: config.artifacts_dir.to_path_buf(),
            devnet: config.devnet.clone(),
            aggregators,
            signers,
            relay_aggregators,
            relay_signers,
            relay_passives,
            cardano_chain_observer,
            run_only_mode: config.run_only_mode,
            current_era: RwLock::new(config.mithril_era.clone()),
            era_reader_adapter: config.mithril_era_reader_adapter.clone(),
            use_era_specific_work_dir: config.use_era_specific_work_dir,
        })
    }

    async fn register_startup_era(
        aggregator: &Aggregator,
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

    pub async fn register_switch_to_next_era(&self, next_era: &str) -> StdResult<()> {
        let next_era_epoch = self
            .cardano_chain_observer
            .get_current_epoch()
            .await?
            .unwrap_or_default()
            + 1;
        if self.era_reader_adapter == "cardano-chain" {
            let devnet = self.devnet.clone();
            assertions::register_era_marker(self.aggregator(0), &devnet, next_era, next_era_epoch)
                .await?;
        }
        let mut current_era = self.current_era.write().await;
        *current_era = next_era.to_owned();

        Ok(())
    }

    async fn start_aggregators(
        config: &MithrilInfrastructureConfig,
        pool_nodes: &[FullNode],
        chain_observer_type: &str,
    ) -> StdResult<Vec<Aggregator>> {
        let mut aggregators = vec![];
        let mut leader_aggregator_endpoint: Option<String> = None;
        for (index, full_node) in pool_nodes.iter().enumerate() {
            let aggregator_name = Aggregator::name_suffix(index);
            let aggregator_artifacts_dir = config
                .artifacts_dir
                .join(format!("mithril-aggregator-{aggregator_name}"));
            let aggregator_store_dir = config
                .store_dir
                .join(format!("aggregator-{aggregator_name}"));
            let aggregator = Aggregator::new(&AggregatorConfig {
                index,
                name: &aggregator_name,
                server_port: config.server_port + index as u64,
                full_node,
                cardano_cli_path: &config.devnet.cardano_cli_path(),
                work_dir: &config.work_dir,
                store_dir: &aggregator_store_dir,
                artifacts_dir: &aggregator_artifacts_dir,
                bin_dir: &config.bin_dir,
                cardano_node_version: &config.cardano_node_version,
                mithril_run_interval: config.mithril_run_interval,
                mithril_era: &config.mithril_era,
                mithril_era_reader_adapter: &config.mithril_era_reader_adapter,
                mithril_era_marker_address: &config.devnet.mithril_era_marker_address()?,
                signed_entity_types: &config.signed_entity_types,
                chain_observer_type,
                leader_aggregator_endpoint: &leader_aggregator_endpoint.clone(),
            })?;

            aggregator
                .set_protocol_parameters(&ProtocolParameters {
                    k: 70,
                    m: 105,
                    phi_f: 0.95,
                })
                .await;

            if leader_aggregator_endpoint.is_none()
                && config.has_leader_follower_signer_registration()
            {
                leader_aggregator_endpoint = Some(aggregator.endpoint());
            }

            aggregators.push(aggregator);
        }

        Self::register_startup_era(&aggregators[0], config).await?;

        for aggregator in &aggregators {
            aggregator.serve().await?;
        }

        Ok(aggregators)
    }

    fn start_relays(
        config: &MithrilInfrastructureConfig,
        aggregator_endpoints: &[String],
        signers_party_ids: &[PartyId],
        relay_signer_registration_mode: String,
        relay_signature_registration_mode: String,
    ) -> StdResult<(Vec<RelayAggregator>, Vec<RelaySigner>, Vec<RelayPassive>)> {
        if !config.use_relays {
            return Ok((vec![], vec![], vec![]));
        }

        let mut relay_aggregators: Vec<RelayAggregator> = vec![];
        let mut relay_signers: Vec<RelaySigner> = vec![];
        let mut relay_passives: Vec<RelayPassive> = vec![];
        let leader_aggregator_endpoint = &aggregator_endpoints[0];

        info!("Starting the Mithril infrastructure in P2P mode (experimental)");

        let mut bootstrap_peer_addr = None;
        for (index, aggregator_endpoint) in aggregator_endpoints.iter().enumerate() {
            let mut relay_aggregator = RelayAggregator::new(
                Aggregator::name_suffix(index),
                config.server_port + index as u64 + 100,
                bootstrap_peer_addr.clone(),
                aggregator_endpoint,
                &config.work_dir,
                &config.bin_dir,
            )?;
            if bootstrap_peer_addr.is_none() {
                bootstrap_peer_addr = Some(relay_aggregator.peer_addr().to_owned());
            }
            relay_aggregator.start()?;
            relay_aggregators.push(relay_aggregator);
        }

        for (index, party_id) in signers_party_ids.iter().enumerate() {
            let mut relay_signer = RelaySigner::new(&RelaySignerConfiguration {
                listen_port: config.server_port + index as u64 + 200,
                server_port: config.server_port + index as u64 + 300,
                dial_to: bootstrap_peer_addr.clone(),
                relay_signer_registration_mode: relay_signer_registration_mode.clone(),
                relay_signature_registration_mode: relay_signature_registration_mode.clone(),
                aggregator_endpoint: leader_aggregator_endpoint,
                party_id: party_id.clone(),
                work_dir: &config.work_dir,
                bin_dir: &config.bin_dir,
            })?;
            relay_signer.start()?;

            relay_signers.push(relay_signer);
        }

        if config.use_p2p_passive_relays {
            let mut relay_passive_id = 1;
            for (index, _aggregator_endpoint) in aggregator_endpoints.iter().enumerate() {
                let mut relay_passive_aggregator = RelayPassive::new(
                    config.server_port + index as u64 + 400,
                    bootstrap_peer_addr.clone(),
                    format!("{relay_passive_id}"),
                    &config.work_dir,
                    &config.bin_dir,
                )?;
                relay_passive_aggregator.start()?;
                relay_passives.push(relay_passive_aggregator);
                relay_passive_id += 1;
            }

            for (index, _party_id) in signers_party_ids.iter().enumerate() {
                let mut relay_passive_signer = RelayPassive::new(
                    config.server_port + index as u64 + 500,
                    bootstrap_peer_addr.clone(),
                    format!("{relay_passive_id}"),
                    &config.work_dir,
                    &config.bin_dir,
                )?;
                relay_passive_signer.start()?;
                relay_passives.push(relay_passive_signer);
                relay_passive_id += 1;
            }
        }

        Ok((relay_aggregators, relay_signers, relay_passives))
    }

    async fn start_signers(
        config: &MithrilInfrastructureConfig,
        leader_aggregator_endpoint: String,
        pool_nodes: &[PoolNode],
        relay_signers: &[RelaySigner],
    ) -> StdResult<Vec<Signer>> {
        let mut signers: Vec<Signer> = vec![];

        for (index, pool_node) in pool_nodes.iter().enumerate() {
            // 50% of signers with key certification if allow unverified signer registration
            // Or 100% of signers otherwise
            let enable_certification =
                index % 2 == 0 || cfg!(not(feature = "allow_skip_signer_certification"));
            let aggregator_endpoint = if config.use_relays {
                relay_signers[index].endpoint()
            } else {
                leader_aggregator_endpoint.clone()
            };

            let signer = Signer::new(&SignerConfig {
                signer_number: index + 1,
                aggregator_endpoint,
                pool_node,
                cardano_cli_path: &config.devnet.cardano_cli_path(),
                work_dir: &config.work_dir,
                store_dir: &config
                    .store_dir
                    .join(format!("signer-{}", pool_node.party_id()?)),
                bin_dir: &config.bin_dir,
                mithril_run_interval: config.mithril_run_interval,
                mithril_era: &config.mithril_era,
                mithril_era_reader_adapter: &config.mithril_era_reader_adapter,
                mithril_era_marker_address: &config.devnet.mithril_era_marker_address()?,
                enable_certification,
            })?;
            signer.start().await?;

            signers.push(signer);
        }

        Ok(signers)
    }

    pub async fn stop_nodes(&self) -> StdResult<()> {
        // Note: The aggregators should be stopped *last* since signers depends on it
        info!("Stopping Mithril infrastructure");
        for signer in &self.signers {
            signer.stop().await?;
        }

        for aggregator in &self.aggregators {
            aggregator.stop().await?;
        }

        Ok(())
    }

    pub fn devnet(&self) -> &Devnet {
        &self.devnet
    }

    pub fn aggregators(&self) -> &[Aggregator] {
        &self.aggregators
    }

    pub fn aggregator(&self, index: usize) -> &Aggregator {
        &self.aggregators[index]
    }

    pub fn signers(&self) -> &[Signer] {
        &self.signers
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

    pub async fn build_client(&self, aggregator: &Aggregator) -> StdResult<Client> {
        let work_dir = {
            let mut artifacts_dir = self
                .artifacts_dir
                .join(format!("mithril-client-aggregator-{}", aggregator.name()));
            if self.use_era_specific_work_dir {
                let current_era = self.current_era.read().await;
                artifacts_dir = artifacts_dir.join(format!("era.{current_era}"));
            }
            if !artifacts_dir.exists() {
                fs::create_dir_all(&artifacts_dir)?;
            }

            artifacts_dir
        };

        Client::new(aggregator.endpoint(), &work_dir, &self.bin_dir)
    }

    pub fn run_only_mode(&self) -> bool {
        self.run_only_mode
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> StdResult<()> {
        for aggregator in self.aggregators() {
            aggregator.tail_logs(number_of_line).await?;
        }
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
        for aggregator in self.aggregators() {
            aggregator.last_error_in_logs(number_of_error).await?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::MithrilInfrastructureConfig;

    #[test]
    fn has_leader_follower_signer_registration_succeeds() {
        let config = MithrilInfrastructureConfig {
            relay_signer_registration_mode: "passthrough".to_string(),
            number_of_aggregators: 1,
            ..MithrilInfrastructureConfig::dummy()
        };

        assert!(!config.has_leader_follower_signer_registration());

        let config = MithrilInfrastructureConfig {
            relay_signer_registration_mode: "passthrough".to_string(),
            number_of_aggregators: 2,
            ..MithrilInfrastructureConfig::dummy()
        };

        assert!(config.has_leader_follower_signer_registration());

        let config = MithrilInfrastructureConfig {
            relay_signer_registration_mode: "p2p".to_string(),
            number_of_aggregators: 1,
            ..MithrilInfrastructureConfig::dummy()
        };

        assert!(!config.has_leader_follower_signer_registration());

        let config = MithrilInfrastructureConfig {
            relay_signer_registration_mode: "p2p".to_string(),
            number_of_aggregators: 2,
            ..MithrilInfrastructureConfig::dummy()
        };

        assert!(!config.has_leader_follower_signer_registration());
    }
}
