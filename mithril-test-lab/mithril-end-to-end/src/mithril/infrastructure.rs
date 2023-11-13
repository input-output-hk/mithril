use crate::{Aggregator, Client, Devnet, RelayAggregator, RelaySigner, Signer, DEVNET_MAGIC_ID};
use anyhow::anyhow;
use mithril_common::chain_observer::{CardanoCliChainObserver, CardanoCliRunner};
use mithril_common::entities::ProtocolParameters;
use mithril_common::{CardanoNetwork, StdResult};
use std::borrow::BorrowMut;
use std::path::{Path, PathBuf};
use std::sync::Arc;

pub struct MithrilInfrastructure {
    work_dir: PathBuf,
    bin_dir: PathBuf,
    devnet: Devnet,
    aggregator: Aggregator,
    signers: Vec<Signer>,
    relay_aggregators: Vec<RelayAggregator>,
    relay_signers: Vec<RelaySigner>,
    cardano_chain_observer: Arc<CardanoCliChainObserver>,
    run_only_mode: bool,
}

impl MithrilInfrastructure {
    pub async fn start(
        server_port: u64,
        devnet: Devnet,
        work_dir: &Path,
        bin_dir: &Path,
        mithril_era: &str,
        run_only_mode: bool,
        use_p2p_network_mode: bool,
    ) -> StdResult<Self> {
        devnet.run().await?;
        let devnet_topology = devnet.topology();
        let bft_node = devnet_topology
            .bft_nodes
            .first()
            .ok_or_else(|| anyhow!("No BFT node available for the aggregator"))?;

        let mut aggregator = Aggregator::new(
            server_port,
            bft_node,
            &devnet.cardano_cli_path(),
            work_dir,
            bin_dir,
            mithril_era,
        )?;
        aggregator.set_protocol_parameters(&ProtocolParameters {
            k: 75,
            m: 100,
            phi_f: 0.95,
        });
        aggregator.serve()?;

        let mut relay_aggregators: Vec<RelayAggregator> = vec![];
        let mut relay_signers: Vec<RelaySigner> = vec![];
        if use_p2p_network_mode {
            let mut relay_aggregator =
                RelayAggregator::new(server_port + 100, aggregator.endpoint(), work_dir, bin_dir)?;
            relay_aggregator.start()?;

            for (index, pool_node) in devnet_topology.pool_nodes.iter().enumerate() {
                let mut relay_signer = RelaySigner::new(
                    server_port + index as u64 + 200,
                    relay_aggregator.peer_addr().to_owned(),
                    aggregator.endpoint(),
                    pool_node,
                    work_dir,
                    bin_dir,
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
            let aggregator_endpoint = if use_p2p_network_mode {
                relay_signers[index].endpoint()
            } else {
                aggregator.endpoint()
            };
            let mut signer = Signer::new(
                aggregator_endpoint,
                pool_node,
                &devnet.cardano_cli_path(),
                work_dir,
                bin_dir,
                mithril_era,
                enable_certification,
            )?;
            signer.start()?;

            signers.push(signer);
        }

        let cardano_chain_observer = Arc::new(CardanoCliChainObserver::new(Box::new(
            CardanoCliRunner::new(
                devnet.cardano_cli_path(),
                bft_node.socket_path.clone(),
                CardanoNetwork::DevNet(DEVNET_MAGIC_ID),
            ),
        )));

        Ok(Self {
            work_dir: work_dir.to_path_buf(),
            bin_dir: bin_dir.to_path_buf(),
            devnet,
            aggregator,
            signers,
            relay_aggregators,
            relay_signers,
            cardano_chain_observer,
            run_only_mode,
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

    pub fn chain_observer(&self) -> Arc<CardanoCliChainObserver> {
        self.cardano_chain_observer.clone()
    }

    pub fn build_client(&self) -> StdResult<Client> {
        Client::new(self.aggregator.endpoint(), &self.work_dir, &self.bin_dir)
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

        Ok(())
    }
}
