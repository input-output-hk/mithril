use crate::{Aggregator, Client, Devnet, Signer, DEVNET_MAGIC_ID};
use mithril_common::chain_observer::{CardanoCliChainObserver, CardanoCliRunner};
use mithril_common::CardanoNetwork;
use std::borrow::BorrowMut;
use std::path::{Path, PathBuf};
use std::sync::Arc;

pub struct MithrilInfrastructure {
    work_dir: PathBuf,
    bin_dir: PathBuf,
    devnet: Devnet,
    aggregator: Aggregator,
    signers: Vec<Signer>,
    cardano_chain_observer: Arc<CardanoCliChainObserver>,
}

impl MithrilInfrastructure {
    pub async fn start(
        server_port: u64,
        devnet: Devnet,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> Result<Self, String> {
        devnet.run().await?;
        let devnet_topology = devnet.topology();
        let bft_node = devnet_topology
            .bft_nodes
            .first()
            .ok_or_else(|| "No BFT node available for the aggregator".to_string())?;

        let mut aggregator = Aggregator::new(
            server_port,
            bft_node,
            &devnet.cardano_cli_path(),
            work_dir,
            bin_dir,
        )?;
        aggregator.serve()?;

        let mut signers: Vec<Signer> = vec![];
        for (index, pool_node) in devnet_topology.pool_nodes.iter().enumerate() {
            // 50% of signers with key certification if allow unverified signer registration
            // Or 100% of signers otherwise
            // TODO: Should be removed once the signer certification is fully deployed
            let enable_certification =
                index % 2 == 0 || cfg!(not(feature = "allow_skip_signer_certification"));
            let mut signer = Signer::new(
                aggregator.endpoint(),
                pool_node,
                &devnet.cardano_cli_path(),
                work_dir,
                bin_dir,
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
            cardano_chain_observer,
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

    pub fn chain_observer(&self) -> Arc<CardanoCliChainObserver> {
        self.cardano_chain_observer.clone()
    }

    pub fn build_client(&self) -> Result<Client, String> {
        Client::new(self.aggregator.endpoint(), &self.work_dir, &self.bin_dir)
    }
}
