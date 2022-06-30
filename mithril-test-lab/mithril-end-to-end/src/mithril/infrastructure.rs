use crate::{Aggregator, Client, Devnet, Signer};
use std::borrow::BorrowMut;
use std::path::{Path, PathBuf};

pub struct MithrilInfrastructure {
    work_dir: PathBuf,
    bin_dir: PathBuf,
    aggregator: Aggregator,
    signers: Vec<Signer>,
}

impl MithrilInfrastructure {
    pub async fn start(
        server_port: u64,
        devnet: Devnet,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> Result<Self, String> {
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
        aggregator.start()?;

        let mut signers: Vec<Signer> = vec![];
        for (i, pool_node) in devnet_topology.pool_nodes.iter().enumerate() {
            let mut signer = Signer::new(
                aggregator.endpoint(),
                i.to_string(),
                pool_node,
                &devnet.cardano_cli_path(),
                work_dir,
                bin_dir,
            )?;
            signer.start()?;

            signers.push(signer);
        }

        devnet.run().await?;

        Ok(Self {
            work_dir: work_dir.to_path_buf(),
            bin_dir: bin_dir.to_path_buf(),
            aggregator,
            signers,
        })
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

    pub fn build_client(&self) -> Result<Client, String> {
        Client::new(self.aggregator.endpoint(), &self.work_dir, &self.bin_dir)
    }
}
