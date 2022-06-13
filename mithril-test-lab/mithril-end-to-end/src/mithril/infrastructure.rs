use crate::{Aggregator, Client, Signer};
use std::borrow::BorrowMut;
use std::path::Path;

pub struct MithrilInfrastructure {
    aggregator: Aggregator,
    client: Client,
    signer: Signer,
}

impl MithrilInfrastructure {
    pub fn start(
        server_port: u64,
        db_dir: &Path,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> Result<Self, String> {
        let mut aggregator = Aggregator::new(server_port, db_dir, work_dir, bin_dir)?;
        let client = Client::new(aggregator.endpoint(), work_dir, bin_dir)?;
        let mut signer = Signer::new(aggregator.endpoint(), db_dir, work_dir, bin_dir)?;

        aggregator.start();
        signer.start();

        Ok(Self {
            aggregator,
            client,
            signer,
        })
    }

    pub fn aggregator(&self) -> &Aggregator {
        &self.aggregator
    }

    pub fn aggregator_mut(&mut self) -> &mut Aggregator {
        self.aggregator.borrow_mut()
    }

    pub fn client(&self) -> &Client {
        &self.client
    }

    pub fn client_mut(&mut self) -> &mut Client {
        self.client.borrow_mut()
    }

    pub fn signer(&self) -> &Signer {
        &self.signer
    }

    pub fn signer_mut(&mut self) -> &mut Signer {
        self.signer.borrow_mut()
    }
}
