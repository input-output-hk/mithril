use crate::{Aggregator, Signer};
use std::borrow::BorrowMut;
use std::path::Path;

pub struct MithrilInfrastructure {
    aggregator: Aggregator,
    signer: Signer,
}

impl MithrilInfrastructure {
    pub fn start(
        server_port: u64,
        db_dir: &Path,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> Result<Self, String> {
        let mut aggregator = Aggregator::new(server_port, db_dir);
        let mut signer = Signer::new(aggregator.endpoint(), db_dir);
        aggregator.start(work_dir, bin_dir)?;
        signer.start(work_dir, bin_dir)?;

        Ok(Self { aggregator, signer })
    }

    pub fn aggregator(&self) -> &Aggregator {
        &self.aggregator
    }

    pub fn aggregator_mut(&mut self) -> &mut Aggregator {
        self.aggregator.borrow_mut()
    }

    pub fn signer(&self) -> &Signer {
        &self.signer
    }

    pub fn signer_mut(&mut self) -> &mut Signer {
        self.signer.borrow_mut()
    }
}
