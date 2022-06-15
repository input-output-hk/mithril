use crate::{Aggregator, Client, Signer};
use std::borrow::BorrowMut;
use std::path::{Path, PathBuf};

pub struct MithrilInfrastructure {
    work_dir: PathBuf,
    bin_dir: PathBuf,
    db_dir: PathBuf,
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
        let mut aggregator = Aggregator::new(server_port, db_dir, work_dir, bin_dir)?;
        let mut signer = Signer::new(aggregator.endpoint(), db_dir, work_dir, bin_dir)?;

        aggregator.start();
        signer.start();

        Ok(Self {
            work_dir: work_dir.to_path_buf(),
            bin_dir: bin_dir.to_path_buf(),
            db_dir: db_dir.to_path_buf(),
            aggregator,
            signer,
        })
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

    pub fn build_client(&self) -> Result<Client, String> {
        Client::new(self.aggregator.endpoint(), &self.work_dir, &self.bin_dir)
    }

    pub fn add_immutable(&self) -> Result<(), Box<dyn std::error::Error>> {
        let db_path = self.db_dir.join("immutable");
        let glob_expr = format!("{}/*.chunk", db_path.to_string_lossy());

        let mut filelist = glob::glob(&glob_expr)?
            .map(|f| {
                str::parse::<usize>(f.unwrap().file_stem().unwrap().to_str().unwrap()).unwrap()
            })
            .collect::<Vec<usize>>();
        filelist.sort();
        let new_number = filelist.pop().unwrap() + 1;

        std::fs::File::create(db_path.join(format!("{:05}.chunk", new_number)))?;
        std::fs::File::create(db_path.join(format!("{:05}.primary", new_number)))?;
        std::fs::File::create(db_path.join(format!("{:05}.secondary", new_number)))?;
        Ok(())
    }
}
