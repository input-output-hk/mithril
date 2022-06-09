use crate::mithril::MithrilProcess;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct Signer {
    aggregator_endpoint: String,
    db_directory: PathBuf,
    process: Option<MithrilProcess>,
}

impl Signer {
    pub fn new(aggregator_endpoint: String, db_directory: &Path) -> Self {
        Self {
            aggregator_endpoint,
            db_directory: db_directory.to_path_buf(),
            process: None,
        }
    }

    pub fn start(&mut self, work_dir: &Path, bin_dir: &Path) -> Result<(), String> {
        let env = HashMap::from([
            ("NETWORK", "testnet"),
            ("PARTY_ID", "0"),
            ("RUN_INTERVAL", "2000"),
            ("AGGREGATOR_ENDPOINT", &self.aggregator_endpoint),
            ("DB_DIRECTORY", self.db_directory.to_str().unwrap()),
        ]);
        let args = vec!["-vvv"];

        self.process = Some(MithrilProcess::start(
            "mithril-signer",
            work_dir,
            bin_dir,
            env,
            &args,
        )?);

        Ok(())
    }

    pub async fn dump_logs_if_crashed(&mut self) -> Result<(), String> {
        if let Some(process) = self.process.as_mut() {
            process.dump_logs_if_crashed().await?;
        }

        Ok(())
    }
}
