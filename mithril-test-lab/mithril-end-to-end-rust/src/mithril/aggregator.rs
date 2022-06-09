use crate::mithril::MithrilProcess;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct Aggregator {
    server_port: u64,
    db_directory: PathBuf,
    process: Option<MithrilProcess>,
}

impl Aggregator {
    pub fn new(server_port: u64, db_directory: &Path) -> Self {
        Self {
            server_port,
            db_directory: db_directory.to_path_buf(),
            process: None,
        }
    }

    pub fn endpoint(&self) -> String {
        format!("http://localhost:{}/aggregator", &self.server_port)
    }

    pub fn start(&mut self, work_dir: &Path, bin_dir: &Path) -> Result<(), String> {
        let port = self.server_port.to_string();
        let env = HashMap::from([
            ("NETWORK", "testnet"),
            (
                "URL_SNAPSHOT_MANIFEST",
                "https://storage.googleapis.com/cardano-testnet/snapshots.json",
            ),
            ("SNAPSHOT_STORE_TYPE", "local"),
            ("SNAPSHOT_UPLOADER_TYPE", "local"),
            (
                "PENDING_CERTIFICATE_STORE_DIRECTORY",
                "./store/pending-certs",
            ),
            ("CERTIFICATE_STORE_DIRECTORY", "./store/certs"),
            ("VERIFICATION_KEY_STORE_DIRECTORY", "./store/certs"),
        ]);
        let args = vec![
            "--db-directory",
            self.db_directory.to_str().unwrap(),
            "--server-port",
            &port,
            "--runtime-interval",
            "5",
            "-vvv",
        ];

        self.process = Some(MithrilProcess::start(
            "mithril-aggregator",
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
