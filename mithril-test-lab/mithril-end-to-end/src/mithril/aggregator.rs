use crate::utils::MithrilCommand;
use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

#[derive(Debug)]
pub struct Aggregator {
    server_port: u64,
    command: MithrilCommand,
    process: Option<Child>,
}

impl Aggregator {
    pub fn new(
        server_port: u64,
        db_directory: &Path,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> Result<Self, String> {
        let port = server_port.to_string();
        let env = HashMap::from([
            ("NETWORK", "testnet"),
            ("RUN_INTERVAL", "5000"),
            ("URL_SNAPSHOT_MANIFEST", ""),
            ("SNAPSHOT_STORE_TYPE", "local"),
            ("SNAPSHOT_UPLOADER_TYPE", "local"),
            ("NETWORK_MAGIC", "1097911063"),
            (
                "PENDING_CERTIFICATE_STORE_DIRECTORY",
                "./store/aggregator/pending-certs",
            ),
            ("CERTIFICATE_STORE_DIRECTORY", "./store/aggregator/certs"),
            (
                "VERIFICATION_KEY_STORE_DIRECTORY",
                "./store/aggregator/certs",
            ),
            ("STAKE_STORE_DIRECTORY", "./store/aggregator/stakes"),
            ("SNAPSHOT_STORE_DIRECTORY", "./store/aggregator/snapshots"),
            (
                "SINGLE_SIGNATURE_STORE_DIRECTORY",
                "./store/aggregator/single_signatures",
            ),
        ]);
        let args = vec![
            "--db-directory",
            db_directory.to_str().unwrap(),
            "--server-port",
            &port,
            "-vvv",
        ];

        let command = MithrilCommand::new("mithril-aggregator", work_dir, bin_dir, env, &args)?;

        Ok(Self {
            server_port,
            command,
            process: None,
        })
    }

    pub fn endpoint(&self) -> String {
        format!("http://localhost:{}/aggregator", &self.server_port)
    }

    pub fn start(&mut self) {
        self.process = Some(self.command.start(&[]));
    }

    pub async fn dump_logs(&self) -> Result<(), String> {
        self.command.dump_logs_to_stdout().await
    }

    pub async fn dump_logs_if_crashed(&mut self) -> Result<(), String> {
        match self.process.as_mut() {
            Some(child) => match child.try_wait() {
                Ok(Some(status)) => {
                    if !status.success() {
                        self.dump_logs().await?;
                    }
                    Ok(())
                }
                Ok(None) => Ok(()),
                Err(e) => Err(format!("failed get mithril-aggregator status: {}", e)),
            },
            None => Ok(()),
        }
    }
}
