use crate::mithril::MithrilCommand;
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
            ("STAKE_STORE_DIRECTORY", "./store/stakes"),
        ]);
        let args = vec![
            "--db-directory",
            db_directory.to_str().unwrap(),
            "--server-port",
            &port,
            "--runtime-interval",
            "5",
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

    pub async fn dump_logs_if_crashed(&mut self) -> Result<(), String> {
        match self.process.as_mut() {
            Some(child) => match child.try_wait() {
                Ok(Some(status)) => {
                    if !status.success() {
                        self.command.dump_logs_to_stdout().await?;
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
