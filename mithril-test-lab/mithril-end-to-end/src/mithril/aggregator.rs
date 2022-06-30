use crate::devnet::BftNode;
use crate::utils::MithrilCommand;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use tokio::process::Child;

#[derive(Debug)]
pub struct Aggregator {
    server_port: u64,
    db_directory: PathBuf,
    command: MithrilCommand,
    process: Option<Child>,
}

impl Aggregator {
    pub fn new(
        server_port: u64,
        bft_node: &BftNode,
        cardano_cli_path: &Path,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> Result<Self, String> {
        let port = server_port.to_string();
        let env = HashMap::from([
            ("NETWORK", "devnet"),
            ("RUN_INTERVAL", "5000"),
            ("URL_SNAPSHOT_MANIFEST", ""),
            ("SNAPSHOT_STORE_TYPE", "local"),
            ("SNAPSHOT_UPLOADER_TYPE", "local"),
            ("NETWORK_MAGIC", "42"),
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
            (
                "CARDANO_NODE_SOCKET_PATH",
                bft_node.socket_path.to_str().unwrap(),
            ),
            ("CARDANO_CLI_PATH", cardano_cli_path.to_str().unwrap()),
        ]);
        let args = vec![
            "--db-directory",
            bft_node.db_path.to_str().unwrap(),
            "--server-port",
            &port,
            "-vvv",
        ];

        let command = MithrilCommand::new("mithril-aggregator", work_dir, bin_dir, env, &args)?;

        Ok(Self {
            server_port,
            db_directory: bft_node.db_path.clone(),
            command,
            process: None,
        })
    }

    pub fn endpoint(&self) -> String {
        format!("http://localhost:{}/aggregator", &self.server_port)
    }

    pub fn db_directory(&self) -> &Path {
        &self.db_directory
    }

    pub fn start(&mut self) {
        self.process = Some(self.command.start(&[]));
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> Result<(), String> {
        self.command.tail_logs(None, number_of_line).await
    }
}
