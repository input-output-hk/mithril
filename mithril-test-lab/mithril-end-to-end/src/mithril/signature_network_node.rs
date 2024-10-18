use anyhow::Context;
use mithril_common::StdResult;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use tokio::process::Child;

use crate::utils::MithrilCommand;

#[derive(Debug)]
pub struct SignatureNetworkNodeConfig<'a> {
    pub id: &'a str,
    pub parent_work_dir: &'a Path,
    pub bin_dir: &'a Path,
    pub peers_input_directories: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct SignatureNetworkNode {
    id: String,
    socket_path: PathBuf,
    command: MithrilCommand,
    process: Option<Child>,
}

impl SignatureNetworkNode {
    pub const INPUT_DIR_NAME: &'static str = "input";
    pub const NODE_SOCK_NAME: &'static str = "node.sock";

    pub fn new(node_config: &SignatureNetworkNodeConfig) -> StdResult<Self> {
        let id = node_config.id.to_string();
        let node_dir = Self::compute_work_dir_path(node_config.parent_work_dir, &id);
        std::fs::create_dir(&node_dir).with_context(|| {
            format!(
                "Failed to create node '{id}' directory: {}",
                node_dir.display()
            )
        })?;

        let input_dir = node_dir
            .join(Self::INPUT_DIR_NAME)
            .to_string_lossy()
            .to_string();
        let socket_path = node_dir
            .join(Self::NODE_SOCK_NAME)
            .to_string_lossy()
            .to_string();
        let peers_input_directories = node_config
            .peers_input_directories
            .iter()
            .map(|d| d.to_string_lossy().to_string())
            .collect::<Vec<String>>()
            .join(" ");

        let env = HashMap::from([
            ("ID", id.as_ref()),
            ("INPUT_DIRECTORY", input_dir.as_ref()),
            ("SOCKET_PATH", socket_path.as_ref()),
            ("PEERS_INPUT_DIRECTORIES", peers_input_directories.as_ref()),
        ]);
        let args = vec!["-vvv"];

        let mut command = MithrilCommand::new(
            "signature-network-node",
            &node_dir,
            node_config.bin_dir,
            env,
            &args,
        )?;
        command.set_log_name(format!("signature-network-node-{id}").as_str());

        Ok(Self {
            id,
            socket_path: node_dir.join(Self::NODE_SOCK_NAME),
            command,
            process: None,
        })
    }

    pub fn compute_work_dir_path(parent_work_dir: &Path, id: &str) -> PathBuf {
        parent_work_dir.join(format!("node-{}", id)).to_path_buf()
    }

    pub fn socket_path(&self) -> &Path {
        &self.socket_path
    }

    pub fn start(&mut self) -> StdResult<()> {
        self.process = Some(self.command.start(&[])?);
        Ok(())
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> StdResult<()> {
        self.command
            .tail_logs(
                Some(format!("signature-network-node-{}", self.id).as_str()),
                number_of_line,
            )
            .await
    }
}
