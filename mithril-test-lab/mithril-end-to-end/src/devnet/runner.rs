use slog_scope::info;
use std::fs;
use std::path::PathBuf;
use std::process::Stdio;
use tokio::process::Command;

#[derive(Debug, Clone)]
pub struct Devnet {
    artifacts_dir: PathBuf,
    number_of_bft_nodes: u8,
    number_of_pool_nodes: u8,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BftNode {
    pub db_path: PathBuf,
    pub socket_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PoolNode {
    pub db_path: PathBuf,
    pub socket_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DevnetTopology {
    pub bft_nodes: Vec<BftNode>,
    pub pool_nodes: Vec<PoolNode>,
}

impl Devnet {
    pub async fn bootstrap(
        devnet_scripts_dir: PathBuf,
        artifacts_target_dir: PathBuf,
        number_of_bft_nodes: u8,
        number_of_pool_nodes: u8,
    ) -> Result<Devnet, String> {
        let bootstrap_script = "devnet-mkfiles.sh";
        let bootstrap_script_path = devnet_scripts_dir
            .canonicalize()
            .map_err(|e| {
                format!(
                    "Can't find bootstrap script '{}' in {}: {}",
                    bootstrap_script,
                    devnet_scripts_dir.display(),
                    e
                )
            })?
            .join(bootstrap_script);

        if artifacts_target_dir.exists() {
            fs::remove_dir_all(&artifacts_target_dir)
                .map_err(|e| format!("Previous artifacts dir removal failed: {}", e))?;
        }

        let mut bootstrap_command = Command::new(&bootstrap_script_path);
        let command_args = &[
            artifacts_target_dir.to_str().unwrap(),
            &number_of_bft_nodes.to_string(),
            &number_of_pool_nodes.to_string(),
        ];
        bootstrap_command
            .current_dir(devnet_scripts_dir)
            .args(command_args)
            .stdout(Stdio::null())
            .kill_on_drop(true);

        info!("Bootstrapping the Devnet"; "script" => &bootstrap_script_path.display(), "args" => #?&command_args);

        bootstrap_command
            .spawn()
            .map_err(|e| format!("{} failed to start: {}", bootstrap_script, e))?
            .wait()
            .await
            .map_err(|e| format!("{} failed to run: {}", bootstrap_script, e))?;

        Ok(Devnet {
            artifacts_dir: artifacts_target_dir,
            number_of_bft_nodes,
            number_of_pool_nodes,
        })
    }

    /// Factory for test purposes
    #[cfg(test)]
    pub fn new(artifacts_dir: PathBuf, number_of_bft_nodes: u8, number_of_pool_nodes: u8) -> Self {
        Self {
            artifacts_dir,
            number_of_bft_nodes,
            number_of_pool_nodes,
        }
    }

    pub fn cardano_cli_path(&self) -> PathBuf {
        self.artifacts_dir.join("cardano-cli")
    }

    pub fn topology(&self) -> DevnetTopology {
        let bft_nodes = (1..=self.number_of_bft_nodes)
            .into_iter()
            .map(|n| BftNode {
                db_path: self.artifacts_dir.join(format!("node-bft{}/db", n)),
                socket_path: self
                    .artifacts_dir
                    .join(format!("node-bft{}/ipc/node.sock", n)),
            })
            .collect::<Vec<_>>();
        let pool_nodes = (1..=self.number_of_pool_nodes)
            .into_iter()
            .map(|n| PoolNode {
                db_path: self.artifacts_dir.join(format!("node-pool{}/db", n)),
                socket_path: self
                    .artifacts_dir
                    .join(format!("node-pool{}/ipc/node.sock", n)),
            })
            .collect::<Vec<_>>();

        DevnetTopology {
            bft_nodes,
            pool_nodes,
        }
    }

    pub async fn run(&self) -> Result<(), String> {
        let run_script = "start-cardano.sh";
        let run_script_path = self.artifacts_dir.join(run_script);
        let mut run_command = Command::new(&run_script_path);
        run_command
            .current_dir(&self.artifacts_dir)
            .kill_on_drop(true);

        info!("Starting the Devnet"; "script" => &run_script_path.display());

        run_command
            .spawn()
            .map_err(|e| format!("Failed to start the devnet: {}", e))?
            .wait()
            .await
            .map_err(|e| format!("Error while starting the devnet: {}", e))?;
        Ok(())
    }

    pub async fn stop(&self) -> Result<(), String> {
        let stop_script = "stop.sh";
        let stop_script_path = self.artifacts_dir.join(stop_script);
        let mut stop_command = Command::new(&stop_script_path);
        stop_command
            .current_dir(&self.artifacts_dir)
            .kill_on_drop(true);

        info!("Stopping the Devnet"; "script" => &stop_script_path.display());

        stop_command
            .spawn()
            .map_err(|e| format!("Failed to stop the devnet: {}", e))?
            .wait()
            .await
            .map_err(|e| format!("Error while stopping the devnet: {}", e))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::devnet::runner::{BftNode, Devnet, PoolNode};
    use crate::devnet::DevnetTopology;
    use std::path::PathBuf;

    #[test]
    pub fn yield_empty_topology_with_0_nodes() {
        let devnet = Devnet::new(PathBuf::new(), 0, 0);
        let topology = devnet.topology();

        assert_eq!(
            (0, 0),
            (topology.bft_nodes.len(), topology.pool_nodes.len())
        );
    }

    #[test]
    pub fn yield_complete_topology_with_2_bft_and_12_pool_nodes() {
        let devnet = Devnet::new(PathBuf::new(), 2, 12);
        let topology = devnet.topology();

        assert_eq!(
            (2, 12),
            (topology.bft_nodes.len(), topology.pool_nodes.len())
        );
    }

    #[test]
    pub fn topology_path_leads_to_artifacts_subfolders() {
        let devnet = Devnet::new(PathBuf::from(r"test/path/"), 1, 1);

        assert_eq!(
            DevnetTopology {
                bft_nodes: vec![BftNode {
                    db_path: PathBuf::from(r"test/path/node-bft1/db"),
                    socket_path: PathBuf::from(r"test/path/node-bft1/ipc/node.sock")
                }],
                pool_nodes: vec![PoolNode {
                    db_path: PathBuf::from(r"test/path/node-pool1/db"),
                    socket_path: PathBuf::from(r"test/path/node-pool1/ipc/node.sock")
                },],
            },
            devnet.topology()
        );
    }
}
