use anyhow::{anyhow, Context};
use mithril_common::entities::{PartyId, TransactionHash};
use mithril_common::StdResult;
use slog_scope::info;
use std::fs::{self, read_to_string, File};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use thiserror::Error;
use tokio::process::Command;

#[derive(Error, Debug, PartialEq, Eq)]
#[error("Retryable devnet error: `{0}`")]
pub struct RetryableDevnetError(pub String);

#[derive(Debug, Clone, Default)]
pub struct Devnet {
    artifacts_dir: PathBuf,
    number_of_pool_nodes: u8,
    number_of_full_nodes: u8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PoolNode {
    pub db_path: PathBuf,
    pub socket_path: PathBuf,
    pub pool_env_path: PathBuf,
    pub kes_secret_key_path: PathBuf,
    pub operational_certificate_path: PathBuf,
}

impl PoolNode {
    pub fn party_id(&self) -> StdResult<PartyId> {
        let content = fs::read_to_string(&self.pool_env_path).with_context(|| {
            format!(
                "error while reading party_id from file '{}'",
                self.pool_env_path.display(),
            )
        })?;
        let party_id = content
            .split('=')
            .nth(1)
            .ok_or(anyhow!("could not get party_id from string '{content}'"))?;

        Ok(party_id.trim().to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FullNode {
    pub db_path: PathBuf,
    pub socket_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DevnetTopology {
    pub pool_nodes: Vec<PoolNode>,
    pub full_nodes: Vec<FullNode>,
}

#[derive(Debug, Clone)]
pub struct DevnetBootstrapArgs {
    pub devnet_scripts_dir: PathBuf,
    pub artifacts_target_dir: PathBuf,
    pub number_of_pool_nodes: u8,
    pub number_of_full_nodes: u8,
    pub cardano_slot_length: f64,
    pub cardano_epoch_length: f64,
    pub cardano_node_version: String,
    pub cardano_hard_fork_latest_era_at_epoch: u16,
    pub skip_cardano_bin_download: bool,
}

impl Devnet {
    pub async fn bootstrap(bootstrap_args: &DevnetBootstrapArgs) -> StdResult<Devnet> {
        let bootstrap_script = "devnet-mkfiles.sh";
        let bootstrap_script_path = bootstrap_args
            .devnet_scripts_dir
            .canonicalize()
            .with_context(|| {
                format!(
                    "Can't find bootstrap script '{}' in {}",
                    bootstrap_script,
                    bootstrap_args.devnet_scripts_dir.display(),
                )
            })?
            .join(bootstrap_script);

        if bootstrap_args.artifacts_target_dir.exists() {
            fs::remove_dir_all(&bootstrap_args.artifacts_target_dir)
                .with_context(|| "Previous artifacts dir removal failed")?;
        }

        let mut bootstrap_command = Command::new(&bootstrap_script_path);
        bootstrap_command.env(
            "SKIP_CARDANO_BIN_DOWNLOAD",
            bootstrap_args.skip_cardano_bin_download.to_string(),
        );
        bootstrap_command.env(
            "ARTIFACTS_DIR",
            bootstrap_args.artifacts_target_dir.to_str().unwrap(),
        );
        bootstrap_command.env(
            "NUM_FULL_NODES",
            bootstrap_args.number_of_full_nodes.to_string(),
        );
        bootstrap_command.env(
            "NUM_POOL_NODES",
            bootstrap_args.number_of_pool_nodes.to_string(),
        );
        bootstrap_command.env(
            "SLOT_LENGTH",
            bootstrap_args.cardano_slot_length.to_string(),
        );
        bootstrap_command.env(
            "EPOCH_LENGTH",
            bootstrap_args.cardano_epoch_length.to_string(),
        );
        bootstrap_command.env("CARDANO_NODE_VERSION", &bootstrap_args.cardano_node_version);
        bootstrap_command.env(
            "CARDANO_HARD_FORK_LATEST_ERA_AT_EPOCH",
            bootstrap_args.cardano_hard_fork_latest_era_at_epoch.to_string(),
        );

        bootstrap_command
            .current_dir(&bootstrap_args.devnet_scripts_dir)
            .stdout(Stdio::null())
            .kill_on_drop(true);

        info!("Bootstrapping the Devnet"; "script" => &bootstrap_script_path.display());

        let exit_status = bootstrap_command
            .spawn()
            .with_context(|| format!("{bootstrap_script} failed to start"))?
            .wait()
            .await
            .with_context(|| format!("{bootstrap_script} failed to run"))?;
        match exit_status.code() {
            Some(0) => Ok(Devnet {
                artifacts_dir: bootstrap_args.artifacts_target_dir.to_owned(),
                number_of_pool_nodes: bootstrap_args.number_of_pool_nodes,
                number_of_full_nodes: bootstrap_args.number_of_full_nodes,
            }),
            Some(code) => Err(anyhow!(RetryableDevnetError(format!(
                "Bootstrap devnet exited with status code: {code}"
            )))),
            None => Err(anyhow!("Bootstrap devnet terminated by signal")),
        }
    }

    /// Factory for test purposes
    #[cfg(test)]
    pub fn new(artifacts_dir: PathBuf, number_of_pool_nodes: u8, number_of_full_nodes: u8) -> Self {
        Self {
            artifacts_dir,
            number_of_pool_nodes,
            number_of_full_nodes,
        }
    }

    pub fn artifacts_dir(&self) -> PathBuf {
        self.artifacts_dir.clone()
    }

    pub fn mithril_era_marker_address_path(&self) -> PathBuf {
        self.artifacts_dir.join("addresses").join("mithril-era.addr")
    }

    pub fn mithril_era_marker_address(&self) -> StdResult<String> {
        let mut mithril_era_marker_address_file =
            File::open(self.mithril_era_marker_address_path())?;
        let mut mithril_era_marker_address_buffer = Vec::new();
        mithril_era_marker_address_file.read_to_end(&mut mithril_era_marker_address_buffer)?;

        String::from_utf8(mithril_era_marker_address_buffer)
            .with_context(|| "Failed to read mithril era marker address file")
    }

    pub fn mithril_payments_transaction_hashes_path(&self) -> PathBuf {
        self.artifacts_dir.join("transaction-hashes.txt")
    }

    pub fn mithril_payments_transaction_hashes(&self) -> StdResult<Vec<TransactionHash>> {
        let transaction_hashes = read_to_string(self.mithril_payments_transaction_hashes_path())?
            .lines()
            .map(String::from)
            .collect();

        Ok(transaction_hashes)
    }

    pub fn cardano_cli_path(&self) -> PathBuf {
        self.artifacts_dir.join("cardano-cli")
    }

    pub fn topology(&self) -> DevnetTopology {
        let pool_nodes = (1..=self.number_of_pool_nodes)
            .map(|n| PoolNode {
                db_path: self.artifacts_dir.join(format!("node-pool{n}/db")),
                socket_path: self.artifacts_dir.join(format!("node-pool{n}/ipc/node.sock")),
                pool_env_path: self.artifacts_dir.join(format!("node-pool{n}/pool.env")),
                kes_secret_key_path: self
                    .artifacts_dir
                    .join(format!("node-pool{n}/shelley/kes.skey")),
                operational_certificate_path: self
                    .artifacts_dir
                    .join(format!("node-pool{n}/shelley/opcert.cert")),
            })
            .collect::<Vec<_>>();
        let full_nodes = (1..=self.number_of_full_nodes)
            .map(|n| FullNode {
                db_path: self.artifacts_dir.join(format!("node-full{n}/db")),
                socket_path: self.artifacts_dir.join(format!("node-full{n}/ipc/node.sock")),
            })
            .collect::<Vec<_>>();

        DevnetTopology {
            pool_nodes,
            full_nodes,
        }
    }

    pub async fn run(&self) -> StdResult<()> {
        let run_script = "start-cardano.sh";
        let run_script_path = self.artifacts_dir.join(run_script);
        let mut run_command = Command::new(&run_script_path);
        run_command.current_dir(&self.artifacts_dir).kill_on_drop(true);

        info!("Starting the Devnet"; "script" => &run_script_path.display());

        let status = run_command
            .spawn()
            .with_context(|| "Failed to start the devnet")?
            .wait()
            .await
            .with_context(|| "Error while starting the devnet")?;
        match status.code() {
            Some(0) => Ok(()),
            Some(code) => Err(anyhow!(RetryableDevnetError(format!(
                "Run devnet exited with status code: {code}"
            )))),
            None => Err(anyhow!("Run devnet terminated by signal")),
        }
    }

    pub async fn stop(&self) -> StdResult<()> {
        let stop_script = "stop.sh";
        let stop_script_path = self.artifacts_dir.join(stop_script);
        let mut stop_command = Command::new(&stop_script_path);
        stop_command.current_dir(&self.artifacts_dir).kill_on_drop(true);

        info!("Stopping the Devnet"; "script" => &stop_script_path.display());

        let exit_status = stop_command
            .spawn()
            .with_context(|| "Failed to stop the devnet")?
            .wait()
            .await
            .with_context(|| "Error while stopping the devnet")?;
        match exit_status.code() {
            Some(0) => Ok(()),
            Some(code) => Err(anyhow!("Stop devnet exited with status code: {code}")),
            None => Err(anyhow!("Stop devnet terminated by signal")),
        }
    }

    pub async fn delegate_stakes(&self, delegation_round: u16) -> StdResult<()> {
        let run_script = "delegate.sh";
        let run_script_path = self.artifacts_dir.join(run_script);
        let mut run_command = Command::new(&run_script_path);
        run_command.current_dir(&self.artifacts_dir).kill_on_drop(true);
        run_command.env("DELEGATION_ROUND", delegation_round.to_string());

        info!("Delegating stakes to the pools"; "script" => &run_script_path.display());

        let status = run_command
            .spawn()
            .with_context(|| "Failed to delegate stakes to the pools")?
            .wait()
            .await
            .with_context(|| "Error while delegating stakes to the pools")?;
        match status.code() {
            Some(0) => Ok(()),
            Some(code) => Err(anyhow!(RetryableDevnetError(format!(
                "Delegating stakes exited with status code: {code}"
            )))),
            None => Err(anyhow!("Delegating stakes terminated by signal")),
        }
    }

    pub async fn write_era_marker(&self, target_path: &Path) -> StdResult<()> {
        let run_script = "era-mithril.sh";
        let run_script_path = self.artifacts_dir.join(run_script);
        let mut run_command = Command::new(&run_script_path);
        run_command.current_dir(&self.artifacts_dir).kill_on_drop(true);
        run_command.env("DATUM_FILE", target_path.to_str().unwrap());

        info!("Writing era marker on chain"; "script" => &run_script_path.display());

        let status = run_command
            .spawn()
            .with_context(|| "Failed to write era marker on chain")?
            .wait()
            .await
            .with_context(|| "Error while writing era marker on chain")?;
        match status.code() {
            Some(0) => Ok(()),
            Some(code) => Err(anyhow!(RetryableDevnetError(format!(
                "Write era marker on chain exited with status code: {code}"
            )))),
            None => Err(anyhow!("Write era marker on chain terminated by signal")),
        }
    }

    pub async fn transfer_funds(&self) -> StdResult<()> {
        let run_script = "payment-mithril.sh";
        let run_script_path = self.artifacts_dir.join(run_script);
        let mut run_command = Command::new(&run_script_path);
        run_command.current_dir(&self.artifacts_dir).kill_on_drop(true);
        run_command.env("PAYMENT_ITERATIONS", "5");

        info!("Transferring some funds on chain"; "script" => &run_script_path.display());

        let status = run_command
            .spawn()
            .with_context(|| "Failed to transfer funds on chain")?
            .wait()
            .await
            .with_context(|| "Error while to transferring funds on chain")?;
        match status.code() {
            Some(0) => Ok(()),
            Some(code) => Err(anyhow!(RetryableDevnetError(format!(
                "Transfer funds on chain exited with status code: {code}"
            )))),
            None => Err(anyhow!("Transfer funds on chain terminated by signal")),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::devnet::runner::{Devnet, FullNode, PoolNode};
    use crate::devnet::DevnetTopology;
    use std::path::PathBuf;

    #[test]
    pub fn yield_empty_topology_with_0_nodes() {
        let devnet = Devnet::new(PathBuf::new(), 0, 0);
        let topology = devnet.topology();

        assert_eq!(0, topology.pool_nodes.len());
        assert_eq!(0, topology.full_nodes.len());
    }

    #[test]
    pub fn yield_complete_topology_with_12_pool_nodes() {
        let devnet = Devnet::new(PathBuf::new(), 12, 0);
        let topology = devnet.topology();

        assert_eq!(12, topology.pool_nodes.len());
    }

    #[test]
    pub fn yield_complete_topology_with_12_full_nodes() {
        let devnet = Devnet::new(PathBuf::new(), 0, 12);
        let topology = devnet.topology();

        assert_eq!(12, topology.full_nodes.len());
    }

    #[test]
    pub fn topology_path_leads_to_artifacts_subfolders() {
        let devnet = Devnet::new(PathBuf::from(r"test/path/"), 1, 1);

        assert_eq!(
            DevnetTopology {
                pool_nodes: vec![PoolNode {
                    db_path: PathBuf::from(r"test/path/node-pool1/db"),
                    socket_path: PathBuf::from(r"test/path/node-pool1/ipc/node.sock"),
                    pool_env_path: PathBuf::from(r"test/path/node-pool1/pool.env"),
                    kes_secret_key_path: PathBuf::from(r"test/path/node-pool1/shelley/kes.skey"),
                    operational_certificate_path: PathBuf::from(
                        r"test/path/node-pool1/shelley/opcert.cert"
                    ),
                },],
                full_nodes: vec![FullNode {
                    db_path: PathBuf::from(r"test/path/node-full1/db"),
                    socket_path: PathBuf::from(r"test/path/node-full1/ipc/node.sock")
                }]
            },
            devnet.topology()
        );
    }
}
