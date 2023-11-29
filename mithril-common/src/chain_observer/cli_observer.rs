use anyhow::{anyhow, Context};
use async_trait::async_trait;
use bech32::{self, ToBase32, Variant};
use hex::FromHex;
use nom::IResult;
use rand_core::RngCore;
use serde_json::Value;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use tokio::process::Command;

use crate::chain_observer::interface::{ChainObserver, ChainObserverError};
use crate::chain_observer::{ChainAddress, TxDatum};
use crate::crypto_helper::{KESPeriod, OpCert, SerDeShelleyFileFormat};
use crate::entities::{Epoch, StakeDistribution};
use crate::{CardanoNetwork, StdResult};

/// `CliRunner` trait defines the asynchronous methods
/// for interaction with the Cardano CLI.
#[async_trait]
pub trait CliRunner {
    /// Launches a UTxO.
    async fn launch_utxo(&self, address: &str) -> StdResult<String>;
    /// Launches the stake distribution.
    async fn launch_stake_distribution(&self) -> StdResult<String>;
    /// Launches the stake snapshot.
    async fn launch_stake_snapshot(&self, stake_pool_id: &str) -> StdResult<String>;
    /// Launches the stake snapshot for all pools.
    async fn launch_stake_snapshot_all_pools(&self) -> StdResult<String>;
    /// Launches the epoch info.
    async fn launch_epoch(&self) -> StdResult<String>;
    /// Launches the kes period.
    async fn launch_kes_period(&self, opcert_file: &str) -> StdResult<String>;
}

/// A runner able to request data from a Cardano node using the
/// [Cardano Cli](https://docs.cardano.org/getting-started/use-cli).
#[derive(Clone, Debug)]
pub struct CardanoCliRunner {
    cli_path: PathBuf,
    socket_path: PathBuf,
    network: CardanoNetwork,
}

impl CardanoCliRunner {
    /// CardanoCliRunner factory
    pub fn new(cli_path: PathBuf, socket_path: PathBuf, network: CardanoNetwork) -> Self {
        Self {
            cli_path,
            socket_path,
            network,
        }
    }

    fn random_out_file() -> StdResult<PathBuf> {
        let mut rng = rand_core::OsRng;
        let dir = std::env::temp_dir().join("cardano-cli-runner");
        if !dir.exists() {
            fs::create_dir_all(&dir)?;
        }
        Ok(dir.join(format!("{}.out", rng.next_u64())))
    }

    fn command_for_utxo(&self, address: &str, out_file: PathBuf) -> Command {
        let mut command = self.get_command();
        command
            .arg("query")
            .arg("utxo")
            .arg("--address")
            .arg(address)
            .arg("--out-file")
            .arg(out_file);
        self.post_config_command(&mut command);

        command
    }

    fn command_for_stake_distribution(&self) -> Command {
        let mut command = self.get_command();
        command.arg("query").arg("stake-distribution");
        self.post_config_command(&mut command);

        command
    }

    fn command_for_stake_snapshot(&self, stake_pool_id: &str) -> Command {
        let mut command = self.get_command();
        command
            .arg("query")
            .arg("stake-snapshot")
            .arg("--stake-pool-id")
            .arg(stake_pool_id);
        self.post_config_command(&mut command);

        command
    }

    fn command_for_stake_snapshot_all_pools(&self) -> Command {
        let mut command = self.get_command();
        command
            .arg("query")
            .arg("stake-snapshot")
            .arg("--all-stake-pools");
        self.post_config_command(&mut command);

        command
    }

    fn command_for_epoch(&self) -> Command {
        let mut command = self.get_command();
        command.arg("query").arg("tip");
        self.post_config_command(&mut command);

        command
    }

    fn command_for_kes_period(&self, opcert_file: &str) -> Command {
        let mut command = self.get_command();
        command
            .arg("query")
            .arg("kes-period-info")
            .arg("--op-cert-file")
            .arg(opcert_file);
        self.post_config_command(&mut command);

        command
    }

    fn get_command(&self) -> Command {
        let mut command = Command::new(&self.cli_path);
        command.env(
            "CARDANO_NODE_SOCKET_PATH",
            self.socket_path.to_string_lossy().as_ref(),
        );

        command
    }

    fn post_config_command<'a>(&'a self, command: &'a mut Command) -> &mut Command {
        match self.network {
            CardanoNetwork::MainNet => command.arg("--mainnet"),
            CardanoNetwork::DevNet(magic) => command.args(vec![
                "--cardano-mode",
                "--testnet-magic",
                &magic.to_string(),
            ]),
            CardanoNetwork::TestNet(magic) => {
                command.args(vec!["--testnet-magic", &magic.to_string()])
            }
        }
    }
}

#[async_trait]
impl CliRunner for CardanoCliRunner {
    async fn launch_utxo(&self, address: &str) -> StdResult<String> {
        let out_file = Self::random_out_file()?;
        let output = self
            .command_for_utxo(address, out_file.clone())
            .output()
            .await?;

        if output.status.success() {
            Ok(fs::read_to_string(out_file)?.trim().to_string())
        } else {
            let message = String::from_utf8_lossy(&output.stderr);

            Err(anyhow!(
                "Error launching command {:?}, error = '{}'",
                self.command_for_utxo(address, out_file),
                message
            ))
        }
    }

    async fn launch_stake_distribution(&self) -> StdResult<String> {
        let output = self.command_for_stake_distribution().output().await?;

        if output.status.success() {
            Ok(std::str::from_utf8(&output.stdout)?.trim().to_string())
        } else {
            let message = String::from_utf8_lossy(&output.stderr);

            Err(anyhow!(
                "Error launching command {:?}, error = '{}'",
                self.command_for_stake_distribution(),
                message
            ))
        }
    }

    async fn launch_stake_snapshot(&self, stake_pool_id: &str) -> StdResult<String> {
        let output = self
            .command_for_stake_snapshot(stake_pool_id)
            .output()
            .await?;

        if output.status.success() {
            Ok(std::str::from_utf8(&output.stdout)?.trim().to_string())
        } else {
            let message = String::from_utf8_lossy(&output.stderr);

            Err(anyhow!(
                "Error launching command {:?}, error = '{}'",
                self.command_for_stake_snapshot(stake_pool_id),
                message
            ))
        }
    }

    async fn launch_stake_snapshot_all_pools(&self) -> StdResult<String> {
        let output = self.command_for_stake_snapshot_all_pools().output().await?;

        if output.status.success() {
            Ok(std::str::from_utf8(&output.stdout)?.trim().to_string())
        } else {
            let message = String::from_utf8_lossy(&output.stderr);

            Err(anyhow!(
                "Error launching command {:?}, error = '{}'",
                self.command_for_stake_snapshot_all_pools(),
                message
            ))
        }
    }

    async fn launch_epoch(&self) -> StdResult<String> {
        let output = self.command_for_epoch().output().await?;

        if output.status.success() {
            Ok(std::str::from_utf8(&output.stdout)?.trim().to_string())
        } else {
            let message = String::from_utf8_lossy(&output.stderr);

            Err(anyhow!(
                "Error launching command {:?}, error = '{}'",
                self.command_for_epoch(),
                message
            ))
        }
    }

    async fn launch_kes_period(&self, opcert_file: &str) -> StdResult<String> {
        let output = self.command_for_kes_period(opcert_file).output().await?;

        if output.status.success() {
            Ok(std::str::from_utf8(&output.stdout)?.trim().to_string())
        } else {
            let message = String::from_utf8_lossy(&output.stderr);

            Err(anyhow!(
                "Error launching command {:?}, error = '{}'",
                self.command_for_kes_period(opcert_file),
                message
            ))
        }
    }
}

/// A [ChainObserver] pulling it's data using a [CardanoCliRunner].
pub struct CardanoCliChainObserver {
    cli_runner: Box<dyn CliRunner + Send + Sync>,
}

impl CardanoCliChainObserver {
    /// CardanoCliChainObserver factory
    pub fn new(cli_runner: Box<dyn CliRunner + Send + Sync>) -> Self {
        Self { cli_runner }
    }

    // This is the only way I found to tell the compiler the correct types
    // and lifetimes for the function `double`.
    fn parse_string<'a>(&'a self, string: &'a str) -> IResult<&str, f64> {
        nom::number::complete::double(string)
    }

    async fn get_current_stake_value(
        &self,
        stake_pool_id: &str,
    ) -> Result<u64, ChainObserverError> {
        let stake_pool_snapshot_output = self
            .cli_runner
            .launch_stake_snapshot(stake_pool_id)
            .await
            .map_err(ChainObserverError::General)?;
        let stake_pool_snapshot: Value = serde_json::from_str(&stake_pool_snapshot_output)
            .with_context(|| format!("output was = '{stake_pool_snapshot_output}'"))
            .map_err(ChainObserverError::InvalidContent)?;
        if let Value::Number(stake_pool_stake) = &stake_pool_snapshot["poolStakeMark"] {
            return stake_pool_stake.as_u64().ok_or_else(|| {
                ChainObserverError::InvalidContent(anyhow!(
                    "Error: could not parse stake pool value as u64 {stake_pool_stake:?}"
                ))
            });
        }
        Err(ChainObserverError::InvalidContent(anyhow!(
            "Error: could not parse stake pool snapshot {stake_pool_snapshot:?}"
        )))
    }

    // This is the legacy way of computing stake distribution, not optimized for mainnet, and usable for versions of the Cardano node up to '1.35.7'
    async fn get_current_stake_distribution_legacy(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError> {
        let output = self
            .cli_runner
            .launch_stake_distribution()
            .await
            .map_err(ChainObserverError::General)?;
        let mut stake_distribution = StakeDistribution::new();

        for (num, line) in output.lines().enumerate() {
            let words: Vec<&str> = line.split_ascii_whitespace().collect();

            if num < 2 || words.len() != 2 {
                continue;
            }

            let stake_pool_id = words[0];
            let stake_fraction = words[1];

            if let Ok((_, _f)) = self.parse_string(stake_fraction) {
                // This block is a fix:
                // the stake retrieved was computed on the current epoch, when we need a value computed on the previous epoch
                // in 'let stake: u64 = (f * 1_000_000_000.0).round() as u64;'
                let stake: u64 = self.get_current_stake_value(stake_pool_id).await?;

                if stake > 0 {
                    let _ = stake_distribution.insert(stake_pool_id.to_string(), stake);
                }
            } else {
                return Err(ChainObserverError::InvalidContent(anyhow!(
                    "could not parse stake from '{}'",
                    words[1]
                )));
            }
        }

        Ok(Some(stake_distribution))
    }

    // This is the new way of computing stake distribution, optimized for mainnet, and usable for versions of the Cardano node from '8.0.0'
    async fn get_current_stake_distribution_optimized(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError> {
        let output = self
            .cli_runner
            .launch_stake_snapshot_all_pools()
            .await
            .map_err(ChainObserverError::General)?;
        let mut stake_distribution = StakeDistribution::new();

        let data: HashMap<String, Value> =
            serde_json::from_str(&output).map_err(|e| ChainObserverError::General(e.into()))?;
        let pools_data = data
            .get("pools")
            .ok_or(ChainObserverError::InvalidContent(anyhow!(
                "Missing 'pools' field"
            )))?
            .as_object()
            .ok_or(ChainObserverError::InvalidContent(anyhow!(
                "Could not convert pool data to object"
            )))?;

        for (k, v) in pools_data.iter() {
            let pool_id_hex = k;
            let pool_id_bech32 = bech32::encode(
                "pool",
                Vec::from_hex(pool_id_hex.as_bytes())
                    .map_err(|e| ChainObserverError::General(e.into()))?
                    .to_base32(),
                Variant::Bech32,
            )
            .map_err(|e| ChainObserverError::General(e.into()))?;
            let stakes = v
                .get("stakeMark")
                .ok_or(ChainObserverError::InvalidContent(anyhow!(
                    "Missing 'stakeMark' field for {pool_id_bech32}"
                )))?
                .as_u64()
                .ok_or(ChainObserverError::InvalidContent(anyhow!(
                    "Stake could not be converted to integer for {pool_id_bech32}"
                )))?;
            if stakes > 0 {
                stake_distribution.insert(pool_id_bech32, stakes);
            }
        }

        Ok(Some(stake_distribution))
    }
}

#[async_trait]
impl ChainObserver for CardanoCliChainObserver {
    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
        let output = self
            .cli_runner
            .launch_epoch()
            .await
            .map_err(ChainObserverError::General)?;
        let v: Value = serde_json::from_str(&output)
            .with_context(|| format!("output was = '{output}'"))
            .map_err(ChainObserverError::InvalidContent)?;

        if let Value::Number(epoch) = &v["epoch"] {
            Ok(epoch.as_u64().map(Epoch))
        } else {
            Ok(None)
        }
    }

    async fn get_current_datums(
        &self,
        address: &ChainAddress,
    ) -> Result<Vec<TxDatum>, ChainObserverError> {
        let output = self
            .cli_runner
            .launch_utxo(address)
            .await
            .map_err(ChainObserverError::General)?;
        let v: HashMap<String, Value> = serde_json::from_str(&output)
            .with_context(|| format!("output was = '{output}'"))
            .map_err(ChainObserverError::InvalidContent)?;

        Ok(v.values()
            .filter_map(|v| {
                v.get("inlineDatum")
                    .filter(|datum| !datum.is_null())
                    .map(|datum| TxDatum(datum.to_string()))
            })
            .collect())
    }

    // TODO: This function implements a fallback mechanism to compute the stake distribution: new/optimized computation when available, legacy computation otherwise
    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError> {
        match self.get_current_stake_distribution_optimized().await {
            Ok(stake_distribution_maybe) => Ok(stake_distribution_maybe),
            Err(_) => self.get_current_stake_distribution_legacy().await,
        }
    }

    async fn get_current_kes_period(
        &self,
        opcert: &OpCert,
    ) -> Result<Option<KESPeriod>, ChainObserverError> {
        let dir = std::env::temp_dir().join("mithril_kes_period");
        fs::create_dir_all(&dir).map_err(|e| ChainObserverError::General(e.into()))?;
        let opcert_file =
            std::env::temp_dir().join(format!("opcert_kes_period-{}", opcert.compute_hash()));
        opcert
            .to_file(&opcert_file)
            .map_err(|e| ChainObserverError::General(e.into()))?;
        let output = self
            .cli_runner
            .launch_kes_period(opcert_file.to_str().unwrap())
            .await
            .map_err(ChainObserverError::General)?;
        let first_left_curly_bracket_index = output.find('{').unwrap_or_default();
        let output_cleaned = output.split_at(first_left_curly_bracket_index).1;
        let v: Value = serde_json::from_str(output_cleaned)
            .with_context(|| format!("output was = '{output}'"))
            .map_err(ChainObserverError::InvalidContent)?;

        if let Value::Number(kes_period) = &v["qKesCurrentKesPeriod"] {
            Ok(kes_period.as_u64().map(|p| p as KESPeriod))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;
    use crate::{chain_observer::test_cli_runner::TestCliRunner, crypto_helper::ColdKeyGenerator};

    use kes_summed_ed25519::{kes::Sum6Kes, traits::KesSk};

    #[tokio::test]
    async fn test_get_current_epoch() {
        let observer = CardanoCliChainObserver::new(Box::<TestCliRunner>::default());
        let epoch = observer.get_current_epoch().await.unwrap().unwrap();

        assert_eq!(Epoch(120), epoch);
    }

    #[tokio::test]
    async fn test_cli_testnet_runner() {
        let runner = CardanoCliRunner::new(
            PathBuf::new().join("cardano-cli"),
            PathBuf::new().join("/tmp/whatever.sock"),
            CardanoNetwork::TestNet(10),
        );

        assert_eq!("Command { std: CARDANO_NODE_SOCKET_PATH=\"/tmp/whatever.sock\" \"cardano-cli\" \"query\" \"tip\" \"--testnet-magic\" \"10\", kill_on_drop: false }", format!("{:?}", runner.command_for_epoch()));
        assert_eq!("Command { std: CARDANO_NODE_SOCKET_PATH=\"/tmp/whatever.sock\" \"cardano-cli\" \"query\" \"stake-distribution\" \"--testnet-magic\" \"10\", kill_on_drop: false }", format!("{:?}", runner.command_for_stake_distribution()));
    }

    #[tokio::test]
    async fn test_cli_devnet_runner() {
        let runner = CardanoCliRunner::new(
            PathBuf::new().join("cardano-cli"),
            PathBuf::new().join("/tmp/whatever.sock"),
            CardanoNetwork::DevNet(25),
        );

        assert_eq!("Command { std: CARDANO_NODE_SOCKET_PATH=\"/tmp/whatever.sock\" \"cardano-cli\" \"query\" \"tip\" \"--cardano-mode\" \"--testnet-magic\" \"25\", kill_on_drop: false }", format!("{:?}", runner.command_for_epoch()));
        assert_eq!("Command { std: CARDANO_NODE_SOCKET_PATH=\"/tmp/whatever.sock\" \"cardano-cli\" \"query\" \"stake-distribution\" \"--cardano-mode\" \"--testnet-magic\" \"25\", kill_on_drop: false }", format!("{:?}", runner.command_for_stake_distribution()));
    }

    #[tokio::test]
    async fn test_cli_mainnet_runner() {
        let runner = CardanoCliRunner::new(
            PathBuf::new().join("cardano-cli"),
            PathBuf::new().join("/tmp/whatever.sock"),
            CardanoNetwork::MainNet,
        );

        assert_eq!(
            "Command { std: CARDANO_NODE_SOCKET_PATH=\"/tmp/whatever.sock\" \"cardano-cli\" \"query\" \"tip\" \"--mainnet\", kill_on_drop: false }",
            format!("{:?}", runner.command_for_epoch())
        );
        assert_eq!(
            "Command { std: CARDANO_NODE_SOCKET_PATH=\"/tmp/whatever.sock\" \"cardano-cli\" \"query\" \"stake-distribution\" \"--mainnet\", kill_on_drop: false }",
            format!("{:?}", runner.command_for_stake_distribution())
        );
    }

    #[tokio::test]
    async fn test_get_current_datums() {
        let observer = CardanoCliChainObserver::new(Box::<TestCliRunner>::default());
        let address = "addrtest_123456".to_string();
        let datums = observer.get_current_datums(&address).await.unwrap();
        assert_eq!(vec![TxDatum(r#"{"constructor":0,"fields":[{"bytes":"5b0a20207b0a20202020226e616d65223a20227468616c6573222c0a202020202265706f6368223a203132330a20207d2c0a20207b0a20202020226e616d65223a20227079746861676f726173222c0a202020202265706f6368223a206e756c6c0a20207d0a5d0a"}]}"#.to_string())], datums);
    }

    #[tokio::test]
    async fn test_get_current_stake_value() {
        let observer = CardanoCliChainObserver::new(Box::<TestCliRunner>::default());
        let stake = observer
            .get_current_stake_value("pool1qqyjr9pcrv97gwrueunug829fs5znw6p2wxft3fvqkgu5f4qlrg")
            .await
            .expect("get current stake value should not fail");
        assert_eq!(3_000_000, stake);

        let stake = observer
            .get_current_stake_value("pool1qpqvz90w7qsex2al2ejjej0rfgrwsguch307w8fraw7a7adf6g8")
            .await
            .expect("get current stake value should not fail");
        assert_eq!(0, stake);
    }

    #[tokio::test]
    async fn test_get_current_stake_distribution_legacy() {
        let observer = CardanoCliChainObserver::new(Box::new(TestCliRunner::legacy()));
        let results = observer
            .get_current_stake_distribution_legacy()
            .await
            .unwrap()
            .unwrap();
        assert_eq!(7, results.len());
        assert_eq!(
            3_000_000,
            *results
                .get("pool1qqyjr9pcrv97gwrueunug829fs5znw6p2wxft3fvqkgu5f4qlrg")
                .unwrap()
        );
        assert_eq!(
            3_000_000,
            *results
                .get("pool1qz2vzszautc2c8mljnqre2857dpmheq7kgt6vav0s38tvvhxm6w")
                .unwrap()
        );
        assert!(results
            .get("pool1qpqvz90w7qsex2al2ejjej0rfgrwsguch307w8fraw7a7adf6g8")
            .is_none());
    }

    #[tokio::test]
    async fn test_get_current_stake_distribution_new() {
        let observer = CardanoCliChainObserver::new(Box::<TestCliRunner>::default());
        let computed_stake_distribution = observer
            .get_current_stake_distribution_optimized()
            .await
            .unwrap()
            .unwrap();
        let mut expected_stake_distribution = StakeDistribution::new();
        expected_stake_distribution.insert(
            "pool1qqqqqdk4zhsjuxxd8jyvwncf5eucfskz0xjjj64fdmlgj735lr9".to_string(),
            300000000001,
        );
        expected_stake_distribution.insert(
            "pool1qqqqpanw9zc0rzh0yp247nzf2s35uvnsm7aaesfl2nnejaev0uc".to_string(),
            600000000001,
        );
        expected_stake_distribution.insert(
            "pool1qqqqzyqf8mlm70883zht60n4q6uqxg4a8x266sewv8ad2grkztl".to_string(),
            1200000000001,
        );
        assert_eq!(
            BTreeMap::from_iter(
                expected_stake_distribution
                    .into_iter()
                    .collect::<Vec<(_, _)>>()
                    .into_iter(),
            ),
            BTreeMap::from_iter(
                computed_stake_distribution
                    .into_iter()
                    .collect::<Vec<(_, _)>>()
                    .into_iter(),
            ),
        );
    }

    #[tokio::test]
    async fn test_get_current_stake_distribution() {
        let observer = CardanoCliChainObserver::new(Box::new(TestCliRunner::legacy()));
        let expected_stake_distribution = observer
            .get_current_stake_distribution_legacy()
            .await
            .unwrap()
            .unwrap();
        let computed_stake_distribution = observer
            .get_current_stake_distribution()
            .await
            .unwrap()
            .unwrap();

        assert_eq!(
            BTreeMap::from_iter(
                expected_stake_distribution
                    .clone()
                    .into_iter()
                    .collect::<Vec<(_, _)>>()
                    .into_iter(),
            ),
            BTreeMap::from_iter(
                computed_stake_distribution
                    .into_iter()
                    .collect::<Vec<(_, _)>>()
                    .into_iter(),
            ),
        );

        let observer = CardanoCliChainObserver::new(Box::<TestCliRunner>::default());
        let expected_stake_distribution = observer
            .get_current_stake_distribution_optimized()
            .await
            .unwrap()
            .unwrap();
        let computed_stake_distribution = observer
            .get_current_stake_distribution()
            .await
            .unwrap()
            .unwrap();

        assert_eq!(
            BTreeMap::from_iter(
                expected_stake_distribution
                    .into_iter()
                    .collect::<Vec<(_, _)>>()
                    .into_iter(),
            ),
            BTreeMap::from_iter(
                computed_stake_distribution
                    .into_iter()
                    .collect::<Vec<(_, _)>>()
                    .into_iter(),
            ),
        );
    }

    #[tokio::test]
    async fn test_get_current_kes_period() {
        let keypair = ColdKeyGenerator::create_deterministic_keypair([0u8; 32]);
        let mut dummy_key_buffer = [0u8; Sum6Kes::SIZE + 4];
        let mut dummy_seed = [0u8; 32];
        let (_, kes_verification_key) = Sum6Kes::keygen(&mut dummy_key_buffer, &mut dummy_seed);
        let operational_certificate = OpCert::new(kes_verification_key, 0, 0, keypair);
        let observer = CardanoCliChainObserver::new(Box::<TestCliRunner>::default());
        let kes_period = observer
            .get_current_kes_period(&operational_certificate)
            .await
            .unwrap()
            .unwrap();
        assert_eq!(404, kes_period);
    }
}
