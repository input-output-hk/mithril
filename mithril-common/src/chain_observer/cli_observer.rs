use async_trait::async_trait;
use nom::IResult;
use serde_json::Value;
use std::error::Error;
use std::path::PathBuf;
use tokio::process::Command;

use crate::chain_observer::interface::*;
use crate::entities::{Epoch, StakeDistribution};
use crate::CardanoNetwork;

#[async_trait]
pub trait CliRunner {
    async fn launch_stake_distribution(&self) -> Result<String, Box<dyn Error + Sync + Send>>;
    async fn launch_stake_snapshot(
        &self,
        stake_pool_id: &str,
    ) -> Result<String, Box<dyn Error + Sync + Send>>;
    async fn launch_epoch(&self) -> Result<String, Box<dyn Error + Sync + Send>>;
}

/// A runner able to request data from a Cardano node using the
/// [Cardano Cli](https://docs.cardano.org/getting-started/use-cli).
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

    fn command_for_epoch(&self) -> Command {
        let mut command = self.get_command();
        command.arg("query").arg("tip");
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
    async fn launch_stake_distribution(&self) -> Result<String, Box<dyn Error + Sync + Send>> {
        let output = self.command_for_stake_distribution().output().await?;

        if output.status.success() {
            Ok(std::str::from_utf8(&output.stdout)?.trim().to_string())
        } else {
            let message = String::from_utf8_lossy(&output.stderr);

            Err(format!(
                "Error launching command {:?}, error = '{}'",
                self.command_for_stake_distribution(),
                message
            )
            .into())
        }
    }

    async fn launch_stake_snapshot(
        &self,
        stake_pool_id: &str,
    ) -> Result<String, Box<dyn Error + Sync + Send>> {
        let output = self
            .command_for_stake_snapshot(stake_pool_id)
            .output()
            .await?;

        if output.status.success() {
            Ok(std::str::from_utf8(&output.stdout)?.trim().to_string())
        } else {
            let message = String::from_utf8_lossy(&output.stderr);

            Err(format!(
                "Error launching command {:?}, error = '{}'",
                self.command_for_stake_snapshot(stake_pool_id),
                message
            )
            .into())
        }
    }

    async fn launch_epoch(&self) -> Result<String, Box<dyn Error + Sync + Send>> {
        let output = self.command_for_epoch().output().await?;

        if output.status.success() {
            Ok(std::str::from_utf8(&output.stdout)?.trim().to_string())
        } else {
            let message = String::from_utf8_lossy(&output.stderr);

            Err(format!(
                "Error launching command {:?}, error = '{}'",
                self.command_for_epoch(),
                message
            )
            .into())
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
            .map_err(|e| {
                ChainObserverError::InvalidContent(
                    format!(
                        "Error: {:?}, output was = '{}'",
                        e, stake_pool_snapshot_output
                    )
                    .into(),
                )
            })?;
        if let Value::Number(stake_pool_stake) = &stake_pool_snapshot["poolStakeMark"] {
            return stake_pool_stake.as_u64().ok_or_else(|| {
                ChainObserverError::InvalidContent(
                    format!(
                        "Error: could not parse stake pool value as u64 {:?}",
                        stake_pool_stake
                    )
                    .into(),
                )
            });
        }
        Err(ChainObserverError::InvalidContent(
            format!(
                "Error: could not parse stake pool snapshot {:?}",
                stake_pool_snapshot
            )
            .into(),
        ))
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
        let v: Value = serde_json::from_str(&output).map_err(|e| {
            ChainObserverError::InvalidContent(
                format!("Error: {:?}, output was = '{}'", e, output).into(),
            )
        })?;

        if let Value::Number(epoch) = &v["epoch"] {
            Ok(epoch.as_u64().map(Epoch))
        } else {
            Ok(None)
        }
    }

    async fn get_current_stake_distribution(
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
                return Err(ChainObserverError::InvalidContent(
                    format!("could not parse stake from '{}'", words[1]).into(),
                ));
            }
        }

        Ok(Some(stake_distribution))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestCliRunner {}

    #[async_trait]
    impl CliRunner for TestCliRunner {
        async fn launch_stake_distribution(&self) -> Result<String, Box<dyn Error + Sync + Send>> {
            let output = r#"
                           PoolId                                 Stake frac
------------------------------------------------------------------------------
pool1qqyjr9pcrv97gwrueunug829fs5znw6p2wxft3fvqkgu5f4qlrg   2.493e-3
pool1qqfnw2fwajdnam7xsqhhrje5cgd8jcltzfrx655rd23eqlxjfef   2.164e-5
pool1qqnjh80kudcjphrxftj74x22q3a4uvw8wknlxptgs7gdqtstqad   8.068e-7
pool1qquwwu6680fr72y4779r2kpc7mxtch8rp2uhuqcc7v9p6q4f7ph   7.073e-7
pool1qpqvz90w7qsex2al2ejjej0rfgrwsguch307w8fraw7a7adf6g8   2.474e-11
pool1qptl80vq84xm28pt3t2lhpfzqag28csjhktxz5k6a74n260clmt   5.600e-7
pool1qpuckgzxwgdru9vvq3ydmuqa077ur783yn2uywz7zq2c29p506e   5.161e-5
pool1qz2vzszautc2c8mljnqre2857dpmheq7kgt6vav0s38tvvhxm6w   1.051e-6
"#;

            Ok(output.to_string())
        }

        async fn launch_stake_snapshot(
            &self,
            stake_pool_id: &str,
        ) -> Result<String, Box<dyn Error + Sync + Send>> {
            let mut output = r#"
{
    "poolStakeGo": 1000000,
    "poolStakeSet": 2000000,
    "poolStakeMark": 3000000,
    "activeStakeGo": 5000000,
    "activeStakeSet": 7000000,
    "activeStakeMark": 9000000
}
"#;
            if stake_pool_id == "pool1qpqvz90w7qsex2al2ejjej0rfgrwsguch307w8fraw7a7adf6g8" {
                output = r#"
                {
                    "poolStakeGo": 0,
                    "poolStakeSet": 0,
                    "poolStakeMark": 0,
                    "activeStakeGo": 5000000,
                    "activeStakeSet": 7000000,
                    "activeStakeMark": 9000000
                }
                "#
            }

            Ok(output.to_string())
        }

        async fn launch_epoch(&self) -> Result<String, Box<dyn Error + Sync + Send>> {
            let output = r#"
{
    "era": "Alonzo",
    "syncProgress": "100.00",
    "hash": "f6d1b8c328697c7a4a8e7f718c79510acbcd411ff4ca19401ded13534d45a38d",
    "epoch": 120,
    "slot": 0,
    "block": 0
}"#;

            Ok(output.to_string())
        }
    }

    #[tokio::test]
    async fn test_get_current_epoch() {
        let observer = CardanoCliChainObserver::new(Box::new(TestCliRunner {}));
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

        assert_eq!("Command { std: \"cardano-cli\" \"query\" \"tip\" \"--testnet-magic\" \"10\", kill_on_drop: false }", format!("{:?}", runner.command_for_epoch()));
        assert_eq!("Command { std: \"cardano-cli\" \"query\" \"stake-distribution\" \"--testnet-magic\" \"10\", kill_on_drop: false }", format!("{:?}", runner.command_for_stake_distribution()));
    }

    #[tokio::test]
    async fn test_cli_devnet_runner() {
        let runner = CardanoCliRunner::new(
            PathBuf::new().join("cardano-cli"),
            PathBuf::new().join("/tmp/whatever.sock"),
            CardanoNetwork::DevNet(25),
        );

        assert_eq!("Command { std: \"cardano-cli\" \"query\" \"tip\" \"--cardano-mode\" \"--testnet-magic\" \"25\", kill_on_drop: false }", format!("{:?}", runner.command_for_epoch()));
        assert_eq!("Command { std: \"cardano-cli\" \"query\" \"stake-distribution\" \"--cardano-mode\" \"--testnet-magic\" \"25\", kill_on_drop: false }", format!("{:?}", runner.command_for_stake_distribution()));
    }

    #[tokio::test]
    async fn test_cli_mainnet_runner() {
        let runner = CardanoCliRunner::new(
            PathBuf::new().join("cardano-cli"),
            PathBuf::new().join("/tmp/whatever.sock"),
            CardanoNetwork::MainNet,
        );

        assert_eq!(
            "Command { std: \"cardano-cli\" \"query\" \"tip\" \"--mainnet\", kill_on_drop: false }",
            format!("{:?}", runner.command_for_epoch())
        );
        assert_eq!(
            "Command { std: \"cardano-cli\" \"query\" \"stake-distribution\" \"--mainnet\", kill_on_drop: false }",
            format!("{:?}", runner.command_for_stake_distribution())
        );
    }

    #[tokio::test]
    async fn test_get_current_stake_value() {
        let observer = CardanoCliChainObserver::new(Box::new(TestCliRunner {}));
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
    async fn test_get_current_stake_distribution() {
        let observer = CardanoCliChainObserver::new(Box::new(TestCliRunner {}));
        let results = observer
            .get_current_stake_distribution()
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
}
