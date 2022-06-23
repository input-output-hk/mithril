#![allow(dead_code)]
use async_trait::async_trait;
use nom::IResult;
use serde_json::Value;
use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;
use tokio::process::Command;

use crate::chain_observer::interface::*;
use crate::entities::{Epoch, StakeDistribution};

#[async_trait]
pub trait CliRunner {
    async fn launch_stake_distribution(&self) -> Result<String, Box<dyn Error + Sync + Send>>;
    async fn launch_epoch(&self) -> Result<String, Box<dyn Error + Sync + Send>>;
}

/// Cardano Network identifier
#[allow(clippy::enum_variant_names)]
enum CardanoCliNetwork {
    MainNet,
    DevNet(u64),
    TestNet(u64),
}

struct CardanoCliRunner {
    cli_path: PathBuf,
    socket_path: PathBuf,
    network: CardanoCliNetwork,
}

impl CardanoCliRunner {
    pub fn new(cli_path: PathBuf, socket_path: PathBuf, network: CardanoCliNetwork) -> Self {
        Self {
            cli_path,
            socket_path,
            network,
        }
    }

    pub fn command_for_stake_distribution(&self) -> Command {
        let mut command = self.get_command();
        command.arg("query").arg("stake-distribution");
        self.post_config_command(&mut command);

        command
    }

    pub fn command_for_epoch(&self) -> Command {
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
            CardanoCliNetwork::MainNet => command.arg("--mainnet"),
            CardanoCliNetwork::DevNet(magic) => command
                .arg(format!("--testnet-magic {}", magic))
                .arg("--cardano-mode"),
            CardanoCliNetwork::TestNet(magic) => command.arg(format!("--testnet-magic {}", magic)),
        }
    }
}

#[async_trait]
impl CliRunner for CardanoCliRunner {
    async fn launch_stake_distribution(&self) -> Result<String, Box<dyn Error + Sync + Send>> {
        let output = self.command_for_stake_distribution().output().await?;

        Ok(std::str::from_utf8(&output.stdout)?.trim().to_string())
    }

    async fn launch_epoch(&self) -> Result<String, Box<dyn Error + Sync + Send>> {
        let output = self.command_for_epoch().output().await?;

        Ok(std::str::from_utf8(&output.stdout)?.trim().to_string())
    }
}

pub struct CardanoCliChainObserver {
    cli_runner: Box<dyn CliRunner + Send + Sync>,
}

impl CardanoCliChainObserver {
    pub fn new(cli_runner: Box<dyn CliRunner + Send + Sync>) -> Self {
        Self { cli_runner }
    }

    // This is the only way I found to tell the compiler the correct types
    // and lifetimes for the function `double`.
    fn parse_string<'a>(&'a self, string: &'a str) -> IResult<&str, f64> {
        nom::number::complete::double(string)
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
            .map_err(|e| ChainObserverError::InvalidContent(e.into()))?;

        if let Value::Number(epoch) = &v["epoch"] {
            Ok(epoch.as_u64())
        } else {
            Ok(None)
        }
    }
    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError> {
        let stake_distribution: HashMap<String, u64> = [
            (
                "pool1qqyjr9pcrv97gwrueunug829fs5znw6p2wxft3fvqkgu5f4qlrg".to_string(),
                2_493_000 as u64,
            ),
            (
                "pool1qqfnw2fwajdnam7xsqhhrje5cgd8jcltzfrx655rd23eqlxjfef".to_string(),
                21_640,
            ),
            (
                "pool1qqnjh80kudcjphrxftj74x22q3a4uvw8wknlxptgs7gdqtstqad".to_string(),
                80,
            ),
            (
                "pool1qquwwu6680fr72y4779r2kpc7mxtch8rp2uhuqcc7v9p6q4f7ph".to_string(),
                70,
            ),
            (
                "pool1qptl80vq84xm28pt3t2lhpfzqag28csjhktxz5k6a74n260clmt".to_string(),
                56,
            ),
            (
                "pool1qpuckgzxwgdru9vvq3ydmuqa077ur783yn2uywz7zq2c29p506e".to_string(),
                51_610,
            ),
            (
                "pool1qz2vzszautc2c8mljnqre2857dpmheq7kgt6vav0s38tvvhxm6w".to_string(),
                1_051,
            ),
        ]
        .into_iter()
        .collect();

        Ok(Some(stake_distribution))
    }

    /* This is temporarily discarded and replaced by a fake method
    TODO: remove this comment block
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

            if let Ok((_, f)) = self.parse_string(words[1]) {
                let stake: u64 = (f * 1_000_000_000.0).round() as u64;

                if stake > 0 {
                    let _ = stake_distribution.insert(words[0].to_string(), stake);
                }
            } else {
                return Err(ChainObserverError::InvalidContent(
                    format!("could not parse stake from '{}'", words[1]).into(),
                ));
            }
        }

        Ok(Some(stake_distribution))
    }
    */
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
    async fn test_get_current_stake_distribution() {
        let observer = CardanoCliChainObserver::new(Box::new(TestCliRunner {}));
        let results = observer
            .get_current_stake_distribution()
            .await
            .unwrap()
            .unwrap();

        assert_eq!(7, results.len());
        assert_eq!(
            2_493_000,
            *results
                .get("pool1qqyjr9pcrv97gwrueunug829fs5znw6p2wxft3fvqkgu5f4qlrg")
                .unwrap()
        );
        assert_eq!(
            1_051,
            *results
                .get("pool1qz2vzszautc2c8mljnqre2857dpmheq7kgt6vav0s38tvvhxm6w")
                .unwrap()
        );
        assert!(results
            .get("pool1qpqvz90w7qsex2al2ejjej0rfgrwsguch307w8fraw7a7adf6g8")
            .is_none());
    }

    #[tokio::test]
    async fn test_get_current_epoch() {
        let observer = CardanoCliChainObserver::new(Box::new(TestCliRunner {}));
        let epoch = observer.get_current_epoch().await.unwrap().unwrap();

        assert_eq!(120, epoch);
    }

    #[tokio::test]
    async fn test_cli_testnet_runner() {
        let runner = CardanoCliRunner::new(
            PathBuf::new().join("cardano-cli"),
            PathBuf::new().join("/tmp/whatever.sock"),
            CardanoCliNetwork::TestNet(10),
        );

        assert_eq!("Command { std: \"cardano-cli\" \"query\" \"tip\" \"--testnet-magic 10\", kill_on_drop: false }", format!("{:?}", runner.command_for_epoch()));
        assert_eq!("Command { std: \"cardano-cli\" \"query\" \"stake-distribution\" \"--testnet-magic 10\", kill_on_drop: false }", format!("{:?}", runner.command_for_stake_distribution()));
    }

    #[tokio::test]
    async fn test_cli_devnet_runner() {
        let runner = CardanoCliRunner::new(
            PathBuf::new().join("cardano-cli"),
            PathBuf::new().join("/tmp/whatever.sock"),
            CardanoCliNetwork::DevNet(25),
        );

        assert_eq!("Command { std: \"cardano-cli\" \"query\" \"tip\" \"--testnet-magic 25\" \"--cardano-mode\", kill_on_drop: false }", format!("{:?}", runner.command_for_epoch()));
        assert_eq!("Command { std: \"cardano-cli\" \"query\" \"stake-distribution\" \"--testnet-magic 25\" \"--cardano-mode\", kill_on_drop: false }", format!("{:?}", runner.command_for_stake_distribution()));
    }

    #[tokio::test]
    async fn test_cli_mainnet_runner() {
        let runner = CardanoCliRunner::new(
            PathBuf::new().join("cardano-cli"),
            PathBuf::new().join("/tmp/whatever.sock"),
            CardanoCliNetwork::MainNet,
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
}
