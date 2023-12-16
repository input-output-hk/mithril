//! test cli runner

use anyhow::anyhow;
use async_trait::async_trait;

use crate::{chain_observer::CliRunner, StdResult};

/// `TestCliRunner` is a struct to run Cardano CLI tests
pub(crate) struct TestCliRunner {
    is_legacy: bool,
}

impl TestCliRunner {
    fn new(is_legacy: bool) -> Self {
        Self { is_legacy }
    }

    /// Creates a new `TestCliRunner` instance in legacy mode.
    pub fn legacy() -> Self {
        Self::new(true)
    }
}

impl Default for TestCliRunner {
    fn default() -> Self {
        Self::new(false)
    }
}

#[async_trait]
impl CliRunner for TestCliRunner {
    /// launches a UTxO.
    async fn launch_utxo(&self, _address: &str) -> StdResult<String> {
        let output = r#"
{
    "1fd4d3e131afe3c8b212772a3f3083d2fbc6b2a7b20e54e4ff08e001598818d8#0": {
        "address": "addr_test1vpcr3he05gemue6eyy0c9clajqnnww8aa2l3jszjdlszjhq093qrn",
        "datum": null,
        "inlineDatum": {
            "constructor": 0,
            "fields": [
                {
                    "bytes": "5b0a20207b0a20202020226e616d65223a20227468616c6573222c0a202020202265706f6368223a203132330a20207d2c0a20207b0a20202020226e616d65223a20227079746861676f726173222c0a202020202265706f6368223a206e756c6c0a20207d0a5d0a"
                }
            ],
        },
        "inlineDatumhash": "b97cbaa0dc5b41864c83c2f625d9bc2a5f3e6b5cd5071c14a2090e630e188c80",
        "referenceScript": null,
        "value": {
            "lovelace": 10000000
        }
    },
    "1fd4d3e131afe3c8b212772a3f3083d2fbc6b2a7b20e54e4ff08e001598818d8#1": {
        "address": "addr_test1vpcr3he05gemue6eyy0c9clajqnnww8aa2l3jszjdlszjhq093qrn",
        "datum": null,
        "datumhash": null,
        "inlineDatum": null,
        "referenceScript": null,
        "value": {
            "lovelace": 9989656678
        }
    }
}
"#;

        Ok(output.to_string())
    }

    /// launches the stake distribution.
    async fn launch_stake_distribution(&self) -> StdResult<String> {
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

    /// launches the stake snapshot.
    async fn launch_stake_snapshot(&self, stake_pool_id: &str) -> StdResult<String> {
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

    /// launches the stake snapshot for all pools.
    async fn launch_stake_snapshot_all_pools(&self) -> StdResult<String> {
        let output = r#"
{
    "pools": {
        "00000036d515e12e18cd3c88c74f09a67984c2c279a5296aa96efe89": {
            "stakeGo": 300000000000,
            "stakeMark": 300000000001,
            "stakeSet": 300000000002
        },
        "000000f66e28b0f18aef20555f4c4954234e3270dfbbdcc13f54e799": {
            "stakeGo": 600000000000,
            "stakeMark": 600000000001,
            "stakeSet": 600000000002
        },
        "00000110093effbf3ce788aebd3e7506b80322bd3995ad432e61fad5": {
            "stakeGo": 1200000000000,
            "stakeMark": 1200000000001,
            "stakeSet": 1200000000002
        },
        "00000ffff93effbf3ce788aebd3e7506b80322bd3995ad432e61fad5": {
            "stakeGo": 0,
            "stakeMark": 0,
            "stakeSet": 1300000000002
        }
    },
    "total": {
        "stakeGo": 2100000000000,
        "stakeMark": 2100000000003,
        "stakeSet": 2100000000006
    }
}
            "#;
        if self.is_legacy {
            Err(anyhow!(
                "launch_stake_snapshot_all_pools is not implemented in legacy cli runner"
            ))
        } else {
            Ok(output.to_string())
        }
    }

    /// launches the epoch info.
    async fn launch_epoch(&self) -> StdResult<String> {
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

    /// launches the kes period.
    async fn launch_kes_period(&self, _opcert_file: &str) -> StdResult<String> {
        let output = r#"
✓ The operational certificate counter agrees with the node protocol state counter
✓ Operational certificate's kes period is within the correct KES period interval
{
    "qKesNodeStateOperationalCertificateNumber": 6,
    "qKesCurrentKesPeriod": 404,
    "qKesOnDiskOperationalCertificateNumber": 6,
    "qKesRemainingSlotsInKesPeriod": 3760228,
    "qKesMaxKESEvolutions": 62,
    "qKesKesKeyExpiry": "2022-03-20T21:44:51Z",
    "qKesEndKesInterval": 434,
    "qKesStartKesInterval": 372,
    "qKesSlotsPerKesPeriod": 129600
}"#;

        Ok(output.to_string())
    }
}
