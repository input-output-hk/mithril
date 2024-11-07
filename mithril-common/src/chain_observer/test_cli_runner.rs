//! test cli runner

use anyhow::anyhow;
use async_trait::async_trait;

use crate::{chain_observer::CliRunner, StdResult};

pub(crate) mod test_expected {

    pub(crate) mod launch_era {
        pub(crate) const ERA: &str = "Conway";
    }
    pub(crate) mod launch_epoch {
        use crate::entities::Epoch;

        pub(crate) const EPOCH: Epoch = Epoch(120);
    }
    pub(crate) mod launch_chain_point {
        use crate::entities::{BlockNumber, SlotNumber};

        pub(crate) const SLOT_NUMBER: SlotNumber = SlotNumber(25886617);
        pub(crate) const BLOCK_NUMBER: BlockNumber = BlockNumber(1270276);
        pub(crate) const BLOCK_HASH: &str =
            "7383b17d7b05b0953cf0649abff60173995eb9febe556889333e20e1e5b7ca84";
    }
    pub(crate) mod launch_utxo {
        pub(crate) const BYTES: &str = "5b0a20207b0a20202020226e616d65223a20227468616c6573222c0a202020202265706f6368223a203132330a20207d2c0a20207b0a20202020226e616d65223a20227079746861676f726173222c0a202020202265706f6368223a206e756c6c0a20207d0a5d0a";
    }
    pub(crate) mod launch_kes_period {
        pub(crate) const KES_PERIOD: u32 = 404;
    }
    pub(crate) mod launch_stake_snapshot {
        pub(crate) const DEFAULT_POOL_STAKE_MARK: u64 = 3_000_000;
        pub(crate) const POOL_ID_SPECIFIC: &str =
            "pool1qpqvz90w7qsex2al2ejjej0rfgrwsguch307w8fraw7a7adf6g8";
        pub(crate) const POOL_STAKE_MARK_FOR_POOL_ID_SPECIFIC: u64 = 0;
    }
    pub(crate) mod launch_stake_snapshot_all_pools {
        pub(crate) const STAKE_MARK_POOL_1: u64 = 300000000001;
        pub(crate) const STAKE_MARK_POOL_2: u64 = 600000000001;
        pub(crate) const STAKE_MARK_POOL_3: u64 = 1200000000001;
    }
}

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
        let output = format!(
            r#"
{{
    "1fd4d3e131afe3c8b212772a3f3083d2fbc6b2a7b20e54e4ff08e001598818d8#0": {{
        "address": "addr_test1vpcr3he05gemue6eyy0c9clajqnnww8aa2l3jszjdlszjhq093qrn",
        "datum": null,
        "inlineDatum": {{
            "constructor": 0,
            "fields": [
                {{
                    "bytes": "{}"
                }}
            ]
        }},
        "inlineDatumhash": "b97cbaa0dc5b41864c83c2f625d9bc2a5f3e6b5cd5071c14a2090e630e188c80",
        "referenceScript": null,
        "value": {{
            "lovelace": 10000000
        }}
    }},
    "1fd4d3e131afe3c8b212772a3f3083d2fbc6b2a7b20e54e4ff08e001598818d8#1": {{
        "address": "addr_test1vpcr3he05gemue6eyy0c9clajqnnww8aa2l3jszjdlszjhq093qrn",
        "datum": null,
        "datumhash": null,
        "inlineDatum": null,
        "referenceScript": null,
        "value": {{
            "lovelace": 9989656678
        }}
    }}
}}
"#,
            test_expected::launch_utxo::BYTES
        );

        Ok(output)
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
        let pool_stake_mark =
            if stake_pool_id == test_expected::launch_stake_snapshot::POOL_ID_SPECIFIC {
                test_expected::launch_stake_snapshot::POOL_STAKE_MARK_FOR_POOL_ID_SPECIFIC
            } else {
                test_expected::launch_stake_snapshot::DEFAULT_POOL_STAKE_MARK
            };

        let output = format!(
            r#"
{{
    "poolStakeGo": 1000000,
    "poolStakeSet": 2000000,
    "poolStakeMark": {pool_stake_mark},
    "activeStakeGo": 5000000,
    "activeStakeSet": 7000000,
    "activeStakeMark": 9000000
}}"#
        );

        Ok(output)
    }

    /// launches the stake snapshot for all pools.
    async fn launch_stake_snapshot_all_pools(&self) -> StdResult<String> {
        let output = format!(
            r#"
{{
    "pools": {{
        "00000036d515e12e18cd3c88c74f09a67984c2c279a5296aa96efe89": {{
            "stakeGo": 300000000000,
            "stakeMark": {},
            "stakeSet": 300000000002
        }},
        "000000f66e28b0f18aef20555f4c4954234e3270dfbbdcc13f54e799": {{
            "stakeGo": 600000000000,
            "stakeMark": {},
            "stakeSet": 600000000002
        }},
        "00000110093effbf3ce788aebd3e7506b80322bd3995ad432e61fad5": {{
            "stakeGo": 1200000000000,
            "stakeMark": {},
            "stakeSet": 1200000000002
        }},
        "00000ffff93effbf3ce788aebd3e7506b80322bd3995ad432e61fad5": {{
            "stakeGo": 0,
            "stakeMark": 0,
            "stakeSet": 1300000000002
        }}
    }},
    "total": {{
        "stakeGo": 2100000000000,
        "stakeMark": 2100000000003,
        "stakeSet": 2100000000006
    }}
}}"#,
            test_expected::launch_stake_snapshot_all_pools::STAKE_MARK_POOL_1,
            test_expected::launch_stake_snapshot_all_pools::STAKE_MARK_POOL_2,
            test_expected::launch_stake_snapshot_all_pools::STAKE_MARK_POOL_3,
        );

        if self.is_legacy {
            Err(anyhow!(
                "launch_stake_snapshot_all_pools is not implemented in legacy cli runner"
            ))
        } else {
            Ok(output)
        }
    }

    /// launches the era info.
    async fn launch_era(&self) -> StdResult<String> {
        let output = format!(
            r#"
{{
    "era": "{}",
    "syncProgress": "100.00",
    "hash": "f6d1b8c328697c7a4a8e7f718c79510acbcd411ff4ca19401ded13534d45a38d",
    "epoch": 735,
    "slot": 0,
    "block": 0
}}"#,
            test_expected::launch_era::ERA
        );

        Ok(output)
    }

    /// launches the epoch info.
    async fn launch_epoch(&self) -> StdResult<String> {
        let output = format!(
            r#"
{{
    "era": "Alonzo",
    "syncProgress": "100.00",
    "hash": "f6d1b8c328697c7a4a8e7f718c79510acbcd411ff4ca19401ded13534d45a38d",
    "epoch": {},
    "slot": 0,
    "block": 0
}}"#,
            test_expected::launch_epoch::EPOCH
        );

        Ok(output)
    }

    /// launches the chain point info.
    async fn launch_chain_point(&self) -> StdResult<String> {
        let output = format!(
            r#"
{{
    "block": {},
    "epoch": 299,
    "era": "Conway",
    "hash": "{}",
    "slot": {},
    "slotInEpoch": 53017,
    "slotsToEpochEnd": 33383,
    "syncProgress": "100.00"
}}"#,
            test_expected::launch_chain_point::BLOCK_NUMBER,
            test_expected::launch_chain_point::BLOCK_HASH,
            test_expected::launch_chain_point::SLOT_NUMBER,
        );

        Ok(output)
    }

    /// launches the kes period.
    async fn launch_kes_period(&self, _opcert_file: &str) -> StdResult<String> {
        let output = format!(
            r#"
✓ The operational certificate counter agrees with the node protocol state counter
✓ Operational certificate's kes period is within the correct KES period interval
{{
    "qKesNodeStateOperationalCertificateNumber": 6,
    "qKesCurrentKesPeriod": {},
    "qKesOnDiskOperationalCertificateNumber": 6,
    "qKesRemainingSlotsInKesPeriod": 3760228,
    "qKesMaxKESEvolutions": 62,
    "qKesKesKeyExpiry": "2022-03-20T21:44:51Z",
    "qKesEndKesInterval": 434,
    "qKesStartKesInterval": 372,
    "qKesSlotsPerKesPeriod": 129600
}}"#,
            test_expected::launch_kes_period::KES_PERIOD
        );

        Ok(output)
    }
}
