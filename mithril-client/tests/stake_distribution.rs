use std::env::temp_dir;
use std::sync::Arc;

use config::builder::DefaultState;
use config::ConfigBuilder;
use mithril_client::common::StdResult;
use mithril_client::dependencies::DependenciesBuilder;
use mithril_client::services::MithrilStakeDistributionService;

const AGGREGATOR_ENDPOINT: &str =
    "https://aggregator.release-preprod.api.mithril.network/aggregator";
const STAKE_DISTRIBUTION_HASH: &str =
    "ec2e057a52757f3b3b77c1594b42a559df3ed65f6a23d03e2dc295663a65e805";
const GENESIS_VERIFICATION_KEY: &str = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";

async fn get_stake_distribution_service() -> StdResult<Arc<dyn MithrilStakeDistributionService>> {
    let config_builder: ConfigBuilder<DefaultState> = ConfigBuilder::default()
        .set_default("genesis_verification_key", "WRITE THE VKEY HERE")?
        .set_default("aggregator_endpoint", AGGREGATOR_ENDPOINT)?;
    let config = Arc::new(config_builder.build()?);
    let service = DependenciesBuilder::new(config)
        .get_mithril_stake_distribution_service()
        .await?;

    Ok(service)
}

#[tokio::test]
pub async fn list() -> StdResult<()> {
    let service = get_stake_distribution_service().await?;
    let stake_distribution = service.list().await?;

    assert_eq!(20, stake_distribution.len());

    Ok(())
}

// Download function from [MithrilStakeDistributionService] return cannot be used.
// It would be useful to return the [StakeDistribution]
#[tokio::test]
pub async fn download() -> StdResult<()> {
    let path = temp_dir().join("mithril-download-stake-distribution");
    let service = get_stake_distribution_service().await?;
    let _ = service
        .download(STAKE_DISTRIBUTION_HASH, &path, GENESIS_VERIFICATION_KEY)
        .await?;

    Ok(())
}
