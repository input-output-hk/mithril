use anyhow::{Context, anyhow};
use reqwest::StatusCode;
use slog_scope::{info, warn};
use std::time::Duration;

use mithril_cardano_node_internal_database::entities::ImmutableFile;
use mithril_common::{StdResult, entities::Epoch, messages::EpochSettingsMessage};

use crate::{Aggregator, attempt, utils::AttemptResult};

pub async fn wait_for_enough_immutable(aggregator: &Aggregator) -> StdResult<()> {
    info!("Waiting that enough immutable have been written in the devnet"; "aggregator" => aggregator.name());

    let db_directory = aggregator.db_directory();
    match attempt!(24, Duration::from_secs(5), {
        match ImmutableFile::list_completed_in_dir(db_directory)
            .with_context(|| {
                format!(
                    "Immutable file listing failed in dir `{}`",
                    db_directory.display(),
                )
            })?
            .last()
        {
            Some(_) => Ok(Some(())),
            None => Ok(None),
        }
    }) {
        AttemptResult::Ok(_) => Ok(()),
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted for enough immutable to be written in `{}`",
            db_directory.display()
        )),
    }
}

pub async fn wait_for_epoch_settings(aggregator: &Aggregator) -> StdResult<EpochSettingsMessage> {
    let aggregator_endpoint = aggregator.endpoint();
    let url = format!("{aggregator_endpoint}/epoch-settings");
    info!("Waiting for the aggregator to expose epoch settings"; "aggregator" => aggregator.name());

    match attempt!(20, Duration::from_millis(1000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => {
                    let epoch_settings = response
                        .json::<EpochSettingsMessage>()
                        .await
                        .with_context(|| "Invalid EpochSettings body")?;
                    info!("Aggregator ready"; "epoch_settings"  => ?epoch_settings);
                    Ok(Some(epoch_settings))
                }
                s if s.is_server_error() => {
                    warn!( "Server error while waiting for the Aggregator, http code: {s}"; "aggregator" => aggregator.name());
                    Ok(None)
                }
                _ => Ok(None),
            },
            Err(_) => Ok(None),
        }
    }) {
        AttemptResult::Ok(epoch_settings) => Ok(epoch_settings),
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted for aggregator to be up, no response from `{url}`"
        )),
    }
}

pub async fn wait_for_aggregator_at_target_epoch(
    aggregator: &Aggregator,
    target_epoch: Epoch,
    wait_reason: String,
) -> StdResult<()> {
    info!(
        "Waiting for the cardano network to be at the target epoch: {}", wait_reason;
        "aggregator" => aggregator.name(),
        "target_epoch" => ?target_epoch
    );

    match attempt!(90, Duration::from_millis(1000), {
        match aggregator
            .chain_observer()
            .get_current_epoch()
            .await
            .with_context(|| "Could not query current epoch")?
        {
            Some(epoch) => {
                if epoch >= target_epoch {
                    Ok(Some(()))
                } else {
                    Ok(None)
                }
            }
            None => Ok(None),
        }
    }) {
        AttemptResult::Ok(_) => {
            info!("Target epoch reached!"; "aggregator" => aggregator.name(), "target_epoch" => ?target_epoch);
            Ok(())
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => {
            Err(anyhow!("Timeout exhausted for target epoch to be reached"))
        }
    }?;

    Ok(())
}
