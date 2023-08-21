use crate::{attempt, utils::AttemptResult};
use mithril_common::{
    chain_observer::{CardanoCliChainObserver, ChainObserver},
    digesters::ImmutableFile,
    entities::Epoch,
    messages::EpochSettingsMessage,
};
use reqwest::StatusCode;
use slog_scope::{info, warn};
use std::{path::Path, sync::Arc, time::Duration};

pub async fn wait_for_enough_immutable(db_directory: &Path) -> Result<(), String> {
    info!("Waiting that enough immutable have been written in the devnet");

    match attempt!(24, Duration::from_secs(5), {
        match ImmutableFile::list_completed_in_dir(db_directory)
            .map_err(|e| {
                format!(
                    "Immutable file listing failed in dir `{}`: {}",
                    db_directory.display(),
                    e
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
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted for enough immutable to be written in `{}`",
            db_directory.display()
        )),
    }
}

pub async fn wait_for_epoch_settings(
    aggregator_endpoint: &str,
) -> Result<EpochSettingsMessage, String> {
    let url = format!("{aggregator_endpoint}/epoch-settings");
    info!("Waiting for the aggregator to expose epoch settings");

    match attempt!(20, Duration::from_millis(1000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => {
                    let epoch_settings = response
                        .json::<EpochSettingsMessage>()
                        .await
                        .map_err(|e| format!("Invalid EpochSettings body : {e}"))?;
                    info!("Aggregator ready"; "epoch_settings"  => ?epoch_settings);
                    Ok(Some(epoch_settings))
                }
                s if s.is_server_error() => {
                    warn!(
                        "Server error while waiting for the Aggregator, http code: {}",
                        s
                    );
                    Ok(None)
                }
                _ => Ok(None),
            },
            Err(_) => Ok(None),
        }
    }) {
        AttemptResult::Ok(epoch_settings) => Ok(epoch_settings),
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted for aggregator to be up, no response from `{url}`"
        )),
    }
}

pub async fn wait_for_target_epoch(
    chain_observer: Arc<CardanoCliChainObserver>,
    target_epoch: Epoch,
    wait_reason: String,
) -> Result<(), String> {
    info!(
        "Waiting for the cardano network to be at the target epoch: {}", wait_reason;
        "target_epoch" => ?target_epoch
    );

    match attempt!(90, Duration::from_millis(1000), {
        match chain_observer.get_current_epoch().await {
            Ok(Some(epoch)) => {
                if epoch >= target_epoch {
                    Ok(Some(()))
                } else {
                    Ok(None)
                }
            }
            Ok(None) => Ok(None),
            Err(err) => Err(format!("Could not query current epoch: {err}")),
        }
    }) {
        AttemptResult::Ok(_) => {
            info!("Target epoch reached!"; "target_epoch" => ?target_epoch);
            Ok(())
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => {
            Err("Timeout exhausted for target epoch to be reached".to_string())
        }
    }?;

    Ok(())
}
