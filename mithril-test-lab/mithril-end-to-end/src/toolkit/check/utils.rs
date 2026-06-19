use anyhow::{Context, anyhow};
use reqwest::StatusCode;
use serde::de::DeserializeOwned;
use slog_scope::info;

use mithril_common::StdResult;

use crate::utils::TimeoutReason;
use crate::{Aggregator, attempt, toolkit::ScenarioToolkitContext, utils::AttemptResult};

pub async fn get_json_response<T: DeserializeOwned>(url: String) -> StdResult<reqwest::Result<T>> {
    match reqwest::get(url.clone()).await {
        Ok(response) => {
            let r = response.status();
            match r {
                StatusCode::OK => Ok(response.json::<T>().await),
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            }
        }
        Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
    }
}

/// Wait until the aggregator produces an artifact, returning the latest one
///
/// Note: the `artifact_list_url` must start with a `/`
pub async fn wait_for_latest_artifact_with_condition<T, P, E>(
    artifact_name: &str,
    artifact_list_url: &str,
    hash_extractor: fn(&T) -> String,
    context: &ScenarioToolkitContext,
    aggregator: &Aggregator,
    condition: P,
    invalid_condition_error_msg_callback: E,
) -> StdResult<T>
where
    T: DeserializeOwned,
    P: Fn(&T) -> bool,
    E: Fn(T) -> String,
{
    let url = format!("{}{artifact_list_url}", aggregator.endpoint());
    info!("Waiting for the aggregator to produce a {artifact_name} "; "aggregator" => &aggregator.name());

    async fn fetch_last_artifact<T: DeserializeOwned>(
        artifact_name: &str,
        url: String,
    ) -> StdResult<Option<T>> {
        match get_json_response::<Vec<T>>(url).await? {
            // Artifact lists are sorted from newest to oldest, so the first item is the latest
            Ok(list) => Ok(list.into_iter().next()),
            Err(err) => Err(anyhow!("Invalid {artifact_name} artifact body: {err}",)),
        }
    }

    match attempt!(20, context.tenth_epoch_delay(), {
        fetch_last_artifact(artifact_name, url.clone()).await
    }, until &condition) {
        AttemptResult::Ok(last_artifact) => {
            info!("Aggregator produced a {artifact_name} artifact"; "hash" => hash_extractor(&last_artifact), "aggregator" => &aggregator.name());
            Ok(last_artifact)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout(TimeoutReason::NoResponse) => Err(anyhow!(
            "Timeout exhausted waiting for {artifact_name}, no response from `{url}`"
        )),
        AttemptResult::Timeout(TimeoutReason::PredicateNotSatisfied(last_response)) => Err(anyhow!(
            "Timeout exhausted waiting for {artifact_name}, {}", invalid_condition_error_msg_callback(last_response)
        )),
    }
        .with_context(|| format!("Requesting aggregator `{}`", aggregator.name()))
}
