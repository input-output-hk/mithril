use indicatif::{ProgressBar, ProgressDrawTarget};
use mithril_common::StdResult;
use reqwest::StatusCode;
use slog_scope::{info, warn};
use thiserror::Error;
use tokio::task::JoinSet;

#[derive(Debug, Error)]
pub enum LoadError {
    #[error("Get snapshotlist, expected HTTP code {expected_http_code} got {got_http_code} with the message: {error_message}.")]
    SnapshotListError {
        expected_http_code: u32,
        got_http_code: u32,
        error_message: String,
    },
}

pub async fn download_and_verify_latest_snasphot(endpoint: String) -> StdResult<()> {
    let http_client = reqwest::Client::new();

    let http_request = http_client.get(format!("{}/artifact/snapshots", endpoint));

    let response = http_request.send().await;
    match response {
        Ok(response) => match response.status() {
            StatusCode::OK => Ok(()),
            status => Err(LoadError::SnapshotListError {
                expected_http_code: 200,
                got_http_code: status.as_u16() as u32,
                error_message: response.text().await.unwrap(),
            }
            .into()),
        },
        Err(_) => {
            // Warning, Ok(()) could be returned if the aggregator is down
            Ok(())
        }
    }
}

pub async fn clients_scenario(endpoint: String, num_clients: usize) -> StdResult<usize> {
    info!(">> Run clients scenario with {num_clients}");

    let mut join_set: JoinSet<StdResult<()>> = JoinSet::new();
    let progress_bar =
        ProgressBar::with_draw_target(Some(num_clients as u64), ProgressDrawTarget::stdout());

    for _client_index in 0..num_clients {
        let endpoint_clone = endpoint.clone();
        join_set.spawn(async move { download_and_verify_latest_snasphot(endpoint_clone).await });
    }
    let mut errors = 0;

    while let Some(res) = join_set.join_next().await {
        let res = res.expect("Tokio task join failed!");
        progress_bar.inc(1);

        if res.is_err() {
            warn!("Run download and verify latest snapshot error: {res:?}");
            errors += 1;
        }
    }

    Ok(errors)
}
