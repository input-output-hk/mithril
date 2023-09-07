use std::sync::Arc;

use anyhow::anyhow;
use async_recursion::async_recursion;
use indicatif::{ProgressBar, ProgressDrawTarget};
use mithril_common::{
    messages::{CertificateMessage, SnapshotListItemMessage, SnapshotListMessage},
    StdResult,
};
use reqwest::StatusCode;
use slog_scope::{info, warn};
use thiserror::Error;
use tokio::task::JoinSet;

#[derive(Debug, Error)]
pub enum LoadError {
    #[error("Get snapshot list, expected HTTP code {expected_http_code} got {got_http_code} with the message: {error_message}.")]
    SnapshotListError {
        expected_http_code: u32,
        got_http_code: u32,
        error_message: String,
    },
    #[error("Get snapshot detail, expected HTTP code {expected_http_code} got {got_http_code} with the message: {error_message}.")]
    SnapshotDetailError {
        expected_http_code: u32,
        got_http_code: u32,
        error_message: String,
    },
    #[error("Get certificate detail, expected HTTP code {expected_http_code} got {got_http_code} with the message: {error_message}.")]
    CertificateDetailError {
        expected_http_code: u32,
        got_http_code: u32,
        error_message: String,
    },
    #[error("Unreachable aggregator")]
    UnreachableAggregatorList,
    #[error("Empty snapshot list received")]
    EmptySnapshotList,
}

pub async fn download_latest_snasphot(
    http_client: Arc<reqwest::Client>,
    endpoint: &str,
) -> StdResult<SnapshotListItemMessage> {
    let http_request = http_client.get(format!("{}/artifact/snapshots", endpoint));
    let response = http_request.send().await;
    let snapshots: SnapshotListMessage = match response {
        Ok(response) => match response.status() {
            StatusCode::OK => Ok(serde_json::from_str::<SnapshotListMessage>(
                &response.text().await.unwrap(),
            )
            .expect("this error should never happen")),
            status => Err(LoadError::SnapshotListError {
                expected_http_code: 200,
                got_http_code: status.as_u16() as u32,
                error_message: response.text().await.unwrap(),
            }),
        },
        Err(_) => Err(LoadError::UnreachableAggregatorList),
    }?;

    let last_snapshot = snapshots.first().ok_or(LoadError::EmptySnapshotList)?;
    let http_request = http_client.get(format!(
        "{}/artifact/snapshot/{}",
        endpoint, last_snapshot.digest
    ));
    let response = http_request.send().await;
    match response {
        Ok(response) => match response.status() {
            StatusCode::OK => Ok(last_snapshot.to_owned()),
            status => Err(anyhow!(LoadError::SnapshotDetailError {
                expected_http_code: 200,
                got_http_code: status.as_u16() as u32,
                error_message: response.text().await.unwrap(),
            })),
        },
        Err(_) => {
            // Warning, Ok(()) could be returned if the aggregator is down
            Ok(last_snapshot.to_owned())
        }
    }
}

#[async_recursion]
pub async fn download_certificate_chain(
    http_client: Arc<reqwest::Client>,
    endpoint: &str,
    certificate_hash: &str,
) -> StdResult<()> {
    let http_request = http_client.get(format!("{}/certificate/{}", endpoint, certificate_hash));
    let response = http_request.send().await;
    let certificate: CertificateMessage = match response {
        Ok(response) => match response.status() {
            StatusCode::OK => Ok(serde_json::from_str::<CertificateMessage>(
                &response.text().await.unwrap(),
            )
            .expect("this error should never happen")),
            status => Err(LoadError::CertificateDetailError {
                expected_http_code: 200,
                got_http_code: status.as_u16() as u32,
                error_message: response.text().await.unwrap(),
            }),
        },
        Err(_) => Err(LoadError::UnreachableAggregatorList),
    }?;

    if certificate.previous_hash.is_empty() {
        return download_certificate_chain(http_client, endpoint, &certificate.previous_hash).await;
    }

    Ok(())
}

pub async fn clients_scenario(endpoint: String, num_clients: usize) -> StdResult<usize> {
    info!(">> Run clients scenario with {num_clients} clients");

    let mut join_set: JoinSet<StdResult<()>> = JoinSet::new();
    let progress_bar =
        ProgressBar::with_draw_target(Some(num_clients as u64), ProgressDrawTarget::stdout());

    let http_client = Arc::new(reqwest::Client::new());
    for _client_index in 0..num_clients {
        let endpoint_clone = endpoint.clone();
        let http_client_clone = http_client.clone();
        join_set.spawn(async move {
            let last_snapshot =
                download_latest_snasphot(http_client_clone.clone(), &endpoint_clone).await?;
            download_certificate_chain(
                http_client_clone,
                &endpoint_clone,
                &last_snapshot.certificate_hash,
            )
            .await?;

            Ok(())
        });
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
