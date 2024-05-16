use anyhow::anyhow;
use async_recursion::async_recursion;
use reqwest::StatusCode;
use serde::Deserialize;
use slog_scope::warn;
use std::time::Duration;
use tokio::time::sleep;

use mithril_common::{
    entities::{Epoch, SignedEntityType},
    messages::{
        CertificateListItemMessage, CertificatePendingMessage, EpochSettingsMessage,
        MithrilStakeDistributionListItemMessage, SnapshotListItemMessage,
    },
    StdResult,
};

use crate::Aggregator;

#[async_recursion]
async fn request_first_list_item_with_expected_size<I>(
    url: &str,
    expected_size: usize,
) -> StdResult<I>
where
    for<'a> I: Deserialize<'a> + Sync + Send + Clone,
{
    sleep(Duration::from_millis(300)).await;

    match reqwest::get(url).await {
        Ok(response) => match response.status() {
            StatusCode::OK => match response.json::<Vec<I>>().await.as_deref() {
                Ok(list) if list.len() == expected_size => Ok(list.first().unwrap().clone()),
                Ok(list) if list.len() > expected_size => Err(anyhow!(
                    "Invalid size, expected {expected_size}, got {}",
                    list.len()
                )),
                Ok(_) => request_first_list_item_with_expected_size::<I>(url, expected_size).await,
                Err(err) => Err(anyhow!("Invalid list body : {err}")),
            },
            s if s.is_server_error() => {
                let message = format!(
                    "Server error while waiting for the Aggregator, http code: {}",
                    s
                );
                warn!("{message}");
                Err(anyhow!(message))
            }
            _ => request_first_list_item_with_expected_size::<I>(url, expected_size).await,
        },
        Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
    }
}

/// Wait for http response until timeout
pub async fn for_http_response(url: &str, timeout: Duration, message: &str) -> StdResult<()> {
    spin_while_waiting!(
        {
            while reqwest::get(url).await.is_err() {
                sleep(Duration::from_millis(300)).await;
            }
            Ok(())
        },
        timeout,
        message.to_owned(),
        format!("Aggregator did not get a response after {timeout:?} from '{url}'")
    )
}

/// Wait for a given epoch in the epoch settings until timeout
pub async fn for_epoch_settings_at_epoch(
    aggregator: &Aggregator,
    timeout: Duration,
    epoch: Epoch,
) -> StdResult<()> {
    let url = &format!("{}/epoch-settings", aggregator.endpoint());
    spin_while_waiting!(
        {
            while let Ok(response) = reqwest::get(url).await {
                match response.status() {
                    StatusCode::OK => {
                        let epoch_settings = response.json::<EpochSettingsMessage>().await.unwrap();

                        if epoch_settings.epoch >= epoch {
                            break;
                        }
                        sleep(Duration::from_millis(300)).await
                    }
                    s if s.is_server_error() => {
                        warn!(
                            "Server error while waiting for the Aggregator, http code: {}",
                            s
                        );
                        break;
                    }
                    _ => sleep(Duration::from_millis(300)).await,
                }
            }
            Ok(())
        },
        timeout,
        format!("Waiting for epoch {epoch}"),
        format!("Aggregator did not get a response after {timeout:?} from '{url}'")
    )
}

/// Wait for pending certificate
pub async fn for_pending_certificate(
    aggregator: &Aggregator,
    timeout: Duration,
    signed_entity_type: &SignedEntityType,
) -> StdResult<()> {
    let url = &format!("{}/certificate-pending", aggregator.endpoint());
    spin_while_waiting!(
        {
            while let Ok(response) = reqwest::get(url).await {
                match response.status() {
                    StatusCode::OK => {
                        let pending_certificate =
                            response.json::<CertificatePendingMessage>().await.unwrap();

                        if &pending_certificate.signed_entity_type == signed_entity_type {
                            break;
                        }
                        sleep(Duration::from_millis(300)).await
                    }
                    s if s.is_server_error() => {
                        warn!(
                            "Server error while waiting for the Aggregator, http code: {}",
                            s
                        );
                        break;
                    }
                    _ => sleep(Duration::from_millis(300)).await,
                }
            }
            Ok(())
        },
        timeout,
        format!("Waiting for pending certificate"),
        format!("Aggregator did not get a response after {timeout:?} from '{url}'")
    )
}

/// Wait for the given number of certificates, return the latest certificate
pub async fn for_certificates(
    aggregator: &Aggregator,
    total: usize,
    timeout: Duration,
) -> StdResult<CertificateListItemMessage> {
    let url = &format!("{}/certificates", aggregator.endpoint());
    spin_while_waiting!(
        {
            request_first_list_item_with_expected_size::<CertificateListItemMessage>(url, total)
                .await
                .map_err(|e| anyhow!(e).context("Request first certificate failure"))
        },
        timeout,
        format!("Waiting for certificates"),
        format!("Aggregator did not get a response after {timeout:?} from '{url}'")
    )
}

/// Wait for Mithril Stake Distribution artifacts
pub async fn for_mithril_stake_distribution_artifacts(
    aggregator: &Aggregator,
    total: usize,
    timeout: Duration,
) -> StdResult<MithrilStakeDistributionListItemMessage> {
    let url = &format!(
        "{}/artifact/mithril-stake-distributions",
        aggregator.endpoint()
    );
    spin_while_waiting!(
        {
            request_first_list_item_with_expected_size::<MithrilStakeDistributionListItemMessage>(
                url, total,
            )
            .await
            .map_err(|e| anyhow!(e).context("Request first mithril stake distribution failure"))
        },
        timeout,
        format!("Waiting for mithril stake distribution artifacts"),
        format!("Aggregator did not get a response after {timeout:?} from '{url}'")
    )
}

/// Wait for Cardano Immutable Files artifacts
pub async fn for_immutable_files_artifacts(
    aggregator: &Aggregator,
    total: usize,
    timeout: Duration,
) -> StdResult<SnapshotListItemMessage> {
    let url = &format!("{}/artifact/snapshots", aggregator.endpoint());
    spin_while_waiting!(
        {
            request_first_list_item_with_expected_size::<SnapshotListItemMessage>(url, total)
                .await
                .map_err(|e| anyhow!(e).context("Request first snapshot failure"))
        },
        timeout,
        format!("Waiting for immutable files artifacts"),
        format!("Aggregator did not get a response after {timeout:?} from '{url}'")
    )
}
