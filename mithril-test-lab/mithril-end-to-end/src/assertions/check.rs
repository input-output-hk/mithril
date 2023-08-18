use crate::{
    attempt, utils::AttemptResult, Client, ClientCommand, MithrilStakeDistributionCommand,
    SnapshotCommand,
};
use mithril_common::{
    entities::Epoch,
    messages::{
        CertificateMessage, MithrilStakeDistributionListMessage, MithrilStakeDistributionMessage,
        SnapshotMessage,
    },
};
use reqwest::StatusCode;
use slog_scope::info;
use std::{error::Error, time::Duration};

pub async fn assert_node_producing_mithril_stake_distribution(
    aggregator_endpoint: &str,
) -> Result<String, String> {
    let url = format!("{aggregator_endpoint}/artifact/mithril-stake-distributions");
    info!("Waiting for the aggregator to produce a mithril stake distribution");

    // todo: reduce the number of attempts if we can reduce the delay between two immutables
    match attempt!(45, Duration::from_millis(2000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<MithrilStakeDistributionListMessage>().await.as_deref() {
                    Ok([stake_distribution, ..]) => Ok(Some(stake_distribution.hash.clone())),
                    Ok(&[]) => Ok(None),
                    Err(err) => Err(format!("Invalid mithril stake distribution body : {err}",)),
                },
                s => Err(format!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(format!("Request to `{url}` failed: {err}")),
        }
    }) {
        AttemptResult::Ok(hash) => {
            info!("Aggregator produced a mithril stake distribution"; "hash" => &hash);
            Ok(hash)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted assert_node_producing_mithril_stake_distribution, no response from `{url}`"
        )),
    }
}

pub async fn assert_signer_is_signing_mithril_stake_distribution(
    aggregator_endpoint: &str,
    hash: &str,
    expected_epoch_min: Epoch,
) -> Result<String, String> {
    let url = format!("{aggregator_endpoint}/artifact/mithril-stake-distribution/{hash}");
    info!(
        "Asserting the aggregator is signing the mithril stake distribution message `{}` with an expected min epoch of `{}`",
        hash,
        expected_epoch_min
    );

    match attempt!(10, Duration::from_millis(1000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<MithrilStakeDistributionMessage>().await {
                    Ok(stake_distribution) => match stake_distribution.epoch {
                        epoch if epoch >= expected_epoch_min => Ok(Some(stake_distribution)),
                        epoch => Err(format!(
                            "Minimum expected mithril stake distribution epoch not reached : {epoch} < {expected_epoch_min}"
                        )),
                    },
                    Err(err) => Err(format!("Invalid mithril stake distribution body : {err}",)),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(format!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(format!("Request to `{url}` failed: {err}")),
        }
    }) {
        AttemptResult::Ok(stake_distribution) => {
            // todo: assert that the mithril stake distribution is really signed
            info!("Signer signed a mithril stake distribution"; "certificate_hash" => &stake_distribution.certificate_hash);
            Ok(stake_distribution.certificate_hash)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted assert_signer_is_signing_mithril_stake_distribution, no response from `{url}`"
        )),
    }
}

pub async fn assert_node_producing_snapshot(aggregator_endpoint: &str) -> Result<String, String> {
    let url = format!("{aggregator_endpoint}/artifact/snapshots");
    info!("Waiting for the aggregator to produce a snapshot");

    // todo: reduce the number of attempts if we can reduce the delay between two immutables
    match attempt!(45, Duration::from_millis(2000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Vec<SnapshotMessage>>().await.as_deref() {
                    Ok([snapshot, ..]) => Ok(Some(snapshot.digest.clone())),
                    Ok(&[]) => Ok(None),
                    Err(err) => Err(format!("Invalid snapshot body : {err}",)),
                },
                s => Err(format!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(format!("Request to `{url}` failed: {err}")),
        }
    }) {
        AttemptResult::Ok(digest) => {
            info!("Aggregator produced a snapshot"; "digest" => &digest);
            Ok(digest)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted assert_node_producing_snapshot, no response from `{url}`"
        )),
    }
}

pub async fn assert_signer_is_signing_snapshot(
    aggregator_endpoint: &str,
    digest: &str,
    expected_epoch_min: Epoch,
) -> Result<String, String> {
    let url = format!("{aggregator_endpoint}/artifact/snapshot/{digest}");
    info!(
        "Asserting the aggregator is signing the snapshot message `{}` with an expected min epoch of `{}`",
        digest,
        expected_epoch_min
    );

    match attempt!(10, Duration::from_millis(1000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<SnapshotMessage>().await {
                    Ok(snapshot) => match snapshot.beacon.epoch {
                        epoch if epoch >= expected_epoch_min => Ok(Some(snapshot)),
                        epoch => Err(format!(
                            "Minimum expected snapshot epoch not reached : {epoch} < {expected_epoch_min}"
                        )),
                    },
                    Err(err) => Err(format!("Invalid snapshot body : {err}",)),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(format!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(format!("Request to `{url}` failed: {err}")),
        }
    }) {
        AttemptResult::Ok(snapshot) => {
            // todo: assert that the snapshot is really signed
            info!("Signer signed a snapshot"; "certificate_hash" => &snapshot.certificate_hash);
            Ok(snapshot.certificate_hash)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted assert_signer_is_signing_snapshot, no response from `{url}`"
        )),
    }
}

pub async fn assert_is_creating_certificate_with_enough_signers(
    aggregator_endpoint: &str,
    certificate_hash: &str,
    total_signers_expected: usize,
) -> Result<(), String> {
    let url = format!("{aggregator_endpoint}/certificate/{certificate_hash}");

    match attempt!(10, Duration::from_millis(1000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<CertificateMessage>().await {
                    Ok(certificate) => Ok(Some(certificate)),
                    Err(err) => Err(format!("Invalid snapshot body : {err}",)),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(format!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(format!("Request to `{url}` failed: {err}")),
        }
    }) {
        AttemptResult::Ok(certificate) => {
            info!("Aggregator produced a certificate"; "certificate" => ?certificate);
            if certificate.metadata.signers.len() == total_signers_expected {
                info!(
                    "Certificate is signed by expected number of signers: {} >= {} ",
                    certificate.metadata.signers.len(),
                    total_signers_expected
                );
                Ok(())
            } else {
                Err(format!(
                    "Certificate is not signed by expected number of signers: {} < {} ",
                    certificate.metadata.signers.len(),
                    total_signers_expected
                ))
            }
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted assert_is_creating_certificate, no response from `{url}`"
        )),
    }
}

pub async fn assert_client_can_verify_snapshot(
    client: &mut Client,
    digest: &str,
) -> Result<(), String> {
    client
        .run(ClientCommand::Snapshot(SnapshotCommand::Download {
            digest: digest.to_string(),
        }))
        .await?;
    info!("Client downloaded & restored the snapshot"; "digest" => &digest);

    Ok(())
}

pub async fn assert_client_can_verify_mithril_stake_distribution(
    client: &mut Client,
    hash: &str,
) -> Result<(), Box<dyn Error>> {
    client
        .run(ClientCommand::MithrilStakeDistribution(
            MithrilStakeDistributionCommand::Download {
                hash: hash.to_owned(),
            },
        ))
        .await?;
    info!("Client downloaded the Mithril stake distribution"; "hash" => &hash);

    Ok(())
}
