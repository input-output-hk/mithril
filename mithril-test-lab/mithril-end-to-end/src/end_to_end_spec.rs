use crate::utils::AttemptResult;
use crate::{attempt, Client, ClientCommand, MithrilInfrastructure};
use mithril_common::chain_observer::{CardanoCliChainObserver, ChainObserver};
use mithril_common::digesters::ImmutableFile;
use mithril_common::entities::{Certificate, CertificatePending, Snapshot};
use mithril_common::{SIGNER_EPOCH_RECORDING_OFFSET, SIGNER_EPOCH_RETRIEVAL_OFFSET};
use reqwest::StatusCode;
use slog_scope::info;
use std::error::Error;
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

pub struct Spec {
    infrastructure: MithrilInfrastructure,
}

impl Spec {
    pub fn new(infrastructure: MithrilInfrastructure) -> Self {
        Self { infrastructure }
    }

    pub async fn run(&self) -> Result<(), Box<dyn Error>> {
        let aggregator_endpoint = self.infrastructure.aggregator().endpoint();

        wait_for_enough_immutable(self.infrastructure.aggregator().db_directory()).await?;
        let min_epoch = (SIGNER_EPOCH_RECORDING_OFFSET - SIGNER_EPOCH_RETRIEVAL_OFFSET) as u64;
        wait_for_target_epoch(
            self.infrastructure.chain_observer(),
            min_epoch,
            "minimal epoch for the signer to be able to sign snapshots".to_string(),
        )
        .await?;
        wait_for_pending_certificate(&aggregator_endpoint).await?;
        wait_for_target_epoch(
            self.infrastructure.chain_observer(),
            min_epoch + 4,
            "epoch after which the stake distribution will change".to_string(),
        )
        .await?; // TODO: This value should be lower: min_epoch+2, but keep it like that until we find out why some epochs don't record signers
                 // TODO: Add a transaction that updates the stake distribution here
        wait_for_target_epoch(
            self.infrastructure.chain_observer(),
            min_epoch + 7,
            "epoch after which thecertificate chain will be long enough".to_string(),
        )
        .await?; // TODO: This value should be lower: min_epoch+4, but keep it like that until we find out why some epochs don't record signers
        let digest = assert_node_producing_snapshot(&aggregator_endpoint).await?;
        let certificate_hash =
            assert_signer_is_signing_snapshot(&aggregator_endpoint, &digest).await?;
        assert_is_creating_certificate(&aggregator_endpoint, &certificate_hash).await?;

        let mut client = self.infrastructure.build_client()?;
        assert_client_can_verify_snapshot(&mut client, &digest).await?;

        Ok(())
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> Result<(), String> {
        self.infrastructure
            .aggregator()
            .tail_logs(number_of_line)
            .await?;
        for signer in self.infrastructure.signers() {
            signer.tail_logs(number_of_line).await?;
        }

        Ok(())
    }
}

async fn wait_for_enough_immutable(db_directory: &Path) -> Result<(), String> {
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

async fn wait_for_pending_certificate(
    aggregator_endpoint: &str,
) -> Result<CertificatePending, String> {
    let url = format!("{}/certificate-pending", aggregator_endpoint);
    info!("Waiting for the aggregator to produce a pending certificate");

    match attempt!(10, Duration::from_millis(1000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => {
                    let certificate = response
                        .json::<CertificatePending>()
                        .await
                        .map_err(|e| format!("Invalid CertificatePending body : {}", e))?;
                    info!("Aggregator ready"; "pending_certificate"  => #?certificate);
                    Ok(Some(certificate))
                }
                s if s.is_server_error() => Err(format!(
                    "Server error while waiting for the Aggregator, http code: {}",
                    s
                )),
                _ => Ok(None),
            },
            Err(_) => Ok(None),
        }
    }) {
        AttemptResult::Ok(certificate) => Ok(certificate),
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted for aggregator to be up, no response from `{}`",
            url
        )),
    }
}

async fn wait_for_target_epoch(
    chain_observer: Arc<CardanoCliChainObserver>,
    target_epoch: u64,
    wait_reason: String,
) -> Result<(), String> {
    info!(
        "Waiting for the cardano network to be at the target epoch: {}", wait_reason;
        "target_epoch" => target_epoch
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
            Err(err) => Err(format!("Could not query current epoch: {}", err)),
        }
    }) {
        AttemptResult::Ok(_) => {
            info!("Target epoch reached !"; "target_epoch" => target_epoch);
            Ok(())
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => {
            Err("Timeout exhausted for target epoch to be reached".to_string())
        }
    }?;

    Ok(())
}

async fn assert_node_producing_snapshot(aggregator_endpoint: &str) -> Result<String, String> {
    let url = format!("{}/snapshots", aggregator_endpoint);
    info!("Waiting for the aggregator to produce a snapshot");

    // todo: reduce the number of attempts if we can reduce the delay between two immutables
    match attempt!(45, Duration::from_millis(2000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Vec<Snapshot>>().await.as_deref() {
                    Ok([snapshot, ..]) => Ok(Some(snapshot.digest.clone())),
                    Ok(&[]) => Ok(None),
                    Err(err) => Err(format!("Invalid snapshot body : {}", err,)),
                },
                s => Err(format!("Unexpected status code from Aggregator: {}", s)),
            },
            Err(err) => Err(format!("Request to `{}` failed: {}", url, err)),
        }
    }) {
        AttemptResult::Ok(digest) => {
            info!("Aggregator produced a snapshot"; "digest" => &digest);
            Ok(digest)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted assert_node_producing_snapshot, no response from `{}`",
            url
        )),
    }
}

async fn assert_signer_is_signing_snapshot(
    aggregator_endpoint: &str,
    digest: &str,
) -> Result<String, String> {
    let url = format!("{}/snapshot/{}", aggregator_endpoint, digest);
    info!(
        "Waiting for the aggregator to produce sign the snapshot `{}`",
        digest
    );

    match attempt!(10, Duration::from_millis(1000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Snapshot>().await {
                    Ok(snapshot) => Ok(Some(snapshot)),
                    Err(err) => Err(format!("Invalid snapshot body : {}", err,)),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(format!("Unexpected status code from Aggregator: {}", s)),
            },
            Err(err) => Err(format!("Request to `{}` failed: {}", url, err)),
        }
    }) {
        AttemptResult::Ok(snapshot) => {
            // todo: assert that the snapshot is really signed
            info!("Signer signed a snapshot"; "certificate_hash" => &snapshot.certificate_hash);
            Ok(snapshot.certificate_hash)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted assert_signer_is_signing_snapshot, no response from `{}`",
            url
        )),
    }
}

async fn assert_is_creating_certificate(
    aggregator_endpoint: &str,
    certificate_hash: &str,
) -> Result<(), String> {
    let url = format!("{}/certificate/{}", aggregator_endpoint, certificate_hash);

    match attempt!(10, Duration::from_millis(1000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Certificate>().await {
                    Ok(certificate) => Ok(Some(certificate)),
                    Err(err) => Err(format!("Invalid snapshot body : {}", err,)),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(format!("Unexpected status code from Aggregator: {}", s)),
            },
            Err(err) => Err(format!("Request to `{}` failed: {}", url, err)),
        }
    }) {
        AttemptResult::Ok(certificate) => {
            info!("Aggregator produced a certificate"; "certificate" => #?certificate);
            Ok(())
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted assert_is_creating_certificate, no response from `{}`",
            url
        )),
    }
}

async fn assert_client_can_verify_snapshot(
    client: &mut Client,
    digest: &str,
) -> Result<(), String> {
    client
        .run(ClientCommand::Download {
            digest: digest.to_string(),
        })
        .await?;
    info!("Client downloaded the snapshot"; "digest" => &digest);

    client
        .run(ClientCommand::Restore {
            digest: digest.to_string(),
        })
        .await?;
    info!("Client restored the snapshot"; "digest" => &digest);

    Ok(())
}
