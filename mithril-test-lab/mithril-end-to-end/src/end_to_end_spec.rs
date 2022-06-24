use crate::utils::AttemptResult;
use crate::{attempt, Client, ClientCommand, MithrilInfrastructure};
use mithril_common::digesters::ImmutableFile;
use mithril_common::entities::{Certificate, Snapshot};
use reqwest::StatusCode;
use slog_scope::info;
use std::error::Error;
use std::path::Path;
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
        wait_for_pending_certificate(&aggregator_endpoint).await?;

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

async fn wait_for_pending_certificate(aggregator_endpoint: &str) -> Result<(), String> {
    let url = format!("{}/certificate-pending", aggregator_endpoint);
    info!("Waiting for the aggregator to produce a pending certificate");

    match attempt!(10, Duration::from_millis(1000), {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => {
                    info!("Aggregator ready");
                    Ok(Some(()))
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
        AttemptResult::Ok(_) => Ok(()),
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(format!(
            "Timeout exhausted for aggregator to be up, no response from `{}`",
            url
        )),
    }
}

async fn assert_node_producing_snapshot(aggregator_endpoint: &str) -> Result<String, String> {
    let url = format!("{}/snapshots", aggregator_endpoint);
    info!("Waiting for the aggregator to produce a snapshot");

    match attempt!(20, Duration::from_millis(1500), {
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
