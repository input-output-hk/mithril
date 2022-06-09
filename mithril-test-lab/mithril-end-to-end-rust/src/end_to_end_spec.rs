use crate::MithrilInfrastructure;
use async_recursion::async_recursion;
use mithril_common::entities::Snapshot;
use reqwest::StatusCode;
use slog_scope::info;
use std::error::Error;
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

        wait_for_pending_certificate(&aggregator_endpoint).await?;
        let digest = assert_node_producing_snapshot(&aggregator_endpoint).await?;
        let _certificate_hash =
            assert_signer_is_signing_snapshot(&aggregator_endpoint, &digest).await?;
        // await aggregator.assertIsCreatingCertificate()?;
        //
        // await assertClientCanVerifySnapshot()?;

        Ok(())
    }

    pub async fn dump_logs_of_failed_processes(&mut self) -> Result<(), String> {
        self.infrastructure
            .aggregator_mut()
            .dump_logs_if_crashed()
            .await?;
        self.infrastructure
            .signer_mut()
            .dump_logs_if_crashed()
            .await?;

        Ok(())
    }
}

async fn wait_for_pending_certificate(aggregator_endpoint: &str) -> Result<(), String> {
    let url = format!("{}/certificate-pending", aggregator_endpoint);

    for _ in 1..10 {
        let response = reqwest::get(url.clone()).await;

        if let Ok(response) = response {
            match response.status() {
                StatusCode::OK => {
                    info!("Aggregator ready");
                    return Ok(());
                }
                s if s.is_server_error() => {
                    return Err(format!(
                        "server error while waiting for the Aggregator, http code: {}",
                        s
                    ));
                }
                _ => {}
            }
        }

        tokio::time::sleep(Duration::from_millis(100)).await;
    }

    Err(format!(
        "Timeout exhausted for aggregator to be up, no response from `{}`",
        url
    ))
}

async fn assert_node_producing_snapshot(aggregator_endpoint: &str) -> Result<String, String> {
    #[async_recursion]
    async fn get_last_digest(
        remaining_attempts: i32,
        aggregator_endpoint: &str,
    ) -> Result<String, String> {
        let url = format!("{}/snapshots", aggregator_endpoint);
        if remaining_attempts == 0 {
            return Err(format!(
                "Timeout exhausted assert_node_producing_snapshot, no response from `{}`",
                url
            ));
        }

        let res = match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Vec<Snapshot>>().await.as_deref() {
                    Ok([snapshot, ..]) => Ok(Some(snapshot.digest.clone())),
                    Ok(&[]) => Ok(None),
                    Err(err) => Err(format!("invalid snapshot body : {}", err,)),
                },
                s => Err(format!("unexpected status code from Aggregator: {}", s)),
            },
            Err(err) => Err(format!(": {}", err)),
        };

        match res {
            Ok(Some(digest)) => {
                info!("aggregator produced a snapshot"; "digest" => &digest);
                Ok(digest)
            }
            Ok(None) => {
                tokio::time::sleep(Duration::from_millis(1000)).await;
                get_last_digest(remaining_attempts - 1, aggregator_endpoint).await
            }
            Err(e) => Err(e),
        }
    }

    get_last_digest(10, aggregator_endpoint).await
}

async fn assert_signer_is_signing_snapshot(
    aggregator_endpoint: &str,
    digest: &str,
) -> Result<String, String> {
    #[async_recursion]
    async fn get_snapshot_from_aggregator(
        remaining_attempts: i32,
        aggregator_endpoint: &str,
        digest: &str,
    ) -> Result<String, String> {
        let url = format!("{}/snapshot/{}", aggregator_endpoint, digest);
        if remaining_attempts == 0 {
            return Err(format!(
                "Timeout exhausted assert_signer_is_signing_snapshot, no response from `{}`",
                url
            ));
        }

        let res = match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Snapshot>().await {
                    Ok(snapshot) => Ok(Some(snapshot)),
                    Err(err) => Err(format!("invalid snapshot body : {}", err,)),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(format!("unexpected status code from Aggregator: {}", s)),
            },
            Err(err) => Err(format!(": {}", err)),
        };

        match res {
            Ok(Some(snapshot)) => {
                // todo: assert that the snapshot is really signed
                info!("signer signed a snapshot"; "certificate_hash" => &snapshot.certificate_hash);
                Ok(snapshot.certificate_hash)
            }
            Ok(None) => {
                tokio::time::sleep(Duration::from_millis(1000)).await;
                get_snapshot_from_aggregator(remaining_attempts - 1, aggregator_endpoint, digest)
                    .await
            }
            Err(e) => Err(e),
        }
    }

    get_snapshot_from_aggregator(10, aggregator_endpoint, digest).await
}
