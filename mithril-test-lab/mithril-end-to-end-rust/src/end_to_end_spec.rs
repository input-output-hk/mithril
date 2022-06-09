use crate::MithrilInfrastructure;
use reqwest::StatusCode;
use slog_scope::{info, warn};
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
        let _digest = assert_node_producing_snapshot(&aggregator_endpoint).await?;
        // let certificate_hash = await signer.assertIsSigningSnapshot()?;
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
        "Timeout waiting for aggregator to be up, no response from `{}`",
        url
    ))
}

async fn assert_node_producing_snapshot(aggregator_endpoint: &str) -> Result<String, String> {
    tokio::time::sleep(Duration::from_secs(4)).await;
    warn!("todo: assert_node_producing_snapshot");

    Ok("digest".to_string())
}
