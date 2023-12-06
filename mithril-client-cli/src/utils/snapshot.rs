use anyhow::{anyhow, Context};
use async_trait::async_trait;
use futures::Future;
use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget, ProgressState, ProgressStyle};
use std::{fmt::Write, time::Duration};
use tokio::sync::RwLock;

use super::{DownloadProgressReporter, ProgressOutputType, SnapshotUnpackerError};
use crate::aggregator_client::{AggregatorClient, AggregatorHTTPClient};

use mithril_client::{
    feedback::{FeedbackReceiver, MithrilEvent},
    Client, MithrilResult,
};
use mithril_common::{
    api_version::APIVersionProvider, messages::SnapshotMessage, StdError, StdResult,
};

/// Utility functions for to the Snapshot commands
pub struct SnapshotUtils;

impl SnapshotUtils {
    /// Return latest snpashot digest if `latest` is specified as digest
    pub async fn expand_eventual_snapshot_alias(
        client: &Client,
        snapshot_id: &str,
    ) -> StdResult<String> {
        if snapshot_id.to_lowercase() == "latest" {
            let last_snapshot = client.snapshot().list().await.with_context(|| {
                "Can not get the list of snapshots while retrieving the latest snapshot digest"
            })?;
            let last_snapshot = last_snapshot.first().ok_or_else(|| {
                anyhow!(
                    "Snapshot not found for digest: '{}'",
                    snapshot_id.to_string()
                )
            })?;
            Ok(last_snapshot.digest.to_owned())
        } else {
            Ok(snapshot_id.to_owned())
        }
    }

    /// Handle the error return by `check_prerequisites`
    pub fn check_disk_space_error(error: StdError) -> StdResult<String> {
        if let Some(SnapshotUnpackerError::NotEnoughSpace {
            left_space: _,
            pathdir: _,
            archive_size: _,
        }) = error.downcast_ref::<SnapshotUnpackerError>()
        {
            Ok(format!("Warning: {}", error))
        } else {
            Err(error)
        }
    }

    /// Display a spinner to while waiting for the result of a future
    pub async fn wait_spinner<T>(
        progress_bar: &MultiProgress,
        future: impl Future<Output = MithrilResult<T>>,
    ) -> MithrilResult<T> {
        let pb = progress_bar.add(ProgressBar::new_spinner());
        let spinner = async move {
            loop {
                pb.tick();
                tokio::time::sleep(Duration::from_millis(50)).await;
            }
        };

        tokio::select! {
            _ = spinner => Err(anyhow!("timeout")),
            res = future => res,
        }
    }

    /// Increments Aggregator's download statistics
    pub async fn add_statistics(
        aggregator_endpoint: &str,
        snapshot: &SnapshotMessage,
    ) -> StdResult<()> {
        let url = "statistics/snapshot";
        let json = serde_json::to_string(&snapshot)?;
        let http_client = AggregatorHTTPClient::new(
            aggregator_endpoint,
            APIVersionProvider::compute_all_versions_sorted()?,
        );
        let _response = http_client.post_content(url, &json).await?;

        Ok(())
    }
}

/// Custom [FeedbackReceiver] for Snapshot to handle events sent
/// by the `mithril-client` library
pub struct IndicatifFeedbackReceiver {
    download_progress_reporter: RwLock<Option<DownloadProgressReporter>>,
    certificate_validation_pb: RwLock<Option<ProgressBar>>,
    output_type: ProgressOutputType,
}

impl IndicatifFeedbackReceiver {
    /// [IndicatifFeedbackReceiver] constructor
    pub fn new(output_type: ProgressOutputType) -> Self {
        Self {
            download_progress_reporter: RwLock::new(None),
            certificate_validation_pb: RwLock::new(None),
            output_type,
        }
    }
}

#[async_trait]
impl FeedbackReceiver for IndicatifFeedbackReceiver {
    async fn handle_event(&self, event: MithrilEvent) {
        match event {
            MithrilEvent::SnapshotDownloadStarted {
                digest: _,
                download_id: _,
                size,
            } => {
                let pb = if self.output_type == ProgressOutputType::TTY {
                    ProgressBar::new(size)
                } else {
                    ProgressBar::with_draw_target(Some(size), ProgressDrawTarget::hidden())
                };
                pb.set_style(ProgressStyle::with_template("{spinner:.green} [{elapsed_precise}] [{wide_bar:.cyan/blue}] {bytes}/{total_bytes} ({eta})")
                    .unwrap()
                    .with_key("eta", |state : &ProgressState, w: &mut dyn Write| write!(w, "{:.1}s", state.eta().as_secs_f64()).unwrap())
                    .progress_chars("#>-"));
                let mut download_progress_reporter = self.download_progress_reporter.write().await;
                *download_progress_reporter =
                    Some(DownloadProgressReporter::new(pb, self.output_type));
            }
            MithrilEvent::SnapshotDownloadProgress {
                download_id: _,
                downloaded_bytes,
                size: _,
            } => {
                let download_progress_reporter = self.download_progress_reporter.read().await;
                if let Some(progress_reporter) = download_progress_reporter.as_ref() {
                    progress_reporter.report(downloaded_bytes);
                }
            }
            MithrilEvent::SnapshotDownloadCompleted { download_id: _ } => {
                let mut download_progress_reporter = self.download_progress_reporter.write().await;
                if let Some(progress_reporter) = download_progress_reporter.as_ref() {
                    progress_reporter.finish("Snapshot download completed");
                }
                *download_progress_reporter = None;
            }
            MithrilEvent::CertificateChainValidationStarted {
                certificate_chain_validation_id: _,
            } => {
                let pb = if self.output_type == ProgressOutputType::TTY {
                    ProgressBar::new_spinner()
                } else {
                    ProgressBar::hidden()
                };
                let mut certificate_validation_pb = self.certificate_validation_pb.write().await;
                *certificate_validation_pb = Some(pb);
            }
            MithrilEvent::CertificateValidated {
                certificate_chain_validation_id: _,
                certificate_hash,
            } => {
                let certificate_validation_pb = self.certificate_validation_pb.read().await;
                if let Some(progress_bar) = certificate_validation_pb.as_ref() {
                    progress_bar.set_message(format!("Certificate '{certificate_hash}' is valid"));
                    progress_bar.inc(1);
                }
            }
            MithrilEvent::CertificateChainValidated {
                certificate_chain_validation_id: _,
            } => {
                let mut certificate_validation_pb = self.certificate_validation_pb.write().await;
                if let Some(progress_bar) = certificate_validation_pb.as_ref() {
                    progress_bar.finish_with_message("Certificate chain validated");
                }
                *certificate_validation_pb = None;
            }
        }
    }
}
