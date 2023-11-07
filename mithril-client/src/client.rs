use crate::aggregator_client::{AggregatorClient, AggregatorHTTPClient};
use crate::certificate_client::CertificateClient;
use crate::feedback::{FeedbackReceiver, FeedbackSender};
use crate::mithril_stake_distribution_client::MithrilStakeDistributionClient;
use crate::snapshot_client::SnapshotClient;
use crate::snapshot_downloader::{HttpSnapshotDownloader, SnapshotDownloader};
use crate::MithrilResult;
use anyhow::{anyhow, Context};
use mithril_common::api_version::APIVersionProvider;
use mithril_common::certificate_chain::CertificateVerifier;
use mithril_common::crypto_helper::ProtocolGenesisVerificationKey;
use reqwest::Url;
use slog::{o, Logger};
use std::sync::Arc;

/// Structure that aggregate the available clients for each of the mithril types.
///
/// Use the [ClientBuilder] to instantiate it easily.
pub struct Client {
    certificate_client: Arc<CertificateClient>,
    mithril_stake_distribution_client: Arc<MithrilStakeDistributionClient>,
    snapshot_client: Arc<SnapshotClient>,
}

impl Client {
    /// Get the client that fetch and verify mithril certificates.
    pub fn certificate(&self) -> Arc<CertificateClient> {
        self.certificate_client.clone()
    }

    /// Get the client that fetch mithril stake distributions.
    pub fn mithril_stake_distribution(&self) -> Arc<MithrilStakeDistributionClient> {
        self.mithril_stake_distribution_client.clone()
    }

    /// Get the client that fetch and download mithril snapshots.
    pub fn snapshot(&self) -> Arc<SnapshotClient> {
        self.snapshot_client.clone()
    }
}

/// Builder than can be used to create a [Client] easily or with custom dependencies.
pub struct ClientBuilder {
    aggregator_url: Option<String>,
    genesis_verification_key: String,
    aggregator_client: Option<Arc<dyn AggregatorClient>>,
    certificate_verifier: Option<Arc<dyn CertificateVerifier>>,
    snapshot_downloader: Option<Arc<dyn SnapshotDownloader>>,
    logger: Option<Logger>,
    feedback_receivers: Vec<Arc<dyn FeedbackReceiver>>,
}

impl ClientBuilder {
    /// Construct a new [ClientBuilder] that fetch data from the aggregator at the given url
    /// and with the given genesis verification key.
    pub fn aggregator(url: &str, genesis_verification_key: &str) -> ClientBuilder {
        Self {
            aggregator_url: Some(url.to_string()),
            genesis_verification_key: genesis_verification_key.to_string(),
            aggregator_client: None,
            certificate_verifier: None,
            snapshot_downloader: None,
            logger: None,
            feedback_receivers: vec![],
        }
    }

    /// Construct a new [ClientBuilder] without any dependencies set.
    ///
    /// Use [ClientBuilder::aggregator] if you don't need to set a custom [AggregatorClient]
    /// to request data from the aggregator.
    pub fn new(genesis_verification_key: &str) -> ClientBuilder {
        Self {
            aggregator_url: None,
            genesis_verification_key: genesis_verification_key.to_string(),
            aggregator_client: None,
            certificate_verifier: None,
            snapshot_downloader: None,
            logger: None,
            feedback_receivers: vec![],
        }
    }

    /// Returns a [Client] that uses the dependencies provided to the [ClientBuilder].
    ///
    /// For missing dependencies the builder will try to create them using default implementations
    /// if possible.
    pub fn build(self) -> MithrilResult<Client> {
        let logger = match self.logger {
            Some(logger) => logger,
            None => Logger::root(slog::Discard, o!()),
        };

        let feedback_sender = FeedbackSender::new(&self.feedback_receivers);

        let aggregator_client = match self.aggregator_client {
            None => {
                let url = self
                    .aggregator_url
                    .ok_or(anyhow!("No aggregator url found: \
                    You must either provide an aggregator url or your own AggregatorClient implementation"))?;
                let url =
                    Url::parse(&url).with_context(|| format!("Invalid aggregator URL: '{url}'"))?;

                Arc::new(
                    AggregatorHTTPClient::new(
                        url,
                        APIVersionProvider::compute_all_versions_sorted()
                            .with_context(|| "Could not compute aggregator api versions")?,
                        logger.clone(),
                    )
                    .with_context(|| "Building aggregator client failed")?,
                )
            }
            Some(client) => client,
        };

        let snapshot_downloader = match self.snapshot_downloader {
            None => Arc::new(
                HttpSnapshotDownloader::new(feedback_sender.clone(), logger.clone())
                    .with_context(|| "Building snapshot downloader failed")?,
            ),
            Some(snapshot_downloader) => snapshot_downloader,
        };

        let genesis_verification_key =
            ProtocolGenesisVerificationKey::try_from(self.genesis_verification_key)
                .with_context(|| "Invalid genesis verification key")?;

        let certificate_client = match self.certificate_verifier {
            None => Arc::new(CertificateClient::new(
                aggregator_client.clone(),
                genesis_verification_key,
                feedback_sender.clone(),
                logger.clone(),
            )),
            Some(verifier) => Arc::new(CertificateClient::new_with_verifier(
                aggregator_client.clone(),
                genesis_verification_key,
                verifier,
                feedback_sender.clone(),
                logger.clone(),
            )),
        };

        let mithril_stake_distribution_client = Arc::new(MithrilStakeDistributionClient::new(
            aggregator_client.clone(),
        ));
        let snapshot_client = Arc::new(SnapshotClient::new(
            aggregator_client,
            snapshot_downloader,
            feedback_sender,
            logger,
        ));

        Ok(Client {
            certificate_client,
            mithril_stake_distribution_client,
            snapshot_client,
        })
    }

    /// Set the [AggregatorClient] that will be used to request data to the aggregator.
    pub fn with_aggregator_client(
        mut self,
        aggregator_client: Arc<dyn AggregatorClient>,
    ) -> ClientBuilder {
        self.aggregator_client = Some(aggregator_client);
        self
    }

    /// Set the [CertificateVerifier] that will be used to validate certificates.
    pub fn with_certificate_verifier(
        mut self,
        certificate_verifier: Arc<dyn CertificateVerifier>,
    ) -> ClientBuilder {
        self.certificate_verifier = Some(certificate_verifier);
        self
    }

    /// Set the [SnapshotDownloader] that will be used to download snapshots.
    pub fn with_snapshot_downloader(
        mut self,
        snapshot_downloader: Arc<dyn SnapshotDownloader>,
    ) -> ClientBuilder {
        self.snapshot_downloader = Some(snapshot_downloader);
        self
    }

    /// Set the [Logger] to use.
    pub fn with_logger(mut self, logger: Logger) -> Self {
        self.logger = Some(logger);
        self
    }

    /// Add a [feedback receiver][FeedbackReceiver] to receive [events][crate::feedback::MithrilEvent]
    /// for tasks that can have a long duration (ie: snapshot download or a long certificate chain
    /// validation).
    pub fn add_feedback_receiver(mut self, receiver: Arc<dyn FeedbackReceiver>) -> Self {
        self.feedback_receivers.push(receiver);
        self
    }
}
