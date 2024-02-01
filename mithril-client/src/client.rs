use anyhow::{anyhow, Context};
use mithril_common::api_version::APIVersionProvider;
use reqwest::Url;
use slog::{o, Logger};
use std::sync::Arc;

use crate::aggregator_client::{AggregatorClient, AggregatorHTTPClient};
use crate::cardano_transaction_proof::CardanoTransactionProofClient;
use crate::certificate_client::{
    CertificateClient, CertificateVerifier, MithrilCertificateVerifier,
};
use crate::feedback::{FeedbackReceiver, FeedbackSender};
use crate::mithril_stake_distribution_client::MithrilStakeDistributionClient;
use crate::snapshot_client::SnapshotClient;
#[cfg(feature = "fs")]
use crate::snapshot_downloader::{HttpSnapshotDownloader, SnapshotDownloader};
use crate::MithrilResult;

/// Structure that aggregates the available clients for each of the Mithril types of certified data.
///
/// Use the [ClientBuilder] to instantiate it easily.
pub struct Client {
    cardano_transaction_proof_client: Arc<CardanoTransactionProofClient>,
    certificate_client: Arc<CertificateClient>,
    mithril_stake_distribution_client: Arc<MithrilStakeDistributionClient>,
    snapshot_client: Arc<SnapshotClient>,
}

impl Client {
    /// Get the client that fetches and verifies Mithril certificates.
    pub fn certificate(&self) -> Arc<CertificateClient> {
        self.certificate_client.clone()
    }

    /// Get the client that fetches Mithril stake distributions.
    pub fn mithril_stake_distribution(&self) -> Arc<MithrilStakeDistributionClient> {
        self.mithril_stake_distribution_client.clone()
    }

    /// Get the client that fetches and downloads Mithril snapshots.
    pub fn snapshot(&self) -> Arc<SnapshotClient> {
        self.snapshot_client.clone()
    }
}

/// Builder than can be used to create a [Client] easily or with custom dependencies.
pub struct ClientBuilder {
    aggregator_endpoint: Option<String>,
    genesis_verification_key: String,
    aggregator_client: Option<Arc<dyn AggregatorClient>>,
    certificate_verifier: Option<Arc<dyn CertificateVerifier>>,
    #[cfg(feature = "fs")]
    snapshot_downloader: Option<Arc<dyn SnapshotDownloader>>,
    logger: Option<Logger>,
    feedback_receivers: Vec<Arc<dyn FeedbackReceiver>>,
}

impl ClientBuilder {
    /// Constructs a new `ClientBuilder` that fetches data from the aggregator at the given
    /// endpoint and with the given genesis verification key.
    pub fn aggregator(endpoint: &str, genesis_verification_key: &str) -> ClientBuilder {
        Self {
            aggregator_endpoint: Some(endpoint.to_string()),
            genesis_verification_key: genesis_verification_key.to_string(),
            aggregator_client: None,
            certificate_verifier: None,
            #[cfg(feature = "fs")]
            snapshot_downloader: None,
            logger: None,
            feedback_receivers: vec![],
        }
    }

    /// Constructs a new `ClientBuilder` without any dependency set.
    ///
    /// Use [ClientBuilder::aggregator] if you don't need to set a custom [AggregatorClient]
    /// to request data from the aggregator.
    pub fn new(genesis_verification_key: &str) -> ClientBuilder {
        Self {
            aggregator_endpoint: None,
            genesis_verification_key: genesis_verification_key.to_string(),
            aggregator_client: None,
            certificate_verifier: None,
            #[cfg(feature = "fs")]
            snapshot_downloader: None,
            logger: None,
            feedback_receivers: vec![],
        }
    }

    /// Returns a `Client` that uses the dependencies provided to this `ClientBuilder`.
    ///
    /// The builder will try to create the missing dependencies using default implementations
    /// if possible.
    pub fn build(self) -> MithrilResult<Client> {
        let logger = self
            .logger
            .unwrap_or_else(|| Logger::root(slog::Discard, o!()));

        let feedback_sender = FeedbackSender::new(&self.feedback_receivers);

        let aggregator_client = match self.aggregator_client {
            None => {
                let endpoint = self
                    .aggregator_endpoint
                    .ok_or(anyhow!("No aggregator endpoint set: \
                    You must either provide an aggregator endpoint or your own AggregatorClient implementation"))?;
                let endpoint_url = Url::parse(&endpoint)
                    .with_context(|| format!("Invalid aggregator endpoint, it must be a correctly formed url: '{endpoint}'"))?;

                Arc::new(
                    AggregatorHTTPClient::new(
                        endpoint_url,
                        APIVersionProvider::compute_all_versions_sorted()
                            .with_context(|| "Could not compute aggregator api versions")?,
                        logger.clone(),
                    )
                    .with_context(|| "Building aggregator client failed")?,
                )
            }
            Some(client) => client,
        };

        #[cfg(feature = "fs")]
        let snapshot_downloader = match self.snapshot_downloader {
            None => Arc::new(
                HttpSnapshotDownloader::new(feedback_sender.clone(), logger.clone())
                    .with_context(|| "Building snapshot downloader failed")?,
            ),
            Some(snapshot_downloader) => snapshot_downloader,
        };

        let cardano_transaction_proof_client = Arc::new(CardanoTransactionProofClient::new(
            aggregator_client.clone(),
        ));

        let certificate_verifier = match self.certificate_verifier {
            None => Arc::new(
                MithrilCertificateVerifier::new(
                    aggregator_client.clone(),
                    &self.genesis_verification_key,
                    feedback_sender.clone(),
                    logger.clone(),
                )
                .with_context(|| "Building certificate verifier failed")?,
            ),
            Some(verifier) => verifier,
        };
        let certificate_client = Arc::new(CertificateClient::new(
            aggregator_client.clone(),
            certificate_verifier,
            logger.clone(),
        ));

        let mithril_stake_distribution_client = Arc::new(MithrilStakeDistributionClient::new(
            aggregator_client.clone(),
        ));
        let snapshot_client = Arc::new(SnapshotClient::new(
            aggregator_client,
            #[cfg(feature = "fs")]
            snapshot_downloader,
            #[cfg(feature = "fs")]
            feedback_sender,
            #[cfg(feature = "fs")]
            logger,
        ));

        Ok(Client {
            cardano_transaction_proof_client,
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

    cfg_fs! {
    /// Set the [SnapshotDownloader] that will be used to download snapshots.
    pub fn with_snapshot_downloader(
        mut self,
        snapshot_downloader: Arc<dyn SnapshotDownloader>,
    ) -> ClientBuilder {
        self.snapshot_downloader = Some(snapshot_downloader);
        self
    }
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
