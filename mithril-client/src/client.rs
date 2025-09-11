use anyhow::{Context, anyhow};
#[cfg(feature = "fs")]
use chrono::Utc;
use reqwest::Url;
use serde::{Deserialize, Serialize};
use slog::{Logger, o};
use std::collections::HashMap;
use std::sync::Arc;

use mithril_common::api_version::APIVersionProvider;
use mithril_common::{MITHRIL_CLIENT_TYPE_HEADER, MITHRIL_ORIGIN_TAG_HEADER};

use crate::MithrilResult;
use crate::aggregator_client::{AggregatorClient, AggregatorHTTPClient};
use crate::cardano_database_client::CardanoDatabaseClient;
use crate::cardano_stake_distribution_client::CardanoStakeDistributionClient;
use crate::cardano_transaction_client::CardanoTransactionClient;
#[cfg(feature = "unstable")]
use crate::certificate_client::CertificateVerifierCache;
use crate::certificate_client::{
    CertificateClient, CertificateVerifier, MithrilCertificateVerifier,
};
use crate::era::{AggregatorHttpEraFetcher, EraFetcher, MithrilEraClient};
use crate::feedback::{FeedbackReceiver, FeedbackSender};
#[cfg(feature = "fs")]
use crate::file_downloader::{
    FileDownloadRetryPolicy, FileDownloader, HttpFileDownloader, RetryDownloader,
};
use crate::mithril_stake_distribution_client::MithrilStakeDistributionClient;
use crate::snapshot_client::SnapshotClient;
#[cfg(feature = "fs")]
use crate::utils::AncillaryVerifier;
#[cfg(feature = "fs")]
use crate::utils::TimestampTempDirectoryProvider;

const DEFAULT_CLIENT_TYPE: &str = "LIBRARY";

#[cfg(target_family = "wasm")]
const fn one_week_in_seconds() -> u32 {
    604800
}

/// Options that can be used to configure the client.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ClientOptions {
    /// HTTP headers to include in the client requests.
    pub http_headers: Option<HashMap<String, String>>,

    /// Tag to retrieve the origin of the client requests.
    #[cfg(target_family = "wasm")]
    #[cfg_attr(target_family = "wasm", serde(default))]
    pub origin_tag: Option<String>,

    /// Whether to enable unstable features in the WASM client.
    #[cfg(target_family = "wasm")]
    #[cfg_attr(target_family = "wasm", serde(default))]
    pub unstable: bool,

    /// Whether to enable certificate chain verification caching in the WASM client.
    ///
    /// `unstable` must be set to `true` for this option to have any effect.
    ///
    /// DANGER: This feature is highly experimental and insecure, and it must not be used in production
    #[cfg(target_family = "wasm")]
    #[cfg_attr(target_family = "wasm", serde(default))]
    pub enable_certificate_chain_verification_cache: bool,

    /// Duration in seconds of certificate chain verification cache in the WASM client.
    ///
    /// Default to one week (604800 seconds).
    ///
    /// `enable_certificate_chain_verification_cache` and `unstable` must both be set to `true`
    /// for this option to have any effect.
    #[cfg(target_family = "wasm")]
    #[cfg_attr(target_family = "wasm", serde(default = "one_week_in_seconds"))]
    pub certificate_chain_verification_cache_duration_in_seconds: u32,
}

impl ClientOptions {
    /// Instantiate a new [ClientOptions].
    pub fn new(http_headers: Option<HashMap<String, String>>) -> Self {
        Self {
            http_headers,
            #[cfg(target_family = "wasm")]
            origin_tag: None,
            #[cfg(target_family = "wasm")]
            unstable: false,
            #[cfg(target_family = "wasm")]
            enable_certificate_chain_verification_cache: false,
            #[cfg(target_family = "wasm")]
            certificate_chain_verification_cache_duration_in_seconds: one_week_in_seconds(),
        }
    }

    /// Enable unstable features in the WASM client.
    #[cfg(target_family = "wasm")]
    pub fn with_unstable_features(self, unstable: bool) -> Self {
        Self { unstable, ..self }
    }
}

/// Structure that aggregates the available clients for each of the Mithril types of certified data.
///
/// Use the [ClientBuilder] to instantiate it easily.
#[derive(Clone)]
pub struct Client {
    certificate_client: Arc<CertificateClient>,
    mithril_stake_distribution_client: Arc<MithrilStakeDistributionClient>,
    snapshot_client: Arc<SnapshotClient>,
    cardano_database_client: Arc<CardanoDatabaseClient>,
    cardano_transaction_client: Arc<CardanoTransactionClient>,
    cardano_stake_distribution_client: Arc<CardanoStakeDistributionClient>,
    mithril_era_client: Arc<MithrilEraClient>,
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

    #[deprecated(since = "0.11.9", note = "supersede by `cardano_database`")]
    /// Get the client that fetches and downloads Mithril snapshots.
    pub fn snapshot(&self) -> Arc<SnapshotClient> {
        self.cardano_database()
    }

    /// Get the client that fetches and downloads Mithril snapshots.
    pub fn cardano_database(&self) -> Arc<SnapshotClient> {
        self.snapshot_client.clone()
    }

    /// Get the client that fetches and downloads Cardano database snapshots.
    pub fn cardano_database_v2(&self) -> Arc<CardanoDatabaseClient> {
        self.cardano_database_client.clone()
    }

    /// Get the client that fetches and verifies Mithril Cardano transaction proof.
    pub fn cardano_transaction(&self) -> Arc<CardanoTransactionClient> {
        self.cardano_transaction_client.clone()
    }

    /// Get the client that fetches Cardano stake distributions.
    pub fn cardano_stake_distribution(&self) -> Arc<CardanoStakeDistributionClient> {
        self.cardano_stake_distribution_client.clone()
    }

    /// Get the client that fetches the current Mithril era.
    pub fn mithril_era_client(&self) -> Arc<MithrilEraClient> {
        self.mithril_era_client.clone()
    }
}

/// Builder than can be used to create a [Client] easily or with custom dependencies.
pub struct ClientBuilder {
    aggregator_endpoint: Option<String>,
    genesis_verification_key: String,
    origin_tag: Option<String>,
    client_type: Option<String>,
    #[cfg(feature = "fs")]
    ancillary_verification_key: Option<String>,
    aggregator_client: Option<Arc<dyn AggregatorClient>>,
    certificate_verifier: Option<Arc<dyn CertificateVerifier>>,
    #[cfg(feature = "fs")]
    http_file_downloader: Option<Arc<dyn FileDownloader>>,
    #[cfg(feature = "unstable")]
    certificate_verifier_cache: Option<Arc<dyn CertificateVerifierCache>>,
    era_fetcher: Option<Arc<dyn EraFetcher>>,
    logger: Option<Logger>,
    feedback_receivers: Vec<Arc<dyn FeedbackReceiver>>,
    options: ClientOptions,
}

impl ClientBuilder {
    /// Constructs a new `ClientBuilder` that fetches data from the aggregator at the given
    /// endpoint and with the given genesis verification key.
    pub fn aggregator(endpoint: &str, genesis_verification_key: &str) -> ClientBuilder {
        Self {
            aggregator_endpoint: Some(endpoint.to_string()),
            genesis_verification_key: genesis_verification_key.to_string(),
            origin_tag: None,
            client_type: None,
            #[cfg(feature = "fs")]
            ancillary_verification_key: None,
            aggregator_client: None,
            certificate_verifier: None,
            #[cfg(feature = "fs")]
            http_file_downloader: None,
            #[cfg(feature = "unstable")]
            certificate_verifier_cache: None,
            era_fetcher: None,
            logger: None,
            feedback_receivers: vec![],
            options: ClientOptions::default(),
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
            origin_tag: None,
            client_type: None,
            #[cfg(feature = "fs")]
            ancillary_verification_key: None,
            aggregator_client: None,
            certificate_verifier: None,
            #[cfg(feature = "fs")]
            http_file_downloader: None,
            #[cfg(feature = "unstable")]
            certificate_verifier_cache: None,
            era_fetcher: None,
            logger: None,
            feedback_receivers: vec![],
            options: ClientOptions::default(),
        }
    }

    /// Returns a `Client` that uses the dependencies provided to this `ClientBuilder`.
    ///
    /// The builder will try to create the missing dependencies using default implementations
    /// if possible.
    pub fn build(self) -> MithrilResult<Client> {
        let logger = self
            .logger
            .clone()
            .unwrap_or_else(|| Logger::root(slog::Discard, o!()));

        let feedback_sender = FeedbackSender::new(&self.feedback_receivers);

        let aggregator_client = match self.aggregator_client {
            None => Arc::new(self.build_aggregator_client(logger.clone())?),
            Some(client) => client,
        };

        let mithril_era_client = match self.era_fetcher {
            None => Arc::new(MithrilEraClient::new(Arc::new(
                AggregatorHttpEraFetcher::new(aggregator_client.clone()),
            ))),
            Some(era_fetcher) => Arc::new(MithrilEraClient::new(era_fetcher)),
        };

        let certificate_verifier = match self.certificate_verifier {
            None => Arc::new(
                MithrilCertificateVerifier::new(
                    aggregator_client.clone(),
                    &self.genesis_verification_key,
                    feedback_sender.clone(),
                    #[cfg(feature = "unstable")]
                    self.certificate_verifier_cache,
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

        #[cfg(feature = "fs")]
        let http_file_downloader = match self.http_file_downloader {
            None => Arc::new(RetryDownloader::new(
                Arc::new(
                    HttpFileDownloader::new(feedback_sender.clone(), logger.clone())
                        .with_context(|| "Building http file downloader failed")?,
                ),
                FileDownloadRetryPolicy::default(),
            )),
            Some(http_file_downloader) => http_file_downloader,
        };

        #[cfg(feature = "fs")]
        let ancillary_verifier = match self.ancillary_verification_key {
            None => None,
            Some(verification_key) => Some(Arc::new(AncillaryVerifier::new(
                verification_key
                    .try_into()
                    .with_context(|| "Building ancillary verifier failed")?,
            ))),
        };

        let snapshot_client = Arc::new(SnapshotClient::new(
            aggregator_client.clone(),
            #[cfg(feature = "fs")]
            http_file_downloader.clone(),
            #[cfg(feature = "fs")]
            ancillary_verifier.clone(),
            #[cfg(feature = "fs")]
            feedback_sender.clone(),
            #[cfg(feature = "fs")]
            logger.clone(),
        ));

        let cardano_database_client = Arc::new(CardanoDatabaseClient::new(
            aggregator_client.clone(),
            #[cfg(feature = "fs")]
            http_file_downloader,
            #[cfg(feature = "fs")]
            ancillary_verifier,
            #[cfg(feature = "fs")]
            feedback_sender,
            #[cfg(feature = "fs")]
            Arc::new(TimestampTempDirectoryProvider::new(&format!(
                "{}",
                Utc::now().timestamp_micros()
            ))),
            #[cfg(feature = "fs")]
            logger,
        ));

        let cardano_transaction_client =
            Arc::new(CardanoTransactionClient::new(aggregator_client.clone()));

        let cardano_stake_distribution_client =
            Arc::new(CardanoStakeDistributionClient::new(aggregator_client));

        Ok(Client {
            certificate_client,
            mithril_stake_distribution_client,
            snapshot_client,
            cardano_database_client,
            cardano_transaction_client,
            cardano_stake_distribution_client,
            mithril_era_client,
        })
    }

    fn build_aggregator_client(
        &self,
        logger: Logger,
    ) -> Result<AggregatorHTTPClient, anyhow::Error> {
        let endpoint = self
            .aggregator_endpoint.as_ref()
            .ok_or(anyhow!("No aggregator endpoint set: \
                    You must either provide an aggregator endpoint or your own AggregatorClient implementation"))?;
        let endpoint_url = Url::parse(endpoint).with_context(|| {
            format!("Invalid aggregator endpoint, it must be a correctly formed url: '{endpoint}'")
        })?;

        let headers = self.compute_http_headers();

        AggregatorHTTPClient::new(
            endpoint_url,
            APIVersionProvider::compute_all_versions_sorted(),
            logger,
            Some(headers),
        )
        .with_context(|| "Building aggregator client failed")
    }

    fn compute_http_headers(&self) -> HashMap<String, String> {
        let mut headers = self.options.http_headers.clone().unwrap_or_default();
        if let Some(origin_tag) = self.origin_tag.clone() {
            headers.insert(MITHRIL_ORIGIN_TAG_HEADER.to_string(), origin_tag);
        }
        if let Some(client_type) = self.client_type.clone() {
            headers.insert(MITHRIL_CLIENT_TYPE_HEADER.to_string(), client_type);
        } else if !headers.contains_key(MITHRIL_CLIENT_TYPE_HEADER) {
            headers.insert(
                MITHRIL_CLIENT_TYPE_HEADER.to_string(),
                DEFAULT_CLIENT_TYPE.to_string(),
            );
        }

        headers
    }

    /// Set the [AggregatorClient] that will be used to request data to the aggregator.
    pub fn with_aggregator_client(
        mut self,
        aggregator_client: Arc<dyn AggregatorClient>,
    ) -> ClientBuilder {
        self.aggregator_client = Some(aggregator_client);
        self
    }

    /// Sets the [EraFetcher] that will be used by the client to retrieve the current Mithril era.
    pub fn with_era_fetcher(mut self, era_fetcher: Arc<dyn EraFetcher>) -> ClientBuilder {
        self.era_fetcher = Some(era_fetcher);
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

    cfg_unstable! {
        /// Set the [CertificateVerifierCache] that will be used to cache certificate validation results.
        ///
        /// Passing a `None` value will disable the cache if any was previously set.
        pub fn with_certificate_verifier_cache(
            mut self,
            certificate_verifier_cache: Option<Arc<dyn CertificateVerifierCache>>,
        ) -> ClientBuilder {
            self.certificate_verifier_cache = certificate_verifier_cache;
            self
        }
    }

    cfg_fs! {
        /// Set the [FileDownloader] that will be used to download artifacts with HTTP.
        pub fn with_http_file_downloader(
            mut self,
            http_file_downloader: Arc<dyn FileDownloader>,
        ) -> ClientBuilder {
            self.http_file_downloader = Some(http_file_downloader);
            self
        }

        /// Set the ancillary verification key to use when verifying the downloaded ancillary files.
        pub fn set_ancillary_verification_key<T: Into<Option<String>>>(
            mut self,
            ancillary_verification_key: T,
        ) -> ClientBuilder {
            self.ancillary_verification_key = ancillary_verification_key.into();
            self
        }
    }

    /// Set the [Logger] to use.
    pub fn with_logger(mut self, logger: Logger) -> Self {
        self.logger = Some(logger);
        self
    }

    /// Set the origin tag.
    pub fn with_origin_tag(mut self, origin_tag: Option<String>) -> Self {
        self.origin_tag = origin_tag;
        self
    }

    /// Set the client type.
    pub fn with_client_type(mut self, client_type: Option<String>) -> Self {
        self.client_type = client_type;
        self
    }

    /// Sets the options to be used by the client.
    pub fn with_options(mut self, options: ClientOptions) -> Self {
        self.options = options;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn default_headers() -> HashMap<String, String> {
        HashMap::from([(
            MITHRIL_CLIENT_TYPE_HEADER.to_string(),
            DEFAULT_CLIENT_TYPE.to_string(),
        )])
    }

    #[tokio::test]
    async fn compute_http_headers_returns_options_http_headers() {
        let http_headers = default_headers();
        let client_builder = ClientBuilder::new("").with_options(ClientOptions {
            http_headers: Some(http_headers.clone()),
        });

        let computed_headers = client_builder.compute_http_headers();

        assert_eq!(computed_headers, http_headers);
    }

    #[tokio::test]
    async fn compute_http_headers_with_origin_tag_returns_options_http_headers_with_origin_tag() {
        let http_headers = default_headers();
        let client_builder = ClientBuilder::new("")
            .with_options(ClientOptions {
                http_headers: Some(http_headers.clone()),
            })
            .with_origin_tag(Some("CLIENT_TAG".to_string()));
        let mut expected_headers = http_headers.clone();
        expected_headers.insert(
            MITHRIL_ORIGIN_TAG_HEADER.to_string(),
            "CLIENT_TAG".to_string(),
        );

        let computed_headers = client_builder.compute_http_headers();
        assert_eq!(computed_headers, expected_headers);
    }

    #[tokio::test]
    async fn test_with_origin_tag_not_overwrite_other_client_options_attributes() {
        let builder = ClientBuilder::new("")
            .with_options(ClientOptions { http_headers: None })
            .with_origin_tag(Some("TEST".to_string()));
        assert_eq!(None, builder.options.http_headers);
        assert_eq!(Some("TEST".to_string()), builder.origin_tag);

        let http_headers = HashMap::from([("Key".to_string(), "Value".to_string())]);
        let builder = ClientBuilder::new("")
            .with_options(ClientOptions {
                http_headers: Some(http_headers.clone()),
            })
            .with_origin_tag(Some("TEST".to_string()));
        assert_eq!(Some(http_headers), builder.options.http_headers);
        assert_eq!(Some("TEST".to_string()), builder.origin_tag);
    }

    #[tokio::test]
    async fn test_with_origin_tag_can_be_unset() {
        let http_headers = HashMap::from([("Key".to_string(), "Value".to_string())]);
        let client_options = ClientOptions {
            http_headers: Some(http_headers.clone()),
        };
        let builder = ClientBuilder::new("")
            .with_options(client_options)
            .with_origin_tag(None);

        assert_eq!(Some(http_headers), builder.options.http_headers);
        assert_eq!(None, builder.origin_tag);
    }

    #[tokio::test]
    async fn compute_http_headers_with_client_type_returns_options_http_headers_with_client_type() {
        let http_headers = HashMap::from([("Key".to_string(), "Value".to_string())]);
        let client_builder = ClientBuilder::new("")
            .with_options(ClientOptions {
                http_headers: Some(http_headers.clone()),
            })
            .with_client_type(Some("CLIENT_TYPE".to_string()));

        let computed_headers = client_builder.compute_http_headers();

        assert_eq!(
            computed_headers,
            HashMap::from([
                ("Key".to_string(), "Value".to_string()),
                (
                    MITHRIL_CLIENT_TYPE_HEADER.to_string(),
                    "CLIENT_TYPE".to_string()
                )
            ])
        );
    }

    #[tokio::test]
    async fn compute_http_headers_with_options_containing_client_type_returns_client_type() {
        let http_headers = HashMap::from([(
            MITHRIL_CLIENT_TYPE_HEADER.to_string(),
            "client type from options".to_string(),
        )]);
        let client_builder = ClientBuilder::new("").with_options(ClientOptions {
            http_headers: Some(http_headers.clone()),
        });

        let computed_headers = client_builder.compute_http_headers();

        assert_eq!(computed_headers, http_headers);
    }

    #[tokio::test]
    async fn test_with_client_type_not_overwrite_other_client_options_attributes() {
        let builder = ClientBuilder::new("")
            .with_options(ClientOptions { http_headers: None })
            .with_client_type(Some("TEST".to_string()));
        assert_eq!(None, builder.options.http_headers);
        assert_eq!(Some("TEST".to_string()), builder.client_type);

        let http_headers = HashMap::from([("Key".to_string(), "Value".to_string())]);
        let builder = ClientBuilder::new("")
            .with_options(ClientOptions {
                http_headers: Some(http_headers.clone()),
            })
            .with_client_type(Some("TEST".to_string()));
        assert_eq!(Some(http_headers), builder.options.http_headers);
        assert_eq!(Some("TEST".to_string()), builder.client_type);
    }

    #[tokio::test]
    async fn test_given_a_none_client_type_compute_http_headers_will_set_client_type_to_default_value()
     {
        let builder_without_client_type = ClientBuilder::new("");
        let computed_headers = builder_without_client_type.compute_http_headers();

        assert_eq!(
            computed_headers,
            HashMap::from([(
                MITHRIL_CLIENT_TYPE_HEADER.to_string(),
                DEFAULT_CLIENT_TYPE.to_string()
            )])
        );

        let builder_with_none_client_type = ClientBuilder::new("").with_client_type(None);
        let computed_headers = builder_with_none_client_type.compute_http_headers();

        assert_eq!(
            computed_headers,
            HashMap::from([(
                MITHRIL_CLIENT_TYPE_HEADER.to_string(),
                DEFAULT_CLIENT_TYPE.to_string()
            )])
        );
    }

    #[tokio::test]
    async fn test_compute_http_headers_will_compute_client_type_header_from_struct_attribute_over_options()
     {
        let http_headers = HashMap::from([(
            MITHRIL_CLIENT_TYPE_HEADER.to_string(),
            "client type from options".to_string(),
        )]);
        let client_builder = ClientBuilder::new("")
            .with_options(ClientOptions {
                http_headers: Some(http_headers.clone()),
            })
            .with_client_type(Some("client type".to_string()));

        let computed_headers = client_builder.compute_http_headers();

        assert_eq!(
            computed_headers,
            HashMap::from([(
                MITHRIL_CLIENT_TYPE_HEADER.to_string(),
                "client type".to_string()
            )])
        );
    }
}
