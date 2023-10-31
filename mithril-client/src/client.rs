use crate::aggregator_client::{AggregatorClient, AggregatorHTTPClient};
use crate::certificate_client::CertificateClient;
use crate::mithril_stake_distribution_client::MithrilStakeDistributionClient;
use crate::snapshot_client::SnapshotClient;
use crate::snapshot_downloader::{HttpSnapshotDownloader, SnapshotDownloader};
use crate::MithrilResult;
use anyhow::{anyhow, Context};
use mithril_common::api_version::APIVersionProvider;
use mithril_common::certificate_chain::CertificateVerifier;
use mithril_common::crypto_helper::ProtocolGenesisVerificationKey;
use reqwest::Url;
use std::sync::Arc;

pub struct Client {
    certificate_client: Arc<CertificateClient>,
    mithril_stake_distribution_client: Arc<MithrilStakeDistributionClient>,
    snapshot_client: Arc<SnapshotClient>,
}

impl Client {
    pub fn certificate(&self) -> Arc<CertificateClient> {
        self.certificate_client.clone()
    }

    pub fn mithril_stake_distribution(&self) -> Arc<MithrilStakeDistributionClient> {
        self.mithril_stake_distribution_client.clone()
    }

    pub fn snapshot(&self) -> Arc<SnapshotClient> {
        self.snapshot_client.clone()
    }
}

pub struct ClientBuilder {
    aggregator_url: Option<String>,
    genesis_verification_key: String,
    aggregator_client: Option<Arc<dyn AggregatorClient>>,
    certificate_verifier: Option<Arc<dyn CertificateVerifier>>,
    snapshot_downloader: Option<Arc<dyn SnapshotDownloader>>,
}

impl ClientBuilder {
    // note: easy alternative to `new().with_aggregator_client(..).with_certificate_verifier(..)`
    pub fn aggregator(url: &str, genesis_verification_key: &str) -> ClientBuilder {
        Self {
            aggregator_url: Some(url.to_string()),
            genesis_verification_key: genesis_verification_key.to_string(),
            aggregator_client: None,
            certificate_verifier: None,
            snapshot_downloader: None,
        }
    }

    pub fn build(self) -> MithrilResult<Client> {
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
                    )
                    .with_context(|| "Building aggregator client failed")?,
                )
            }
            Some(client) => client,
        };

        let snapshot_downloader = match self.snapshot_downloader {
            None => Arc::new(
                HttpSnapshotDownloader::new()
                    .with_context(|| "Building snapshot downloader failed")?,
            ),
            Some(snapshot_downloader) => snapshot_downloader,
        };

        let genesis_verification_key =
            ProtocolGenesisVerificationKey::try_from(self.genesis_verification_key)
                .with_context(|| "Invalid genesis verification key")?;

        let certificate_client = match self.certificate_verifier {
            None => {
                Arc::new(CertificateClient::new(
                    aggregator_client.clone(),
                    genesis_verification_key,
                    //todo: configurable log
                    slog_scope::logger(),
                ))
            }
            Some(verifier) => Arc::new(CertificateClient::new_with_verifier(
                aggregator_client.clone(),
                genesis_verification_key,
                verifier,
            )),
        };

        let mithril_stake_distribution_client = Arc::new(MithrilStakeDistributionClient::new(
            aggregator_client.clone(),
        ));
        let snapshot_client = Arc::new(SnapshotClient::new(aggregator_client, snapshot_downloader));

        Ok(Client {
            certificate_client,
            mithril_stake_distribution_client,
            snapshot_client,
        })
    }

    pub fn new(genesis_verification_key: &str) -> ClientBuilder {
        Self {
            aggregator_url: None,
            genesis_verification_key: genesis_verification_key.to_string(),
            aggregator_client: None,
            certificate_verifier: None,
            snapshot_downloader: None,
        }
    }

    pub fn with_aggregator_client(
        mut self,
        aggregator_client: Arc<dyn AggregatorClient>,
    ) -> ClientBuilder {
        self.aggregator_client = Some(aggregator_client);
        self
    }

    pub fn with_certificate_verifier(
        mut self,
        certificate_verifier: Arc<dyn CertificateVerifier>,
    ) -> ClientBuilder {
        self.certificate_verifier = Some(certificate_verifier);
        self
    }

    pub fn with_snapshot_downloader(
        mut self,
        snapshot_downloader: Arc<dyn SnapshotDownloader>,
    ) -> ClientBuilder {
        self.snapshot_downloader = Some(snapshot_downloader);
        self
    }
}
