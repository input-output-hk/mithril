use std::sync::Arc;

use anyhow::Context;
use config::Config;
use slog::Logger;

use mithril_common::{
    api_version::APIVersionProvider,
    certificate_chain::{CertificateVerifier, MithrilCertificateVerifier},
    digesters::{CardanoImmutableDigester, ImmutableDigester},
    StdResult,
};

use crate::{
    aggregator_client::{
        AggregatorClient, AggregatorHTTPClient, CertificateClient, MithrilStakeDistributionClient,
        SnapshotClient,
    },
    services::{
        AppMithrilStakeDistributionService, MithrilClientSnapshotService,
        MithrilStakeDistributionService, SnapshotService,
    },
};

/// Dependencies builder
pub struct DependenciesBuilder {
    /// Configuration
    pub config: Arc<Config>,

    /// HTTP Aggregator client
    pub aggregator_client: Option<Arc<dyn AggregatorClient>>,

    /// SnapshotClient
    pub snapshot_client: Option<Arc<SnapshotClient>>,

    /// MithrilStakeDistributionClient
    pub mithril_stake_distribution_client: Option<Arc<MithrilStakeDistributionClient>>,

    /// CertificateClient
    pub certificate_client: Option<Arc<CertificateClient>>,

    /// CertificateVerifier
    pub certificate_verifier: Option<Arc<dyn CertificateVerifier>>,

    /// ImmutableDigester
    pub immutable_digester: Option<Arc<dyn ImmutableDigester>>,

    /// [SnapshotService]
    pub snapshot_service: Option<Arc<dyn SnapshotService>>,

    /// [MithrilStakeDistributionService]
    pub mithril_stake_distribution_service: Option<Arc<dyn MithrilStakeDistributionService>>,
}

impl DependenciesBuilder {
    /// Constructor
    pub fn new(config: Arc<Config>) -> Self {
        Self {
            config,
            aggregator_client: None,
            snapshot_client: None,
            mithril_stake_distribution_client: None,
            certificate_client: None,
            certificate_verifier: None,
            immutable_digester: None,
            snapshot_service: None,
            mithril_stake_distribution_service: None,
        }
    }

    async fn build_logger(&mut self) -> StdResult<Logger> {
        Ok(slog_scope::logger())
    }

    /// Return an instance of the logger. Since the logger is a singleton it is
    /// provider directly by its own library.
    pub async fn get_logger(&mut self) -> StdResult<Logger> {
        self.build_logger().await
    }

    async fn build_aggregator_client(&mut self) -> StdResult<Arc<dyn AggregatorClient>> {
        let client = AggregatorHTTPClient::new(
            &self.config.get_string("aggregator_endpoint")?,
            APIVersionProvider::compute_all_versions_sorted()?,
        );

        Ok(Arc::new(client))
    }

    /// Get a clone of the [AggregatorClient] dependency
    pub async fn get_aggregator_client(&mut self) -> StdResult<Arc<dyn AggregatorClient>> {
        if self.aggregator_client.is_none() {
            self.aggregator_client = Some(self.build_aggregator_client().await?);
        }

        Ok(self.aggregator_client.as_ref().cloned().unwrap())
    }

    async fn build_snapshot_client(&mut self) -> StdResult<Arc<SnapshotClient>> {
        let client = SnapshotClient::new(self.get_aggregator_client().await?);

        Ok(Arc::new(client))
    }

    /// Get a clone of the [SnapshotClient] dependency
    pub async fn get_snapshot_client(&mut self) -> StdResult<Arc<SnapshotClient>> {
        if self.snapshot_client.is_none() {
            self.snapshot_client = Some(
                self.build_snapshot_client()
                    .await
                    .with_context(|| "Dependencies Builder can not build Snapshot Client")?,
            );
        }

        Ok(self.snapshot_client.as_ref().cloned().unwrap())
    }

    async fn build_mithril_stake_distribution_client(
        &mut self,
    ) -> StdResult<Arc<MithrilStakeDistributionClient>> {
        let client = MithrilStakeDistributionClient::new(self.get_aggregator_client().await?);

        Ok(Arc::new(client))
    }

    /// Get a clone of the [SnapshotClient] dependency
    pub async fn get_mithril_stake_distribution_client(
        &mut self,
    ) -> StdResult<Arc<MithrilStakeDistributionClient>> {
        if self.mithril_stake_distribution_client.is_none() {
            self.mithril_stake_distribution_client = Some(
                self.build_mithril_stake_distribution_client()
                    .await
                    .with_context(|| {
                        "Dependencies Builder can not build Mithril Stake distribution Client"
                    })?,
            );
        }

        Ok(self
            .mithril_stake_distribution_client
            .as_ref()
            .cloned()
            .unwrap())
    }

    async fn build_certificate_client(&mut self) -> StdResult<Arc<CertificateClient>> {
        let client = CertificateClient::new(self.get_aggregator_client().await?);

        Ok(Arc::new(client))
    }

    /// Get a clone of the [CertificateClient] dependency
    pub async fn get_certificate_client(&mut self) -> StdResult<Arc<CertificateClient>> {
        if self.certificate_client.is_none() {
            self.certificate_client = Some(
                self.build_certificate_client()
                    .await
                    .with_context(|| "Dependencies Builder can not build Certificate Client")?,
            );
        }

        Ok(self.certificate_client.as_ref().cloned().unwrap())
    }

    async fn build_certificate_verifier(&mut self) -> StdResult<Arc<dyn CertificateVerifier>> {
        let verifier = MithrilCertificateVerifier::new(self.get_logger().await?);

        Ok(Arc::new(verifier))
    }

    /// Get a clone of the [CertificateVerifier] dependency
    pub async fn get_certificate_verifier(&mut self) -> StdResult<Arc<dyn CertificateVerifier>> {
        if self.certificate_verifier.is_none() {
            self.certificate_verifier = Some(
                self.build_certificate_verifier()
                    .await
                    .with_context(|| "Dependencies Builder can not build Certificate Verifier")?,
            );
        }

        Ok(self.certificate_verifier.as_ref().cloned().unwrap())
    }

    async fn build_immutable_digester(&mut self) -> StdResult<Arc<dyn ImmutableDigester>> {
        let digester = CardanoImmutableDigester::new(None, self.get_logger().await?);

        Ok(Arc::new(digester))
    }

    /// Get a clone of the [ImmutableDigester] dependency
    pub async fn get_immutable_digester(&mut self) -> StdResult<Arc<dyn ImmutableDigester>> {
        if self.immutable_digester.is_none() {
            self.immutable_digester = Some(
                self.build_immutable_digester()
                    .await
                    .with_context(|| "Dependencies Builder can not build Immutable Digester")?,
            );
        }

        Ok(self.immutable_digester.as_ref().cloned().unwrap())
    }

    async fn build_snapshot_service(&mut self) -> StdResult<Arc<dyn SnapshotService>> {
        let snapshot_service = MithrilClientSnapshotService::new(
            self.get_snapshot_client().await?,
            self.get_certificate_client().await?,
            self.get_certificate_verifier().await?,
            self.get_immutable_digester().await?,
        );

        Ok(Arc::new(snapshot_service))
    }

    /// Get a clone of the [SnapshotService] dependency
    pub async fn get_snapshot_service(&mut self) -> StdResult<Arc<dyn SnapshotService>> {
        if self.snapshot_service.is_none() {
            self.snapshot_service = Some(
                self.build_snapshot_service()
                    .await
                    .with_context(|| "Dependencies Builder can not build Snapshot Service")?,
            );
        }

        Ok(self.snapshot_service.as_ref().cloned().unwrap())
    }

    async fn build_mithril_stake_distribution_service(
        &mut self,
    ) -> StdResult<Arc<dyn MithrilStakeDistributionService>> {
        let service = AppMithrilStakeDistributionService::new(
            self.get_mithril_stake_distribution_client().await?,
            self.get_certificate_client().await?,
            self.get_certificate_verifier().await?,
        );

        Ok(Arc::new(service))
    }

    /// Get a clone of the [MithrilStakeDistributionService] dependency
    pub async fn get_mithril_stake_distribution_service(
        &mut self,
    ) -> StdResult<Arc<dyn MithrilStakeDistributionService>> {
        if self.mithril_stake_distribution_service.is_none() {
            self.mithril_stake_distribution_service = Some(
                self.build_mithril_stake_distribution_service()
                    .await
                    .with_context(|| {
                        "Dependencies Builder can not build Mithril Stake Distribution Service"
                    })?,
            );
        }

        Ok(self
            .mithril_stake_distribution_service
            .as_ref()
            .cloned()
            .unwrap())
    }
}
