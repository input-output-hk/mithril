use semver::Version;
use std::sync::Arc;

use mithril_common::entities::CompressionAlgorithm;

use crate::artifact_builder::{
    AncillaryArtifactBuilder, AncillaryFileUploader, CardanoDatabaseArtifactBuilder,
    CardanoImmutableFilesFullArtifactBuilder, CardanoStakeDistributionArtifactBuilder,
    CardanoTransactionsArtifactBuilder, DigestArtifactBuilder, DigestFileUploader,
    ImmutableArtifactBuilder, ImmutableFilesUploader, MithrilStakeDistributionArtifactBuilder,
};
use crate::dependency_injection::builder::SNAPSHOT_ARTIFACTS_DIR;
use crate::dependency_injection::{DependenciesBuilder, DependenciesBuilderError, Result};
use crate::file_uploaders::{
    CloudRemotePath, FileUploadRetryPolicy, GcpBackendUploader, GcpUploader, LocalUploader,
};
use crate::http_server::CARDANO_DATABASE_DOWNLOAD_PATH;
use crate::services::{
    CompressedArchiveSnapshotter, DumbSnapshotter, MithrilSignedEntityService, SignedEntityService,
    SignedEntityServiceArtifactsDependencies, Snapshotter, SnapshotterCompressionAlgorithm,
};
use crate::{
    DumbUploader, ExecutionEnvironment, FileUploader, LocalSnapshotUploader, SnapshotUploaderType,
};

impl DependenciesBuilder {
    async fn build_signed_entity_service(&mut self) -> Result<Arc<dyn SignedEntityService>> {
        let logger = self.root_logger();
        let signed_entity_storer = self.get_signed_entity_storer().await?;
        let epoch_service = self.get_epoch_service().await?;
        let mithril_stake_distribution_artifact_builder = Arc::new(
            MithrilStakeDistributionArtifactBuilder::new(epoch_service.clone()),
        );
        let snapshotter = self.get_snapshotter().await?;
        let snapshot_uploader = self.get_snapshot_uploader().await?;
        let cardano_node_version = Version::parse(&self.configuration.cardano_node_version)
            .map_err(|e| DependenciesBuilderError::Initialization { message: format!("Could not parse configuration setting 'cardano_node_version' value '{}' as Semver.", self.configuration.cardano_node_version), error: Some(e.into()) })?;
        let cardano_immutable_files_full_artifact_builder =
            Arc::new(CardanoImmutableFilesFullArtifactBuilder::new(
                self.configuration.get_network()?,
                &cardano_node_version,
                snapshotter.clone(),
                snapshot_uploader,
                self.configuration.snapshot_compression_algorithm,
                logger.clone(),
            ));
        let prover_service = self.get_prover_service().await?;
        let cardano_transactions_artifact_builder = Arc::new(
            CardanoTransactionsArtifactBuilder::new(prover_service.clone()),
        );
        let stake_store = self.get_stake_store().await?;
        let cardano_stake_distribution_artifact_builder =
            Arc::new(CardanoStakeDistributionArtifactBuilder::new(stake_store));
        let cardano_database_artifact_builder = Arc::new(
            self.build_cardano_database_artifact_builder(cardano_node_version)
                .await?,
        );
        let dependencies = SignedEntityServiceArtifactsDependencies::new(
            mithril_stake_distribution_artifact_builder,
            cardano_immutable_files_full_artifact_builder,
            cardano_transactions_artifact_builder,
            cardano_stake_distribution_artifact_builder,
            cardano_database_artifact_builder,
        );
        let signed_entity_service = Arc::new(MithrilSignedEntityService::new(
            signed_entity_storer,
            dependencies,
            self.get_signed_entity_lock().await?,
            self.get_metrics_service().await?,
            logger,
        ));

        // Compute the cache pool for prover service
        // This is done here to avoid circular dependencies between the prover service and the signed entity service
        // TODO: Make this part of a warmup phase of the aggregator?
        if let Some(signed_entity) = signed_entity_service
            .get_last_cardano_transaction_snapshot()
            .await?
        {
            prover_service
                .compute_cache(signed_entity.artifact.block_number)
                .await?;
        }

        Ok(signed_entity_service)
    }

    /// [SignedEntityService] service
    pub async fn get_signed_entity_service(&mut self) -> Result<Arc<dyn SignedEntityService>> {
        if self.signed_entity_service.is_none() {
            self.signed_entity_service = Some(self.build_signed_entity_service().await?);
        }

        Ok(self.signed_entity_service.as_ref().cloned().unwrap())
    }

    async fn build_snapshotter(&mut self) -> Result<Arc<dyn Snapshotter>> {
        let snapshotter: Arc<dyn Snapshotter> = match self.configuration.environment {
            ExecutionEnvironment::Production => {
                let ongoing_snapshot_directory = self
                    .configuration
                    .get_snapshot_dir()?
                    .join("pending_snapshot");

                let algorithm = match self.configuration.snapshot_compression_algorithm {
                    CompressionAlgorithm::Gzip => SnapshotterCompressionAlgorithm::Gzip,
                    CompressionAlgorithm::Zstandard => self
                        .configuration
                        .zstandard_parameters
                        .unwrap_or_default()
                        .into(),
                };

                Arc::new(CompressedArchiveSnapshotter::new(
                    self.configuration.db_directory.clone(),
                    ongoing_snapshot_directory,
                    algorithm,
                    self.root_logger(),
                )?)
            }
            _ => Arc::new(DumbSnapshotter::new()),
        };

        Ok(snapshotter)
    }

    /// [Snapshotter] service.
    pub async fn get_snapshotter(&mut self) -> Result<Arc<dyn Snapshotter>> {
        if self.snapshotter.is_none() {
            self.snapshotter = Some(self.build_snapshotter().await?);
        }

        Ok(self.snapshotter.as_ref().cloned().unwrap())
    }

    async fn build_snapshot_uploader(&mut self) -> Result<Arc<dyn FileUploader>> {
        let logger = self.root_logger();
        if self.configuration.environment == ExecutionEnvironment::Production {
            match self.configuration.snapshot_uploader_type {
                SnapshotUploaderType::Gcp => {
                    let allow_overwrite = true;
                    let remote_folder_path = CloudRemotePath::new("cardano-immutable-files-full");

                    Ok(Arc::new(
                        self.build_gcp_uploader(remote_folder_path, allow_overwrite)?,
                    ))
                }
                SnapshotUploaderType::Local => {
                    let snapshot_artifacts_dir = self
                        .configuration
                        .get_snapshot_dir()?
                        .join(SNAPSHOT_ARTIFACTS_DIR);
                    std::fs::create_dir_all(&snapshot_artifacts_dir).map_err(|e| {
                        DependenciesBuilderError::Initialization {
                            message: format!(
                                "Cannot create '{snapshot_artifacts_dir:?}' directory."
                            ),
                            error: Some(e.into()),
                        }
                    })?;

                    Ok(Arc::new(LocalSnapshotUploader::new(
                        self.configuration.get_server_url()?,
                        &snapshot_artifacts_dir,
                        logger,
                    )))
                }
            }
        } else {
            Ok(Arc::new(DumbUploader::new(FileUploadRetryPolicy::never())))
        }
    }

    /// Get a [FileUploader]
    pub async fn get_snapshot_uploader(&mut self) -> Result<Arc<dyn FileUploader>> {
        if self.snapshot_uploader.is_none() {
            self.snapshot_uploader = Some(self.build_snapshot_uploader().await?);
        }

        Ok(self.snapshot_uploader.as_ref().cloned().unwrap())
    }

    fn build_gcp_uploader(
        &self,
        remote_folder_path: CloudRemotePath,
        allow_overwrite: bool,
    ) -> Result<GcpUploader> {
        let logger = self.root_logger();
        let bucket = self
            .configuration
            .snapshot_bucket_name
            .to_owned()
            .ok_or_else(|| {
                DependenciesBuilderError::MissingConfiguration("snapshot_bucket_name".to_string())
            })?;

        Ok(GcpUploader::new(
            Arc::new(GcpBackendUploader::try_new(
                bucket,
                self.configuration.snapshot_use_cdn_domain,
                logger.clone(),
            )?),
            remote_folder_path,
            allow_overwrite,
            FileUploadRetryPolicy::default(),
        ))
    }

    fn build_cardano_database_ancillary_uploaders(
        &self,
    ) -> Result<Vec<Arc<dyn AncillaryFileUploader>>> {
        let logger = self.root_logger();
        if self.configuration.environment == ExecutionEnvironment::Production {
            match self.configuration.snapshot_uploader_type {
                SnapshotUploaderType::Gcp => {
                    let allow_overwrite = true;
                    let remote_folder_path =
                        CloudRemotePath::new("cardano-database").join("ancillary");

                    Ok(vec![Arc::new(self.build_gcp_uploader(
                        remote_folder_path,
                        allow_overwrite,
                    )?)])
                }
                SnapshotUploaderType::Local => {
                    let server_url_prefix = self.configuration.get_server_url()?;
                    let ancillary_url_prefix = server_url_prefix
                        .sanitize_join(&format!("{CARDANO_DATABASE_DOWNLOAD_PATH}/ancillary/"))?;
                    let target_dir = self.get_cardano_db_artifacts_dir()?.join("ancillary");

                    std::fs::create_dir_all(&target_dir).map_err(|e| {
                        DependenciesBuilderError::Initialization {
                            message: format!("Cannot create '{target_dir:?}' directory."),
                            error: Some(e.into()),
                        }
                    })?;

                    Ok(vec![Arc::new(LocalUploader::new(
                        ancillary_url_prefix,
                        &target_dir,
                        FileUploadRetryPolicy::default(),
                        logger,
                    ))])
                }
            }
        } else {
            Ok(vec![Arc::new(DumbUploader::new(
                FileUploadRetryPolicy::never(),
            ))])
        }
    }

    fn build_cardano_database_immutable_uploaders(
        &self,
    ) -> Result<Vec<Arc<dyn ImmutableFilesUploader>>> {
        let logger = self.root_logger();
        if self.configuration.environment == ExecutionEnvironment::Production {
            match self.configuration.snapshot_uploader_type {
                SnapshotUploaderType::Gcp => {
                    let allow_overwrite = false;
                    let remote_folder_path =
                        CloudRemotePath::new("cardano-database").join("immutable");

                    Ok(vec![Arc::new(self.build_gcp_uploader(
                        remote_folder_path,
                        allow_overwrite,
                    )?)])
                }
                SnapshotUploaderType::Local => {
                    let server_url_prefix = self.configuration.get_server_url()?;
                    let immutable_url_prefix = server_url_prefix
                        .sanitize_join(&format!("{CARDANO_DATABASE_DOWNLOAD_PATH}/immutable/"))?;

                    Ok(vec![Arc::new(LocalUploader::new_without_copy(
                        immutable_url_prefix,
                        FileUploadRetryPolicy::default(),
                        logger,
                    ))])
                }
            }
        } else {
            Ok(vec![Arc::new(DumbUploader::new(
                FileUploadRetryPolicy::never(),
            ))])
        }
    }

    fn build_cardano_database_digests_uploaders(&self) -> Result<Vec<Arc<dyn DigestFileUploader>>> {
        let logger = self.root_logger();
        if self.configuration.environment == ExecutionEnvironment::Production {
            match self.configuration.snapshot_uploader_type {
                SnapshotUploaderType::Gcp => {
                    let allow_overwrite = false;
                    let remote_folder_path =
                        CloudRemotePath::new("cardano-database").join("digests");

                    Ok(vec![Arc::new(self.build_gcp_uploader(
                        remote_folder_path,
                        allow_overwrite,
                    )?)])
                }
                SnapshotUploaderType::Local => {
                    let server_url_prefix = self.configuration.get_server_url()?;
                    let digests_url_prefix = server_url_prefix
                        .sanitize_join(&format!("{CARDANO_DATABASE_DOWNLOAD_PATH}/digests/"))?;
                    let target_dir = self.get_cardano_db_artifacts_dir()?.join("digests");

                    std::fs::create_dir_all(&target_dir).map_err(|e| {
                        DependenciesBuilderError::Initialization {
                            message: format!("Cannot create '{target_dir:?}' directory."),
                            error: Some(e.into()),
                        }
                    })?;

                    Ok(vec![Arc::new(LocalUploader::new(
                        digests_url_prefix,
                        &target_dir,
                        FileUploadRetryPolicy::default(),
                        logger,
                    ))])
                }
            }
        } else {
            Ok(vec![Arc::new(DumbUploader::new(
                FileUploadRetryPolicy::never(),
            ))])
        }
    }

    async fn build_cardano_database_artifact_builder(
        &mut self,
        cardano_node_version: Version,
    ) -> Result<CardanoDatabaseArtifactBuilder> {
        let snapshot_dir = self.configuration.get_snapshot_dir()?;
        let immutable_dir = self.get_cardano_db_artifacts_dir()?.join("immutable");

        let ancillary_builder = Arc::new(AncillaryArtifactBuilder::new(
            self.build_cardano_database_ancillary_uploaders()?,
            self.get_snapshotter().await?,
            self.configuration.get_network()?,
            self.configuration.snapshot_compression_algorithm,
            self.root_logger(),
        )?);

        let immutable_builder = Arc::new(ImmutableArtifactBuilder::new(
            immutable_dir,
            self.build_cardano_database_immutable_uploaders()?,
            self.get_snapshotter().await?,
            self.configuration.snapshot_compression_algorithm,
            self.root_logger(),
        )?);

        let digest_builder = Arc::new(DigestArtifactBuilder::new(
            self.configuration.get_server_url()?,
            self.build_cardano_database_digests_uploaders()?,
            snapshot_dir.join("pending_cardano_database_digests"),
            self.get_immutable_file_digest_mapper().await?,
            self.root_logger(),
        )?);

        Ok(CardanoDatabaseArtifactBuilder::new(
            self.configuration.db_directory.clone(),
            &cardano_node_version,
            self.configuration.snapshot_compression_algorithm,
            ancillary_builder,
            immutable_builder,
            digest_builder,
        ))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::TempDir;
    use mithril_persistence::sqlite::ConnectionBuilder;

    use crate::dependency_injection::builder::CARDANO_DB_ARTIFACTS_DIR;
    use crate::Configuration;

    use super::*;

    #[tokio::test]
    async fn if_not_local_uploader_create_cardano_database_immutable_dirs() {
        let snapshot_directory = TempDir::create(
            "builder",
            "if_not_local_uploader_create_cardano_database_immutable_dirs",
        );
        let cdb_dir = snapshot_directory.join(CARDANO_DB_ARTIFACTS_DIR);
        let ancillary_dir = cdb_dir.join("ancillary");
        let immutable_dir = cdb_dir.join("immutable");
        let digests_dir = cdb_dir.join("digests");

        let mut dep_builder = {
            let config = Configuration {
                snapshot_directory,
                // Test environment yield dumb uploaders
                environment: ExecutionEnvironment::Test,
                ..Configuration::new_sample()
            };

            DependenciesBuilder::new_with_stdout_logger(config)
        };

        assert!(!ancillary_dir.exists());
        assert!(!immutable_dir.exists());
        assert!(!digests_dir.exists());

        dep_builder
            .build_cardano_database_artifact_builder(Version::parse("1.0.0").unwrap())
            .await
            .unwrap();

        assert!(!ancillary_dir.exists());
        assert!(immutable_dir.exists());
        assert!(!digests_dir.exists());
    }

    #[tokio::test]
    async fn if_local_uploader_creates_all_cardano_database_subdirs() {
        let snapshot_directory = TempDir::create(
            "builder",
            "if_local_uploader_creates_all_cardano_database_subdirs",
        );
        let cdb_dir = snapshot_directory.join(CARDANO_DB_ARTIFACTS_DIR);
        let ancillary_dir = cdb_dir.join("ancillary");
        let immutable_dir = cdb_dir.join("immutable");
        let digests_dir = cdb_dir.join("digests");

        let mut dep_builder = {
            let config = Configuration {
                snapshot_directory,
                // Must use production environment to make `snapshot_uploader_type` effective
                environment: ExecutionEnvironment::Production,
                snapshot_uploader_type: SnapshotUploaderType::Local,
                ..Configuration::new_sample()
            };

            DependenciesBuilder::new_with_stdout_logger(config)
        };
        // In production environment the builder can't create in-memory SQLite connections, we
        // need to provide it manually to avoid creations of unnecessary files.
        dep_builder.sqlite_connection =
            Some(Arc::new(ConnectionBuilder::open_memory().build().unwrap()));

        assert!(!ancillary_dir.exists());
        assert!(!immutable_dir.exists());
        assert!(!digests_dir.exists());

        dep_builder
            .build_cardano_database_artifact_builder(Version::parse("1.0.0").unwrap())
            .await
            .unwrap();

        assert!(ancillary_dir.exists());
        assert!(immutable_dir.exists());
        assert!(digests_dir.exists());
    }
}
